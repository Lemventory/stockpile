module UI.Transaction.TransactionHistory where

import API.Transaction
import Prelude

import Data.Array (null, sortBy, (!!), (:))
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith)
import Data.String as String
import Data.Time (diff)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Types.Transaction (Transaction(..), TransactionItem(..), PaymentTransaction(..), TransactionStatus(..), TransactionType(..))
import Types.UUID (UUID(..))
import UI.Common.Form as F
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select
import Web.HTML.HTMLTextAreaElement as TextArea

transactionHistory :: Nut
transactionHistory = Deku.do
  -- State
  setTransactions /\ transactionsValue <- useState []
  setSelectedTransaction /\ selectedTransactionValue <- useState Nothing
  setIsLoading /\ isLoadingValue <- useState false
  setStatusMessage /\ statusMessageValue <- useState ""  
  setSearchText /\ searchTextValue <- useState ""
  
  -- Component loading effect
  D.div
    [ DA.klass_ "transaction-history-container container mx-auto p-4"
    , DL.load_ \_ -> do
        liftEffect $ Console.log "Transaction History component loading"
        loadTransactions
    ]
    [ D.h2
        [ DA.klass_ "text-2xl font-bold mb-6" ]
        [ text_ "Transaction History" ]
      
      -- Search/filter controls
      , D.div
          [ DA.klass_ "controls mb-6 p-4 border rounded" ]
          [ D.div
              [ DA.klass_ "grid grid-cols-3 gap-4" ]
              [ D.div_
                  [ D.label
                      [ DA.klass_ "block mb-2" ]
                      [ text_ "Search" ]
                  , D.input
                      [ DA.klass_ "form-input w-full"
                      , DA.placeholder_ "Search by ID..."
                      , DA.value_ ""
                      , DL.input_ \evt -> do
                          for_ (target evt >>= Input.fromEventTarget) \el -> do
                            value <- Input.value el
                            setSearchText value
                      ]
                      []
                  ]
              , D.div_
                  [ D.label
                      [ DA.klass_ "block mb-2" ]
                      [ text_ "Refresh" ]
                  , D.button
                      [ DA.klass_ (F.buttonClass "blue")
                      , DL.click_ \_ -> loadTransactions
                      ]
                      [ text_ "Refresh Transactions" ]
                  ]
              ]
          ]
          
      -- Transactions list
      , D.div
          [ DA.klass_ "transactions-list mb-8" ]
          [ D.h3
              [ DA.klass_ "text-lg font-semibold mb-2" ]
              [ text_ "Transactions" ]
          , isLoadingValue <#~> \isLoading ->
              if isLoading then
                D.div [ DA.klass_ "text-center p-4" ] [ text_ "Loading transactions..." ]
              else
                transactionsValue <#~> \transactions ->
                  let
                    filteredTx = 
                      if searchTextValue == "" 
                      then transactions 
                      else filter 
                        (\tx -> contains (toLowerCase (show (transactionId tx))) 
                                        (toLowerCase searchTextValue)) 
                        transactions
                  in
                    if null filteredTx then
                      D.div [ DA.klass_ "text-gray-500 text-center p-4" ] [ text_ "No transactions found" ]
                    else
                      D.table
                        [ DA.klass_ "w-full border" ]
                        [ D.thead_
                            [ D.tr [ DA.klass_ "bg-gray-100" ]
                                [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "ID" ]
                                , D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Date" ]
                                , D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Type" ]
                                , D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Status" ]
                                , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Items" ]
                                , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Total" ]
                                , D.th [ DA.klass_ "p-2" ] [ text_ "Actions" ]
                                ]
                            ]
                        , D.tbody_
                            (filteredTx <#> \(Transaction tx) ->
                              D.tr 
                                [ DA.klass_ "border-t hover:bg-gray-50 cursor-pointer"
                                , DL.click_ \_ -> do
                                    setSelectedTransaction (Just (Transaction tx))
                                ]
                                [ D.td [ DA.klass_ "p-2" ] [ text_ (show tx.id) ]
                                , D.td [ DA.klass_ "p-2" ] [ text_ (formatDateTime tx.created) ]
                                , D.td [ DA.klass_ "p-2" ] [ text_ (show tx.transactionType) ]
                                , D.td [ DA.klass_ "p-2" ] 
                                    [ D.span
                                        [ DA.klass_ (statusClass tx.status) ]
                                        [ text_ (show tx.status) ]
                                    ]
                                , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (show (length tx.items)) ]
                                , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney tx.total) ]
                                , D.td [ DA.klass_ "p-2 text-center" ]
                                    [ D.button
                                        [ DA.klass_ "text-blue-600 hover:text-blue-800 mr-2"
                                        , DL.click_ \evt -> do
                                            -- Prevent row click event from firing
                                            stopPropagation evt
                                            loadTransactionDetails tx.id
                                        ]
                                        [ text_ "View" ]
                                    ]
                                ]
                            )
                        ]
          ]
          
      -- Transaction details
      , selectedTransactionValue <#~> \maybeTransaction ->
          case maybeTransaction of
            Nothing -> D.div_ []
            Just (Transaction tx) ->
              D.div
                [ DA.klass_ "transaction-details p-4 border rounded bg-gray-50" ]
                [ D.div
                    [ DA.klass_ "flex justify-between mb-4" ]
                    [ D.h3
                        [ DA.klass_ "text-lg font-semibold" ]
                        [ text_ "Transaction Details" ]
                    , D.button
                        [ DA.klass_ "text-gray-600 hover:text-gray-800"
                        , DL.click_ \_ -> setSelectedTransaction Nothing
                        ]
                        [ text_ "Close" ]
                    ]
                , D.div
                    [ DA.klass_ "grid grid-cols-2 gap-2 mb-6" ]
                    [ D.div [ DA.klass_ "font-semibold" ] [ text_ "Transaction ID:" ]
                    , D.div_ [ text_ (show tx.id) ]
                    , D.div [ DA.klass_ "font-semibold" ] [ text_ "Date:" ]
                    , D.div_ [ text_ (formatDateTime tx.created) ]
                    , D.div [ DA.klass_ "font-semibold" ] [ text_ "Type:" ]
                    , D.div_ [ text_ (show tx.transactionType) ]
                    , D.div [ DA.klass_ "font-semibold" ] [ text_ "Status:" ]
                    , D.div_ 
                        [ D.span
                            [ DA.klass_ (statusClass tx.status) ]
                            [ text_ (show tx.status) ]
                        ]
                    , D.div [ DA.klass_ "font-semibold" ] [ text_ "Employee ID:" ]
                    , D.div_ [ text_ (show tx.employeeId) ]
                    , D.div [ DA.klass_ "font-semibold" ] [ text_ "Register ID:" ]
                    , D.div_ [ text_ (show tx.registerId) ]
                    ]
                  
                -- Items section
                , D.h4
                    [ DA.klass_ "font-semibold text-md mb-2" ]
                    [ text_ "Items" ]
                , D.table
                    [ DA.klass_ "w-full border mb-6" ]
                    [ D.thead_
                        [ D.tr [ DA.klass_ "bg-gray-100" ]
                            [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Item ID" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Quantity" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Price" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Subtotal" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Tax" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Total" ]
                            ]
                        ]
                    , D.tbody_
                        (tx.items <#> \(TransactionItem item) ->
                          let 
                            taxTotal = foldl (\acc tax -> acc + tax.amount) (Discrete 0) item.taxes
                          in
                            D.tr [ DA.klass_ "border-t" ]
                              [ D.td [ DA.klass_ "p-2" ] [ text_ (show item.menuItemSku) ]
                              , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (show item.quantity) ]
                              , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney item.pricePerUnit) ]
                              , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney item.subtotal) ]
                              , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney taxTotal) ]
                              , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney item.total) ]
                              ]
                        )
                    ]
                    
                -- Payments section
                , D.h4
                    [ DA.klass_ "font-semibold text-md mb-2" ]
                    [ text_ "Payments" ]
                , D.table
                    [ DA.klass_ "w-full border mb-6" ]
                    [ D.thead_
                        [ D.tr [ DA.klass_ "bg-gray-100" ]
                            [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Method" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Amount" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Tendered" ]
                            , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Change" ]
                            ]
                        ]
                    , D.tbody_
                        (tx.payments <#> \(PaymentTransaction payment) ->
                          D.tr [ DA.klass_ "border-t" ]
                            [ D.td [ DA.klass_ "p-2" ] [ text_ (show payment.method) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney payment.amount) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney payment.tendered) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney payment.change) ]
                            ]
                        )
                    ]
                    
                -- Summary section
                , D.div
                    [ DA.klass_ "transaction-summary p-4 border rounded" ]
                    [ D.div
                        [ DA.klass_ "grid grid-cols-2 gap-2" ]
                        [ D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Subtotal:" ]
                        , D.div [ DA.klass_ "text-right" ] [ text_ (formatMoney tx.subtotal) ]
                        , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Discount:" ]
                        , D.div [ DA.klass_ "text-right" ] [ text_ (formatMoney tx.discountTotal) ]
                        , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Tax:" ]
                        , D.div [ DA.klass_ "text-right" ] [ text_ (formatMoney tx.taxTotal) ]
                        , D.div [ DA.klass_ "text-right font-semibold text-lg" ] [ text_ "Total:" ]
                        , D.div [ DA.klass_ "text-right text-lg" ] [ text_ (formatMoney tx.total) ]
                        ]
                    ]
                ]
          
      -- Status message
      , D.div
          [ DA.klass_ "status-message mt-4 p-2 text-center" ]
          [ statusMessageValue <#~> \msg ->
              if msg == "" then
                D.span_ []
              else if contains "Error" msg then
                D.div [ DA.klass_ "bg-red-100 text-red-800 p-2 rounded" ] [ text_ msg ]
              else
                D.div [ DA.klass_ "bg-green-100 text-green-800 p-2 rounded" ] [ text_ msg ]
          ]
    ]
  where
    loadTransactions = do
      liftEffect $ Console.log "Transaction History component loading"
      void $ launchAff do
        result <- getAllTransactions

        liftEffect case result of
          Right txns -> do
            let sortedTxns = sortBy
                  (\(Transaction a) (Transaction b) ->
                    compare b.created a.created)
                  txns
            
            -- This is where setTransactions is used
            setTransactions sortedTxns
            setStatusMessage ""
          Left err -> do
            Console.error $ "Failed to load transactions: " <> err
            setStatusMessage $ "Error: " <> err
      liftEffect $ setIsLoading false
  
    loadTransactionDetails txId = do
      setIsLoading true
      setStatusMessage "Loading transaction details..."
      
      void $ launchAff do
        result <- getTransaction txId
        
        liftEffect case result of
          Right tx -> do
            setSelectedTransaction (Just tx)
            setStatusMessage ""
          Left err -> do
            Console.error $ "Failed to load transaction details: " <> err
            setStatusMessage $ "Error: " <> err
        
        liftEffect $ setIsLoading false

-- Helper functions
formatMoney :: Discrete USD -> String
formatMoney (Discrete cents) =
  let
    dollars = Int.toNumber cents / 100.0
    formatted = if Int.toNumber (Int.floor (dollars * 100.0)) / 100.0 == dollars
                then show (Int.floor dollars) <> "." <> padStart 2 (show (Int.floor ((dollars - Int.floor dollars) * 100.0)))
                else show dollars
  in
    "$" <> formatted

formatDateTime :: DateTime -> String
formatDateTime dt = toLocaleString dt

statusClass :: TransactionStatus -> String
statusClass status = case status of
  Created -> "text-blue-600 bg-blue-100 px-2 py-1 rounded"
  InProgress -> "text-yellow-600 bg-yellow-100 px-2 py-1 rounded"
  Completed -> "text-green-600 bg-green-100 px-2 py-1 rounded"
  Voided -> "text-red-600 bg-red-100 px-2 py-1 rounded"
  Refunded -> "text-purple-600 bg-purple-100 px-2 py-1 rounded"

contains :: String -> String -> Boolean
contains str substr = 
  indexOf (Pattern substr) str /= -1

toLowerCase :: String -> String
toLowerCase = String.toLower

padStart :: Int -> String -> String
padStart targetLength str =
  let
    paddingLength = max 0 (targetLength - String.length str)
    padding = replicate paddingLength "0"
  in
    joinWith "" padding <> str

find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find predicate arr = 
  case uncons arr of
    Nothing -> Nothing
    Just { head, tail } -> 
      if predicate head 
        then Just head 
        else find predicate tail
  where
    uncons :: Array a -> Maybe { head :: a, tail :: Array a }
    uncons [] = Nothing
    uncons arr = Just { head: unsafePartial (arr !! 0), tail: drop 1 arr }

toLocaleString :: DateTime -> String
toLocaleString dt = show dt  -- Simple implementation, format as needed

foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b
foldl f init arr = unsafeFoldl f init arr
  where
    unsafeFoldl _ acc [] = acc
    unsafeFoldl f acc xs = unsafeFoldl f (f acc (unsafePartial (xs !! 0))) (drop 1 xs)

filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter pred = foldl (\acc x -> if pred x then snoc acc x else acc) []

length :: forall a. Array a -> Int
length arr = foldl (\acc _ -> acc + 1) 0 arr

indexOf :: Pattern -> String -> Int
indexOf (Pattern substr) str = indexOf' 0
  where 
    indexOf' start =
      if start >= String.length str
        then -1
        else if String.take (String.length substr) (String.drop start str) == substr
          then start
          else indexOf' (start + 1)

replicate :: Int -> String -> Array String
replicate n str = 
  if n <= 0 
    then []
    else str : replicate (n - 1) str

snoc :: forall a. Array a -> a -> Array a
snoc xs x = xs <> [x]

drop :: forall a. Int -> Array a -> Array a
drop n xs = 
  if n <= 0 || length xs == 0
    then xs
    else drop (n - 1) (unsafeTail xs)
  where
    unsafeTail :: Array a -> Array a
    unsafeTail arr = unsafePartial $ fromJust $ tail arr
    
    tail :: Array a -> Maybe (Array a)
    tail [] = Nothing
    tail xs = Just (unsafePartial $ slice 1 (length xs) xs)
    
    slice :: Int -> Int -> Array a -> Array a
    slice start end xs = 
      let go i acc = 
            if i >= end
              then acc
              else if i >= start
                then go (i + 1) (snoc acc (unsafePartial $ xs !! i))
                else go (i + 1) acc
      in go 0 []

    fromJust :: forall a. Maybe a -> a
    fromJust (Just x) = x
    fromJust Nothing = unsafeCrashWith "fromJust: Nothing"

transactionId :: Transaction -> UUID
transactionId (Transaction tx) = tx.id

