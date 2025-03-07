module UI.Transaction.TransactionHistory where

import API.Transaction
import Prelude

import Data.Array (drop, filter, find, foldl, length, null, replicate, snoc, sortBy, (!!), (:))
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, toLower)
import Data.String as String
import Data.String.CodeUnits (contains, indexOf)
import Data.String.Utils (padStart)
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
import Utils.Money (formatMoney)
import Web.Event.Event (Event, target)
import Web.Event.Event (stopPropagation)
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select
import Web.HTML.HTMLTextAreaElement as TextArea

transactionHistory :: Nut
transactionHistory = Deku.do
  setTransactions /\ transactionsValue <- useState []
  setSelectedTransaction /\ selectedTransactionValue <- useState Nothing
  setIsLoading /\ isLoadingValue <- useState false
  setStatusMessage /\ statusMessageValue <- useState ""  
  setSearchText /\ searchTextValue <- useState ""
  
  let
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
            liftEffect $ Console.log $ "Setting " <> show (length sortedTxns) <> " transactions"
            liftEffect $ setTransactions sortedTxns  -- Make sure setTransactions is properly lifted
            liftEffect $ setStatusMessage ""
          Left err -> do
            liftEffect $ Console.error $ "Failed to load transactions: " <> err
            liftEffect $ setStatusMessage $ "Error: " <> err
        liftEffect $ setIsLoading false

    loadTransactionDetails txId = do
      liftEffect $ setIsLoading true
      liftEffect $ setStatusMessage "Loading transaction details..."
      
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
              D.div_ [
                transactionsValue <#~> \transactions ->
                  searchTextValue <#~> \text ->
                    let
                      filteredTx = if text == ""
                        then transactions
                        else filter
                          (\tx -> contains (Pattern (toLower text)) (toLower (show (transactionId tx))))
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
                                              stopPropagation evt
                                              loadTransactionDetails tx.id
                                          ]
                                          [ text_ "View" ]
                                      ]
                                  ]
                              )
                          ]
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

-- Helper functions


formatDateTime :: DateTime -> String
formatDateTime dt = toLocaleString dt

statusClass :: TransactionStatus -> String
statusClass status = case status of
  Created -> "text-blue-600 bg-blue-100 px-2 py-1 rounded"
  InProgress -> "text-yellow-600 bg-yellow-100 px-2 py-1 rounded"
  Completed -> "text-green-600 bg-green-100 px-2 py-1 rounded"
  Voided -> "text-red-600 bg-red-100 px-2 py-1 rounded"
  Refunded -> "text-purple-600 bg-purple-100 px-2 py-1 rounded"

-- toLowerCase :: String -> String
-- toLowerCase = String.toLower

toLocaleString :: DateTime -> String
toLocaleString dt = show dt  -- Simple implementation, format as needed

transactionId :: Transaction -> UUID
transactionId (Transaction tx) = tx.id

