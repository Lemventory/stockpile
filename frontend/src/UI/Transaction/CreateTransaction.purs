module UI.Transaction.CreateTransaction where

import Prelude

import API.Inventory (readInventory)
import API.Transaction (createTransaction) as API
import Data.Array (filter, find, foldl, length, null, replicate, (:))
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Finance.Money.Extended (fromDiscrete', toDiscrete)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..), joinWith, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Types.Inventory (Inventory(..), InventoryResponse(..), MenuItem(..))
import Types.Transaction (PaymentMethod(..), PaymentTransaction(..), TaxCategory(..), Transaction(..), TransactionItem(..), TransactionStatus(..), TransactionType(..))
import Types.UUID (parseUUID)
import UI.Common.Form as F
import Utils.Money (formatMoney', fromDollars, toDollars)
import Utils.UUIDGen (genUUID)
import Web.Event.Event (target)
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select

createTransaction :: Nut
createTransaction = Deku.do

  setItems /\ itemsValue <- useState []
  setPayments /\ paymentsValue <- useState []
  setCustomerId /\ customerIdValue <- useState Nothing
  setEmployee /\ employeeValue <- useState ""
  setRegisterId /\ registerIdValue <- useState ""
  setLocationId /\ locationIdValue <- useState ""
  setSubtotal /\ subtotalValue <- useState (Discrete 0)
  setDiscountTotal /\ discountTotalValue <- useState (Discrete 0)
  setTaxTotal /\ taxTotalValue <- useState (Discrete 0)
  setTotal /\ totalValue <- useState (Discrete 0)
  setStatus /\ statusValue <- useState Created
  setTransactionType /\ transactionTypeValue <- useState Sale

  setInventory /\ inventoryValue <- useState []
  setSearchText /\ searchTextValue <- useState ""
  setSelectedItem /\ selectedItemValue <- useState Nothing
  setItemQuantity /\ itemQuantityValue <- useState "1"
  setPaymentMethod /\ paymentMethodValue <- useState Cash
  setPaymentAmount /\ paymentAmountValue <- useState ""
  setTenderedAmount /\ tenderedAmountValue <- useState ""
  setStatusMessage /\ statusMessageValue <- useState ""
  setIsProcessing /\ isProcessingValue <- useState false

  D.div
    [ DA.klass_ "transaction-container container mx-auto p-4"
    , DL.load_ \_ -> do
        liftEffect $ Console.log "Transaction component loading"

        void $ launchAff do
          employeeId <- liftEffect genUUID
          registerId <- liftEffect genUUID
          locationId <- liftEffect genUUID

          liftEffect do
            setEmployee (show employeeId)
            setRegisterId (show registerId)
            setLocationId (show locationId)

        void $ launchAff do
          result <- readInventory
          liftEffect case result of
            Right (InventoryData (Inventory items)) -> do
              Console.log $ "Loaded " <> show (length items) <>
                " inventory items"
              setInventory items
            Right (Message msg) -> do
              Console.error $ "API Message: " <> msg
              setStatusMessage $ "Error: " <> msg
            Left err -> do
              Console.error $ "Failed to load inventory: " <> err
              setStatusMessage $ "Error loading inventory: " <> err
    ]
    [ D.h2
        [ DA.klass_ "text-2xl font-bold mb-6" ]
        [ text_ "New Transaction" ]

    , D.div
        [ DA.klass_ "inventory-search mb-8 p-4 border rounded" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2" ]
            [ text_ "Add Items" ]
        , D.div
            [ DA.klass_ "flex mb-4" ]
            [ D.input
                [ DA.klass_ "form-input flex-grow mr-2"
                , DA.placeholder_ "Search inventory..."
                , DA.value_ ""
                , DL.input_ \evt -> do
                    for_ (Event.target evt >>= Input.fromEventTarget) \el -> do
                      value <- Input.value el
                      setSearchText value
                ]
                []
            ]
        , D.div
            [ DA.klass_ "inventory-results" ]
            [ inventoryValue <#~> \items ->
                let
                  filteredItems =
                    searchTextValue <#~> \text ->
                      if text == "" then D.div_ []
                      else D.div_
                        ( filter
                            ( \(MenuItem i) ->
                                contains (toLowerCase i.name) (toLowerCase text)
                                  ||
                                    contains (toLowerCase i.brand)
                                      (toLowerCase text)
                            )
                            items
                            <#> \item@(MenuItem i) ->
                              D.div
                                [ DA.klass_
                                    "inventory-item p-2 border rounded cursor-pointer hover:bg-gray-100"
                                , DL.click_ \_ -> do
                                    setSelectedItem (Just item)
                                    setItemQuantity "1"
                                ]
                                [ D.div [ DA.klass_ "font-semibold" ]
                                    [ text_ i.name ]
                                , D.div [ DA.klass_ "text-sm" ]
                                    [ text_ ("$" <> show i.price) ]
                                ]
                        )
                in
                  if null items then D.div [ DA.klass_ "text-gray-500" ]
                    [ text_ "No items found" ]
                  else D.div [ DA.klass_ "grid grid-cols-3 gap-2" ]
                    ( map
                        ( \item@(MenuItem i) ->
                            D.div
                              [ DA.klass_
                                  "inventory-item p-2 border rounded cursor-pointer hover:bg-gray-100"
                              , DL.click_ \_ -> do
                                  setSelectedItem (Just item)
                                  setItemQuantity "1"
                              ]
                              [ D.div [ DA.klass_ "font-semibold" ]
                                  [ text_ i.name ]
                              , D.div [ DA.klass_ "text-sm" ]
                                  [ text_ ("$" <> show i.price) ]
                              ]
                        )
                        items
                    )
            ]
        ]

    , D.div
        [ DA.klass_ "selected-item mb-8" ]
        [ selectedItemValue <#~> \maybeItem ->
            case maybeItem of
              Nothing -> D.div_ []
              Just (MenuItem item) ->
                D.div
                  [ DA.klass_ "p-4 border rounded" ]
                  [ D.h3
                      [ DA.klass_ "text-lg font-semibold mb-2" ]
                      [ text_ "Selected Item" ]
                  , D.div [ DA.klass_ "font-semibold" ] [ text_ item.name ]
                  , D.div [ DA.klass_ "text-sm mb-4" ]
                      [ text_ ("$" <> show item.price) ]
                  , D.div
                      [ DA.klass_ "flex items-center" ]
                      [ D.label
                          [ DA.klass_ "mr-2" ]
                          [ text_ "Quantity:" ]
                      , D.input
                          [ DA.klass_ "form-input w-20 mr-4"
                          , DA.xtype_ "number"
                          , DA.min_ "1"
                          , DA.value_ "1"
                          , DL.input_ \evt -> do
                              for_ (target evt >>= Input.fromEventTarget) \el ->
                                do
                                  value <- Input.value el
                                  setItemQuantity value
                          ]
                          []
                      , D.button
                          [ DA.klass_ (F.buttonClass "green")
                          , runOn DL.click $
                              map
                                ( \args@
                                     { qty
                                     , maybeSelectedItem
                                     , currSubtotal
                                     , currTaxTotal
                                     , currTotal
                                     , currItems
                                     } ->
                                    if (maybeSelectedItem == Nothing) then
                                      setStatusMessage "No item selected"
                                    else case readFloat qty of
                                      Nothing ->
                                        setStatusMessage "Invalid quantity"
                                      Just qtyNum ->
                                        case maybeSelectedItem of
                                          Just menuItem ->
                                            processValidItem qtyNum
                                              menuItem
                                              currSubtotal
                                              currTaxTotal
                                              currTotal
                                              currItems
                                              setSubtotal
                                              setTaxTotal
                                              setTotal
                                              setItems
                                              setSelectedItem
                                              setStatusMessage
                                          Nothing ->
                                            -- This shouldn't happen due to the earlier check
                                            setStatusMessage "No item selected"
                                )
                                ( { qty: _
                                  , maybeSelectedItem: _
                                  , currSubtotal: _
                                  , currTaxTotal: _
                                  , currTotal: _
                                  , currItems: _
                                  }
                                    <$> itemQuantityValue
                                    <*> selectedItemValue
                                    <*> subtotalValue
                                    <*> taxTotalValue
                                    <*> totalValue
                                    <*> itemsValue
                                )
                          ]
                          [ text_ "Add to Transaction" ]
                      ]
                  ]
        ]

    , D.div
        [ DA.klass_ "transaction-items mb-8" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2" ]
            [ text_ "Transaction Items" ]
        , itemsValue <#~> \items ->
            if null items then
              D.div [ DA.klass_ "text-gray-500" ] [ text_ "No items added yet" ]
            else
              D.table
                [ DA.klass_ "w-full border" ]
                [ D.thead_
                    [ D.tr [ DA.klass_ "bg-gray-100" ]
                        [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Item" ]
                        , D.th [ DA.klass_ "p-2 text-right" ]
                            [ text_ "Quantity" ]
                        , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Price" ]
                        , D.th [ DA.klass_ "p-2 text-right" ]
                            [ text_ "Subtotal" ]
                        , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Tax" ]
                        , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Total" ]
                        , D.th [ DA.klass_ "p-2" ] [ text_ "Actions" ]
                        ]
                    ]
                , D.tbody_
                    ( items <#> \(TransactionItem item) ->
                        let
                          taxTotal = foldl (\acc tax -> acc + toDiscrete tax.amount)
                            (Discrete 0)
                            item.taxes
                          taxTotalMoney = fromDiscrete' taxTotal
                        in
                          D.tr [ DA.klass_ "border-t" ]
                            [ D.td [ DA.klass_ "p-2" ]
                                [ inventoryValue <#~> \invItems ->
                                    let
                                      itemInfo = find
                                        ( \(MenuItem i) -> i.sku ==
                                            item.menuItemSku
                                        )
                                        invItems
                                    in
                                      case itemInfo of
                                        Just (MenuItem i) -> text_ i.name
                                        Nothing -> text_ "Unknown Item"
                                ]
                            , D.td [ DA.klass_ "p-2 text-right" ]
                                [ text_ (show item.quantity) ]
                            , D.td [ DA.klass_ "p-2 text-right" ]
                                [ text_ (formatMoney' item.pricePerUnit) ]
                            , D.td [ DA.klass_ "p-2 text-right" ]
                                [ text_ (formatMoney' item.subtotal) ]
                            , D.td [ DA.klass_ "p-2 text-right" ]
                                [ text_ (formatMoney' taxTotalMoney) ]
                            , D.td [ DA.klass_ "p-2 text-right" ]
                                [ text_ (formatMoney' item.total) ]
                            , D.td [ DA.klass_ "p-2 text-center" ]
                                [ D.button
                                  [ DA.klass_ "text-red-600 hover:text-red-800"
                                  , runOn DL.click $ 
                                      (\currItems currSubtotal currTaxTotal currTotal ->
                                        do
                                          let
                                            updatedItems = filter
                                              ( \(TransactionItem i) -> i.id /= item.id )
                                              currItems

                                          setSubtotal (currSubtotal - toDiscrete item.subtotal)
                                          setTaxTotal (currTaxTotal - taxTotal)
                                          setTotal (currTotal - toDiscrete item.total)
                                          setItems updatedItems
                                          setStatusMessage "Item removed from transaction"
                                      ) <$> itemsValue <*> subtotalValue <*> taxTotalValue <*> totalValue
                                  ]
                                  [ text_ "Remove" ]
                                ]
                            ]
                    )
                ]
        ]

    , D.div
        [ DA.klass_ "payment-section mb-8 p-4 border rounded" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2" ]
            [ text_ "Payment" ]
        , D.div
            [ DA.klass_ "grid grid-cols-2 gap-4" ]
            [ D.div_
                [ D.label
                    [ DA.klass_ "block mb-2" ]
                    [ text_ "Payment Method" ]
                , D.select
                    [ DA.klass_ "form-select w-full"
                    , DL.change_ \evt -> do
                        for_ (target evt >>= Select.fromEventTarget) \el -> do
                          value <- Select.value el
                          case value of
                            "Cash" -> setPaymentMethod Cash
                            "Credit" -> setPaymentMethod Credit
                            "Debit" -> setPaymentMethod Debit
                            "ACH" -> setPaymentMethod ACH
                            "GiftCard" -> setPaymentMethod GiftCard
                            _ -> setPaymentMethod Cash
                    ]
                    [ D.option [ DA.value_ "Cash" ] [ text_ "Cash" ]
                    , D.option [ DA.value_ "Credit" ] [ text_ "Credit Card" ]
                    , D.option [ DA.value_ "Debit" ] [ text_ "Debit Card" ]
                    , D.option [ DA.value_ "ACH" ] [ text_ "ACH Transfer" ]
                    , D.option [ DA.value_ "GiftCard" ] [ text_ "Gift Card" ]
                    ]
                ]
            , D.div_
                [ D.label
                    [ DA.klass_ "block mb-2" ]
                    [ text_ "Amount" ]
                , D.input
                    [ DA.klass_ "form-input w-full"
                    , DA.xtype_ "text"
                    , DA.placeholder_ "Amount"
                    , DL.input_ \evt -> do
                        for_ (target evt >>= Input.fromEventTarget) \el -> do
                          value <- Input.value el
                          setPaymentAmount value
                    ]
                    []
                ]
            ]
        , paymentMethodValue <#~> \method ->
            if method == Cash then
              D.div
                [ DA.klass_ "mt-4" ]
                [ D.label
                    [ DA.klass_ "block mb-2" ]
                    [ text_ "Tendered Amount" ]
                , D.input
                    [ DA.klass_ "form-input w-full"
                    , DA.xtype_ "text"
                    , DA.placeholder_ "Tendered Amount"
                    , DL.input_ \evt -> do
                        for_ (target evt >>= Input.fromEventTarget) \el -> do
                          value <- Input.value el
                          setTenderedAmount value
                    ]
                    []
                ]
            else
              D.div_ []
        , D.button
          [ DA.klass_ (F.buttonClass "blue" <> " mt-4")
          , runOn DL.click $ 
              (\payAmt tenderedAmt method currPayments ->
                do
                  case (readFloat payAmt) of
                    Nothing -> do
                      setStatusMessage "Invalid payment amount"
                    Just amount -> do
                      let
                        tenderedAmount = case readFloat tenderedAmt of
                          Just t -> t
                          Nothing -> amount

                        paymentAmount = fromDiscrete'
                          (Discrete (Int.floor (amount * 100.0)))
                        paymentTendered = fromDiscrete'
                          (Discrete (Int.floor (tenderedAmount * 100.0)))
                        change =
                          if
                            toDiscrete paymentTendered > toDiscrete
                              paymentAmount then
                            fromDiscrete'
                              ( toDiscrete paymentTendered - toDiscrete
                                  paymentAmount
                              )
                          else fromDiscrete' (Discrete 0)

                      void $ launchAff do
                        paymentId <- liftEffect genUUID
                        transactionId <- liftEffect genUUID

                        let
                          newPayment = PaymentTransaction
                            { id: paymentId
                            , transactionId: transactionId
                            , method: method
                            , amount: paymentAmount
                            , tendered: paymentTendered
                            , change: change
                            , reference: Nothing
                            , approved: true
                            , authorizationCode: Nothing
                            }

                        liftEffect do
                          setPayments (newPayment : currPayments)
                          setPaymentAmount ""
                          setTenderedAmount ""
                          setStatusMessage "Payment added"
              ) <$> paymentAmountValue
                <*> tenderedAmountValue
                <*> paymentMethodValue
                <*> paymentsValue
          ]
          [ text_ "Add Payment" ]
        ]

    , D.div
        [ DA.klass_ "payment-list mb-8" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2" ]
            [ text_ "Payment Details" ]
        , paymentsValue <#~> \payments ->
            if null payments then
              D.div [ DA.klass_ "text-gray-500" ]
                [ text_ "No payments added yet" ]
            else
              D.table
                [ DA.klass_ "w-full border" ]
                [ D.thead_
                    [ D.tr [ DA.klass_ "bg-gray-100" ]
                        [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Method" ]
                        , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Amount" ]
                        , D.th [ DA.klass_ "p-2 text-right" ]
                            [ text_ "Tendered" ]
                        , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Change" ]
                        , D.th [ DA.klass_ "p-2" ] [ text_ "Actions" ]
                        ]
                    ]
                , D.tbody_
                    ( payments <#> \(PaymentTransaction payment) ->
                        D.tr [ DA.klass_ "border-t" ]
                          [ D.td [ DA.klass_ "p-2" ]
                              [ text_ (show payment.method) ]
                          , D.td [ DA.klass_ "p-2 text-right" ]
                              [ text_ (formatMoney' payment.amount) ]
                          , D.td [ DA.klass_ "p-2 text-right" ]
                              [ text_ (formatMoney' payment.tendered) ]
                          , D.td [ DA.klass_ "p-2 text-right" ]
                              [ text_ (formatMoney' payment.change) ]
                          , D.td [ DA.klass_ "p-2 text-center" ]
                              [ D.button
                                [ DA.klass_ "text-red-600 hover:text-red-800"
                                , runOn DL.click $ 
                                    (\currPayments -> do
                                      let
                                        updatedPayments = filter
                                          (\(PaymentTransaction p) -> p.id /= payment.id)
                                          currPayments
                                      setPayments updatedPayments
                                      setStatusMessage "Payment removed"
                                    ) <$> paymentsValue
                                ]
                                [ text_ "Remove" ]
                              ]
                          ]
                    )
                ]
        ]

    , D.div
        [ DA.klass_ "transaction-summary mb-8 p-4 border rounded bg-gray-50" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2 border-b pb-2" ]
            [ text_ "Transaction Summary" ]
        , D.div
            [ DA.klass_ "grid grid-cols-2 gap-2" ]
            [ D.div [ DA.klass_ "text-right font-semibold" ]
                [ text_ "Subtotal:" ]
            , D.div [ DA.klass_ "text-right" ]
                [ subtotalValue <#~> \amount -> text_
                    (formatMoney' (fromDiscrete' amount))
                ]
            , D.div [ DA.klass_ "text-right font-semibold" ]
                [ text_ "Discount:" ]
            , D.div [ DA.klass_ "text-right" ]
                [ discountTotalValue <#~> \amount -> text_
                    (formatMoney' (fromDiscrete' amount))
                ]
            , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Tax:" ]
            , D.div [ DA.klass_ "text-right" ]
                [ taxTotalValue <#~> \amount -> text_
                    (formatMoney' (fromDiscrete' amount))
                ]
            , D.div
                [ DA.klass_ "text-right font-semibold text-lg border-t pt-1" ]
                [ text_ "Total:" ]
            , D.div [ DA.klass_ "text-right text-lg border-t pt-1" ]
                [ totalValue <#~> \amount -> text_
                    (formatMoney' (fromDiscrete' amount))
                ]
            , D.div [ DA.klass_ "text-right font-semibold pt-4" ]
                [ text_ "Payment Total:" ]
            , D.div [ DA.klass_ "text-right pt-4" ]
                [ paymentsValue <#~> \payments ->
                    let
                      paymentTotal = foldl
                        ( \acc (PaymentTransaction p) -> acc + toDiscrete
                            p.amount
                        )
                        (Discrete 0)
                        payments
                    in
                      text_ (formatMoney' (fromDiscrete' paymentTotal))
                ]
            , D.div [ DA.klass_ "text-right font-semibold" ]
                [ text_ "Remaining:" ]
            , D.div [ DA.klass_ "text-right" ]
                [ (Tuple <$> totalValue <*> paymentsValue) <#~>
                    \(Tuple total payments) ->
                      let
                        paymentTotal = foldl
                          ( \acc (PaymentTransaction p) -> acc + toDiscrete
                              p.amount
                          )
                          (Discrete 0)
                          payments
                        remaining = total - paymentTotal
                      in
                        if remaining <= Discrete 0 then D.span
                          [ DA.klass_ "text-green-600" ]
                          [ text_ "$0.00" ]
                        else D.span [ DA.klass_ "text-red-600" ]
                          [ text_ (formatMoney' (fromDiscrete' remaining)) ]
                ]
            ]
        ]

    , D.div
        [ DA.klass_ "action-buttons flex justify-between" ]
        [ D.button
            [ DA.klass_ (F.buttonClass "red")
            , DL.click_ \_ -> do
                setItems []
                setPayments []
                setSubtotal (Discrete 0)
                setDiscountTotal (Discrete 0)
                setTaxTotal (Discrete 0)
                setTotal (Discrete 0)
                setStatusMessage "Transaction cleared"
            ]
            [ text_ "Clear Transaction" ]

        , D.button
          [ DA.klass_ (F.buttonClass "green")
          , DA.disabled $
              isProcessingValue <#> \isProcessing ->
                if isProcessing then "true" else ""
          , runOn DL.click $ 
              (\currItems currPayments currTotal empId regId locId discTotal taxTotal -> 
                do
                  if null currItems then do
                    setStatusMessage "Cannot complete: No items in transaction"
                  else do
                    let
                      paymentTotal = foldl
                        ( \acc (PaymentTransaction p) -> acc + toDiscrete p.amount )
                        (Discrete 0)
                        currPayments
                    if paymentTotal < currTotal then do
                      setStatusMessage "Cannot complete: Payment amount is insufficient"
                    else do
                      setIsProcessing true
                      setStatusMessage "Processing transaction..."

                      void $ launchAff do
                        transactionId <- liftEffect genUUID
                        currentTime <- liftEffect now

                        let
                          curTime = toDateTime currentTime
                          updatedItems = map
                            ( \(TransactionItem item) ->
                                TransactionItem
                                  (item { transactionId = transactionId })
                            )
                            currItems

                          updatedPayments = map
                            ( \(PaymentTransaction payment) ->
                                PaymentTransaction
                                  (payment { transactionId = transactionId })
                            )
                            currPayments

                          employeeUUID = parseUUID empId
                          registerUUID = parseUUID regId
                          locationUUID = parseUUID locId

                        case
                          Tuple (Tuple employeeUUID registerUUID) locationUUID
                          of
                          Tuple (Tuple (Just empId) (Just regId)) (Just locId) ->
                            do
                              let
                                transaction = Transaction
                                  { id: transactionId
                                  , status: Completed
                                  , created: toDateTime currentTime
                                  , completed: Just curTime
                                  , customer: Nothing             
                                  , employee: empId              
                                  , register: regId               
                                  , location: locId               
                                  , items: updatedItems
                                  , payments: updatedPayments
                                  , subtotal: fromDiscrete'
                                      (currTotal - taxTotal + discTotal)
                                  , discountTotal: fromDiscrete' discTotal
                                  , taxTotal: fromDiscrete' taxTotal
                                  , total: fromDiscrete' currTotal
                                  , transactionType: Sale
                                  , isVoided: false
                                  , voidReason: Nothing
                                  , isRefunded: false
                                  , refundReason: Nothing
                                  , referenceTransactionId: Nothing
                                  , notes: Nothing
                                  }

                              result <- API.createTransaction transaction

                              liftEffect case result of
                                Right completedTx -> do
                                  setItems []
                                  setPayments []
                                  setSubtotal (Discrete 0)
                                  setDiscountTotal (Discrete 0)
                                  setTaxTotal (Discrete 0)
                                  setTotal (Discrete 0)
                                  setStatusMessage "Transaction completed successfully"
                                Left err -> do
                                  setStatusMessage $ "Error completing transaction: " <> err

                          _ -> liftEffect $ setStatusMessage "Invalid employee, register or location ID"

                        liftEffect $ setIsProcessing false
              ) <$> itemsValue
                <*> paymentsValue
                <*> totalValue
                <*> employeeValue
                <*> registerIdValue
                <*> locationIdValue
                <*> discountTotalValue
                <*> taxTotalValue
          ]
          [ isProcessingValue <#~> \isProcessing ->
              if isProcessing then text_ "Processing..."
              else text_ "Complete Transaction"
          ]
        ]

    , D.div
        [ DA.klass_ "status-message mt-4 p-2 text-center" ]
        [ statusMessageValue <#~> \msg ->
            if msg == "" then
              D.span_ []
            else if contains "Error" msg then
              D.div [ DA.klass_ "bg-red-100 text-red-800 p-2 rounded" ]
                [ text_ msg ]
            else
              D.div [ DA.klass_ "bg-green-100 text-green-800 p-2 rounded" ]
                [ text_ msg ]
        ]
    ]

processValidItem
  :: Number
  -> MenuItem
  -> Discrete USD
  -> Discrete USD
  -> Discrete USD
  -> Array TransactionItem
  -> (Discrete USD -> Effect Unit)
  -> (Discrete USD -> Effect Unit)
  -> (Discrete USD -> Effect Unit)
  -> (Array TransactionItem -> Effect Unit)
  -> (Maybe MenuItem -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit
processValidItem
  qtyNum
  (MenuItem item)
  currSubtotal
  currTaxTotal
  currTotal
  currItems
  setSubtotal
  setTaxTotal
  setTotal
  setItems
  setSelectedItem
  setStatusMessage = do
  void $ launchAff do
    itemId <- liftEffect genUUID
    transactionId <- liftEffect genUUID
    currentTime <- liftEffect now

    let
      priceInDollars = toDollars item.price
      price = fromDollars priceInDollars
      priceAsDiscrete = fromDiscrete' price

      qtyAsInt = Int.floor qtyNum
      itemSubtotal = fromDiscrete' (price * (Discrete qtyAsInt))

      -- Fixed tax calculation - convert to Int, calculate, then convert back
      taxRate = 0.1
      taxRateInt = Int.floor (taxRate * 100.0)

      -- Extract the raw Int value from Discrete
      subtotalAsInt = case toDiscrete itemSubtotal of
        Discrete n -> n

      -- Calculate tax amount in Int
      taxAmountInt = (subtotalAsInt * taxRateInt) / 100

      -- Convert back to DiscreteMoney
      itemTaxTotal = fromDiscrete' (Discrete taxAmountInt)

      itemTotal = itemSubtotal + itemTaxTotal

      newItem = TransactionItem
        { id: itemId
        , transactionId: transactionId
        , menuItemSku: item.sku
        , quantity: qtyNum
        , pricePerUnit: priceAsDiscrete
        , discounts: []
        , taxes:
            [ { category: RegularSalesTax
              , rate: taxRate
              , amount: itemTaxTotal
              , description: "Sales Tax"
              }
            ]
        , subtotal: itemSubtotal
        , total: itemTotal
        }

    liftEffect do
      setSubtotal (currSubtotal + toDiscrete itemSubtotal)
      setTaxTotal (currTaxTotal + toDiscrete itemTaxTotal)
      setTotal (currTotal + toDiscrete itemTotal)

      setItems (newItem : currItems)
      setSelectedItem Nothing
      setStatusMessage "Item added to transaction"

readFloat :: String -> Maybe Number
readFloat str = Number.fromString (trim str)

contains :: String -> String -> Boolean
contains str substr =
  case String.indexOf (Pattern substr) str of
    Just _ -> true
    Nothing -> false

toLowerCase :: String -> String
toLowerCase = String.toLower

padStart :: Int -> String -> String
padStart targetLength str =
  let
    paddingLength = max 0 (targetLength - String.length str)
    padding = replicate paddingLength "0"
  in
    joinWith "" padding <> str