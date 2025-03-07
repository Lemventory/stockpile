module UI.Transaction.CreateTransaction where

import Prelude

import API.Inventory (readInventory)
import Data.Array (filter, find, foldl, length, null, replicate, (:))
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..), fromDiscrete)
import Data.Foldable (for_)
import Data.Int (fromNumber, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (Pattern(..), indexOf, joinWith, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
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
import Types.DiscreteUSD (toDiscrete)
import Types.Inventory (Inventory(..), InventoryResponse(..), MenuItem(..))
import Types.Transaction (PaymentMethod(..), PaymentTransaction(..), TaxCategory(..), Transaction(..), TransactionItem(..), TransactionStatus(..), TransactionType(..))
import Types.UUID (UUID(..))
import UI.Common.Form as F
import Utils.Formatting (parseUUID)
import Utils.Money (formatMoney')
import Utils.UUIDGen (genUUID)
import Web.Event.Event (Event, target)
import Web.Event.Event as Event
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select
import Web.HTML.HTMLTextAreaElement as TextArea

createTransaction :: Nut
createTransaction = Deku.do
  -- Transaction state
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
  
  -- UI state
  setInventory /\ inventoryValue <- useState []
  setSearchText /\ searchTextValue <- useState ""
  setSelectedItem /\ selectedItemValue <- useState Nothing
  setItemQuantity /\ itemQuantityValue <- useState "1"
  setPaymentMethod /\ paymentMethodValue <- useState Cash
  setPaymentAmount /\ paymentAmountValue <- useState ""
  setTenderedAmount /\ tenderedAmountValue <- useState ""
  setStatusMessage /\ statusMessageValue <- useState ""
  setIsProcessing /\ isProcessingValue <- useState false
  
  -- Component loading effect
  D.div
    [ DA.klass_ "transaction-container container mx-auto p-4"
    , DL.load_ \_ -> do
        liftEffect $ Console.log "Transaction component loading"
        
        -- Generate default IDs
        void $ launchAff do
          employeeId <- liftEffect genUUID
          registerId <- liftEffect genUUID
          locationId <- liftEffect genUUID
          
          liftEffect do
            setEmployee (show employeeId)
            setRegisterId (show registerId)
            setLocationId (show locationId)
          
        -- Load inventory items
        void $ launchAff do
          result <- readInventory
          liftEffect case result of
            Right (InventoryData (Inventory items)) -> do
              Console.log $ "Loaded " <> show (length items) <> " inventory items"
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
        
      -- Inventory search section
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
                        if text == ""
                        then D.div_ []
                        else D.div_ (
                          filter (\(MenuItem i) ->
                            contains (toLowerCase i.name) (toLowerCase text) ||
                            contains (toLowerCase i.brand) (toLowerCase text)
                          ) items 
                          <#> \item@(MenuItem i) ->
                            D.div
                              [ DA.klass_ "inventory-item p-2 border rounded cursor-pointer hover:bg-gray-100"
                              , DL.click_ \_ -> do
                                  setSelectedItem (Just item)
                                  setItemQuantity "1"
                              ]
                              [ D.div [ DA.klass_ "font-semibold" ] [ text_ i.name ]
                              , D.div [ DA.klass_ "text-sm" ] [ text_ ("$" <> show i.price) ]
                              ]
                        )
                  in
                    if null items
                      then D.div [ DA.klass_ "text-gray-500" ] [ text_ "No items found" ]
                      else D.div [ DA.klass_ "grid grid-cols-3 gap-2" ]
                        (map (\item@(MenuItem i) ->
                          D.div
                            [ DA.klass_ "inventory-item p-2 border rounded cursor-pointer hover:bg-gray-100"
                            , DL.click_ \_ -> do
                                setSelectedItem (Just item)
                                setItemQuantity "1"
                            ]
                            [ D.div [ DA.klass_ "font-semibold" ] [ text_ i.name ]
                            , D.div [ DA.klass_ "text-sm" ] [ text_ ("$" <> show i.price) ]
                            ]
                        ) items)
              ]
          ]
          
      -- Selected item details section
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
                    , D.div [ DA.klass_ "text-sm mb-4" ] [ text_ ("$" <> show item.price) ]
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
                                for_ (target evt >>= Input.fromEventTarget) \el -> do
                                  value <- Input.value el
                                  setItemQuantity value
                            ]
                            []
                        , D.button
                          [ DA.klass_ (F.buttonClass "green")
                          , DL.click_ \_ -> do
                              itemQuantity <- itemQuantityValue
                              case (readFloat itemQuantity) of
                                Nothing -> do
                                  liftEffect $ setStatusMessage "Invalid quantity"
                                Just qty -> void $ launchAff do
                                  itemId <- liftEffect genUUID
                                  transactionId <- liftEffect genUUID
                                  now <- liftEffect now

                                  let
                                    -- Create the Discrete USD first
                                    priceInCents = Int.floor (item.price * 100.0)
                                    price = Discrete priceInCents
                                    
                                    -- Convert to DiscreteUSD for our internal representation
                                    priceAsDiscreteUSD = fromDiscrete price
                                    
                                    -- Calculate subtotal directly as DiscreteUSD
                                    qtyAsInt = Int.floor qty
                                    itemSubtotal = priceAsDiscreteUSD * (fromDiscrete (Discrete qtyAsInt))

                                    -- Calculate tax as 10% of subtotal (still DiscreteUSD)
                                    taxRate = 0.1
                                    itemTaxTotal = fromNumber (itemSubtotal * taxRate)
                                    
                                    -- Total is subtotal + tax
                                    itemTotal = itemSubtotal + itemTaxTotal

                                    -- Create the new item
                                    newItem = TransactionItem
                                      { id: itemId
                                      , transactionId: transactionId
                                      , menuItemSku: item.sku
                                      , quantity: qty
                                      , pricePerUnit: priceAsDiscreteUSD
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
                                    -- Update the transaction totals
                                    currentSubtotal <- subtotalValue
                                    currentTaxTotal <- taxTotalValue
                                    currentTotal <- totalValue

                                    -- Convert our DiscreteUSD to Discrete USD for the transaction totals
                                    setSubtotal (currentSubtotal + toDiscrete itemSubtotal)
                                    setTaxTotal (currentTaxTotal + toDiscrete itemTaxTotal)
                                    setTotal (currentTotal + toDiscrete itemTotal)

                                    -- Add item to the items list
                                    currentItems <- itemsValue
                                    setItems (newItem : currentItems)

                                    -- Reset UI state
                                    setSelectedItem Nothing
                                    setStatusMessage "Item added to transaction"
                          ]
                          [ text_ "Add to Transaction" ]
                        ]
                    ]
          ]
      
      -- Current transaction items list
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
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Quantity" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Price" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Subtotal" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Tax" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Total" ]
                          , D.th [ DA.klass_ "p-2" ] [ text_ "Actions" ]
                          ]
                      ]
                  , D.tbody_
                      (items <#> \(TransactionItem item) ->
                        let 
                          taxTotal = foldl (\acc tax -> acc + tax.amount) (Discrete 0) item.taxes
                        in
                          D.tr [ DA.klass_ "border-t" ]
                            [ D.td [ DA.klass_ "p-2" ] 
                                [ inventoryValue <#~> \invItems ->
                                    let 
                                      itemInfo = find (\(MenuItem i) -> i.sku == item.menuItemSku) invItems
                                    in
                                      case itemInfo of
                                        Just (MenuItem i) -> text_ i.name
                                        Nothing -> text_ "Unknown Item"
                                ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (show item.quantity) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' item.pricePerUnit) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' item.subtotal) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' taxTotal) ]
                            , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' item.total) ]
                            , D.td [ DA.klass_ "p-2 text-center" ]
                                [ D.button
                                    [ DA.klass_ "text-red-600 hover:text-red-800"
                                    , DL.click_ \_ -> do
                                        currentItems <- itemsValue
                                        let 
                                          updatedItems = filter 
                                            (\(TransactionItem i) -> i.id /= item.id) 
                                            currentItems
                                        
                                        -- Update transaction totals
                                        currentSubtotal <- subtotalValue
                                        currentTaxTotal <- taxTotalValue
                                        currentTotal <- totalValue
                                        
                                        setSubtotal (currentSubtotal - item.subtotal)
                                        setTaxTotal (currentTaxTotal - taxTotal)
                                        setTotal (currentTotal - item.total)
                                        
                                        setItems updatedItems
                                        setStatusMessage "Item removed from transaction"
                                    ]
                                    [ text_ "Remove" ]
                                ]
                            ]
                      )
                  ]
          ]
      
      -- Payment section
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
              , DL.click_ \_ -> do
                  case (readFloat paymentAmountValue) of
                    Nothing -> 
                      setStatusMessage "Invalid payment amount"
                    Just amount -> do
                      method <- paymentMethodValue
                      
                      let 
                        tenderedAmount = case readFloat tenderedAmountValue of
                          Just t -> t
                          Nothing -> amount
                        
                        paymentAmount = Discrete (amount * 100.0)
                        paymentTendered = Discrete (tenderedAmount * 100.0)
                        change = if paymentTendered > paymentAmount 
                                then paymentTendered - paymentAmount
                                else Discrete 0
                      
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
                          currPayments <- paymentsValue
                          setPayments (newPayment : currPayments)
                          setPaymentAmount ""
                          setTenderedAmount ""
                          setStatusMessage "Payment added"
              ]
              [ text_ "Add Payment" ]
          ]
      
      -- Payment list
      , D.div
          [ DA.klass_ "payment-list mb-8" ]
          [ D.h3
              [ DA.klass_ "text-lg font-semibold mb-2" ]
              [ text_ "Payment Details" ]
          , paymentsValue <#~> \payments ->
              if null payments then
                D.div [ DA.klass_ "text-gray-500" ] [ text_ "No payments added yet" ]
              else
                D.table
                  [ DA.klass_ "w-full border" ]
                  [ D.thead_
                      [ D.tr [ DA.klass_ "bg-gray-100" ]
                          [ D.th [ DA.klass_ "p-2 text-left" ] [ text_ "Method" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Amount" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Tendered" ]
                          , D.th [ DA.klass_ "p-2 text-right" ] [ text_ "Change" ]
                          , D.th [ DA.klass_ "p-2" ] [ text_ "Actions" ]
                          ]
                      ]
                  , D.tbody_
                      (payments <#> \(PaymentTransaction payment) ->
                        D.tr [ DA.klass_ "border-t" ]
                          [ D.td [ DA.klass_ "p-2" ] [ text_ (show payment.method) ]
                          , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' payment.amount) ]
                          , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' payment.tendered) ]
                          , D.td [ DA.klass_ "p-2 text-right" ] [ text_ (formatMoney' payment.change) ]
                          , D.td [ DA.klass_ "p-2 text-center" ]
                              [ D.button
                                  [ DA.klass_ "text-red-600 hover:text-red-800"
                                  , DL.click_ \_ -> do
                                      currentPayments <- paymentsValue
                                      let updatedPayments = filter 
                                            (\(PaymentTransaction p) -> p.id /= payment.id) 
                                            currentPayments
                                      setPayments updatedPayments
                                      setStatusMessage "Payment removed"
                                  ]
                                  [ text_ "Remove" ]
                              ]
                          ]
                      )
                  ]
          ]
      
      -- Transaction summary
      , D.div
          [ DA.klass_ "transaction-summary mb-8 p-4 border rounded bg-gray-50" ]
          [ D.h3
              [ DA.klass_ "text-lg font-semibold mb-2 border-b pb-2" ]
              [ text_ "Transaction Summary" ]
          , D.div
              [ DA.klass_ "grid grid-cols-2 gap-2" ]
              [ D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Subtotal:" ]
              , D.div [ DA.klass_ "text-right" ] [ subtotalValue <#~> \amount -> text_ (formatMoney' amount) ]
              , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Discount:" ]
              , D.div [ DA.klass_ "text-right" ] [ discountTotalValue <#~> \amount -> text_ (formatMoney' amount) ]
              , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Tax:" ]
              , D.div [ DA.klass_ "text-right" ] [ taxTotalValue <#~> \amount -> text_ (formatMoney' amount) ]
              , D.div [ DA.klass_ "text-right font-semibold text-lg border-t pt-1" ] [ text_ "Total:" ]
              , D.div [ DA.klass_ "text-right text-lg border-t pt-1" ] [ totalValue <#~> \amount -> text_ (formatMoney' amount) ]
              , D.div [ DA.klass_ "text-right font-semibold pt-4" ] [ text_ "Payment Total:" ]
              , D.div [ DA.klass_ "text-right pt-4" ] 
                  [ paymentsValue <#~> \payments -> 
                      let paymentTotal = foldl (\acc (PaymentTransaction p) -> acc + p.amount) (Discrete 0) payments
                      in text_ (formatMoney' paymentTotal)
                  ]
              , D.div [ DA.klass_ "text-right font-semibold" ] [ text_ "Remaining:" ]
              , D.div [ DA.klass_ "text-right" ] 
                  [ (Tuple <$> totalValue <*> paymentsValue) <#~> \(Tuple total payments) -> 
                      let 
                        paymentTotal = foldl (\acc (PaymentTransaction p) -> acc + p.amount) (Discrete 0) payments
                        remaining = total - paymentTotal
                      in 
                        if remaining <= Discrete 0
                          then D.span [ DA.klass_ "text-green-600" ] [ text_ "$0.00" ]
                          else D.span [ DA.klass_ "text-red-600" ] [ text_ (formatMoney' remaining) ]
                  ]
              ]
          ]
          
      -- Action buttons
      , D.div
          [ DA.klass_ "action-buttons flex justify-between" ]
          [ D.button
              [ DA.klass_ (F.buttonClass "red")
              , DL.click_ \_ -> do
                  -- Reset the form
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
              , DL.click_ \_ -> do
                  currentItems <- itemsValue
                  currentPayments <- paymentsValue
                  currentTotal <- totalValue
                  currentEmployeeId <- employeeValue
                  currentRegisterId <- registerIdValue
                  currentLocationId <- locationIdValue
                  
                  -- Validate
                  if null currentItems then
                    setStatusMessage "Cannot complete: No items in transaction"
                  else do
                    let paymentTotal = foldl (\acc (PaymentTransaction p) -> acc + p.amount) (Discrete 0) currentPayments
                    if paymentTotal < currentTotal then
                      setStatusMessage "Cannot complete: Payment amount is insufficient"
                    else do
                      -- Process transaction
                      setIsProcessing true
                      setStatusMessage "Processing transaction..."
                      
                      void $ launchAff do
                        transactionId <- liftEffect genUUID
                        timestamp <- liftEffect now
                        
                        -- Create a complete transaction object
                        discTotal <- liftEffect discountTotalValue
                        taxTotal <- liftEffect taxTotalValue
                        
                        -- Update transaction items with the correct transaction ID
                        let 
                          updatedItems = map 
                            (\(TransactionItem item) -> 
                              TransactionItem (item { transactionId = transactionId }))
                            currentItems
                            
                          -- Update payments with the correct transaction ID
                          updatedPayments = map
                            (\(PaymentTransaction payment) -> 
                              PaymentTransaction (payment { transactionId = transactionId }))
                            currentPayments
                          
                          employeeUUID = parseUUID currentEmployeeId
                          registerUUID = parseUUID currentRegisterId
                          locationUUID = parseUUID currentLocationId
                          
                        case Tuple (Tuple employeeUUID registerUUID) locationUUID of
                          Tuple (Tuple (Just empId) (Just regId)) (Just locId) -> do
                            let transaction = Transaction
                                  { id: transactionId
                                  , status: Completed
                                  , created: timestamp
                                  , completed: Just timestamp
                                  , customerId: Nothing
                                  , employeeId: empId
                                  , registerId: regId
                                  , locationId: locId
                                  , items: updatedItems
                                  , payments: updatedPayments
                                  , subtotal: currentTotal - taxTotal + discTotal
                                  , discountTotal: discTotal
                                  , taxTotal: taxTotal
                                  , total: currentTotal
                                  , transactionType: Sale
                                  , isVoided: false
                                  , voidReason: Nothing
                                  , isRefunded: false
                                  , refundReason: Nothing
                                  , referenceTransactionId: Nothing
                                  , notes: Nothing
                                  }
                            
                            -- Submit transaction to API
                            result <- createTransaction transaction
                            
                            liftEffect case result of
                              Right completedTx -> do
                                -- Reset form on success
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
              ]
              [ isProcessingValue <#~> \isProcessing ->
                  if isProcessing then text_ "Processing..." else text_ "Complete Transaction"
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

-- -- Helper functions
-- formatMoney' :: Discrete USD -> String
-- formatMoney' (Discrete cents) =
--   let
--     dollars = Int.toNumber cents / 100.0
--     formatted = if Int.toNumber (Int.floor (dollars * 100.0)) / 100.0 == dollars
--                 then show (Int.floor dollars) <> "." <> padStart 2 (show (Int.floor ((dollars - dollars) * 100.0)))
--                 else show dollars
--   in
--     "$" <> formatted

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
