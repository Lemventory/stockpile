module UI.Transaction.CreateTransaction where

import Prelude

import API.Inventory (readInventory)
import API.Transaction (createTransaction) as API
import Data.Array (filter, find, foldl, length, null, sort, (:))
import Data.Array (nub) as Array
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Finance.Money.Extended (fromDiscrete', toDiscrete)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.String (Pattern(..), contains, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
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

-- Helper functions
updateNumpad :: String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
updateNumpad digit setNumpadValue setItemQuantity = do
  -- Get the current value
  numpadValue <- readNumpadValueST
  
  -- Only allow one decimal point
  let newValue = 
        if digit == "." && contains (Pattern ".") numpadValue
        then numpadValue
        else numpadValue <> digit
  
  -- Update both displays
  setNumpadValue newValue
  setItemQuantity newValue

-- We need this stub since we can't directly read from Poll in an Effect
readNumpadValueST :: Effect String
readNumpadValueST = pure ""

-- Helper to read float values
readFloat :: String -> Maybe Number
readFloat str = Number.fromString (trim str)

-- Helper to process valid items
processValidItem :: Number
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
  -> (String -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect Unit
processValidItem 
  qtyNum
  menuItem@(MenuItem item)
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
  setNumpadValue
  setItemQuantity = do
  void $ launchAff do
    itemId <- liftEffect genUUID
    transactionId <- liftEffect genUUID

    let
      priceInDollars = toDollars item.price
      price = fromDollars priceInDollars
      priceAsDiscrete = fromDiscrete' price

      qtyAsInt = Int.floor qtyNum
      itemSubtotal = fromDiscrete' (price * (Discrete qtyAsInt))

      taxRate = 0.1
      taxRateInt = Int.floor (taxRate * 100.0)

      subtotalAsInt = case toDiscrete itemSubtotal of
        Discrete n -> n

      taxAmountInt = (subtotalAsInt * taxRateInt) / 100

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
      setNumpadValue ""
      setItemQuantity "1"

formatCentsToDollars :: Int -> String
formatCentsToDollars cents =
  let
    dollars = cents / 100
    centsRemaining = cents `mod` 100
    centsStr = if centsRemaining < 10
               then "0" <> show centsRemaining
               else show centsRemaining
  in
    show dollars <> "." <> centsStr

createTransaction :: Nut
createTransaction = Deku.do

  -- State for cart and transaction
  setItems /\ itemsValue <- useState []
  setPayments /\ paymentsValue <- useState []

  -- Transaction data
  setEmployee /\ employeeValue <- useState ""
  setRegisterId /\ registerIdValue <- useState ""
  setLocationId /\ locationIdValue <- useState ""
  setSubtotal /\ subtotalValue <- useState (Discrete 0)
  setDiscountTotal /\ discountTotalValue <- useState (Discrete 0)
  setTaxTotal /\ taxTotalValue <- useState (Discrete 0)
  setTotal /\ totalValue <- useState (Discrete 0)

  -- Inventory and search
  setInventory /\ inventoryValue <- useState []
  setFilteredInventory /\ filteredInventoryValue <- useState []
  setSearchText /\ searchTextValue <- useState ""
  
  -- Selected item and quantity
  setSelectedItem /\ selectedItemValue <- useState Nothing
  setItemQuantity /\ itemQuantityValue <- useState "1"
  
  -- Payment information
  setPaymentMethod /\ paymentMethodValue <- useState Cash
  setPaymentAmount /\ paymentAmountValue <- useState ""
  setTenderedAmount /\ tenderedAmountValue <- useState ""
  
  -- UI state
  setStatusMessage /\ statusMessageValue <- useState ""
  setIsProcessing /\ isProcessingValue <- useState false
  setActiveCategory /\ activeCategoryValue <- useState "All Items"
  setNumpadValue /\ numpadValueValue <- useState ""

  -- Get unique categories from inventory
  let 
    getCategories = inventoryValue <#> \items -> 
      ["All Items"] <> (sort $ Array.nub $ map (\(MenuItem i) -> show i.category) items)

  D.div
    [ DA.klass_ "tx-main-container"
    -- , DA.style_ styles -- This line is important
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
              Console.log $ "Loaded " <> show (length items) <> " inventory items"
              setInventory items
              setFilteredInventory items
            Right (Message msg) -> do
              Console.error $ "API Message: " <> msg
              setStatusMessage $ "Error: " <> msg
            Left err -> do
              Console.error $ "Failed to load inventory: " <> err
              setStatusMessage $ "Error loading inventory: " <> err
    ]
    [ 
      -- Content area with cart and inventory
      D.div
        [ DA.klass_ "tx-content-area" ]
        [
          -- Cart container
          D.div
            [ DA.klass_ "tx-cart-container" ]
            [
              -- Cart header with column titles
              D.div
                [ DA.klass_ "tx-cart-header" ]
                [
                  D.div 
                    [ DA.klass_ "tx-item-details" ]
                    [
                      D.div [ DA.klass_ "tx-item-quantity" ] [ text_ "Qty" ],
                      D.div [ DA.klass_ "tx-item-name" ] [ text_ "Item" ]
                    ],
                  D.div [ DA.klass_ "tx-item-price" ] [ text_ "Price" ],
                  D.div [ DA.klass_ "tx-item-total" ] [ text_ "Total" ],
                  D.div [ DA.klass_ "tx-item-actions" ] [ text_ "" ]
                ],
              
              -- Scrollable cart items area
              D.div
                [ DA.klass_ "tx-cart-items" ]
                [ itemsValue <#~> \items ->
                    if null items then
                      D.div [ DA.klass_ "tx-text-gray-500 text-center p-4" ] [ text_ "No items added yet" ]
                    else
                      D.div_ (items <#> \(TransactionItem item) ->
                        let
                          taxTotal = foldl (\acc tax -> acc + toDiscrete tax.amount) (Discrete 0) item.taxes
                          taxTotalMoney = fromDiscrete' taxTotal
                        in
                          D.div
                            [ DA.klass_ "tx-cart-item" ]
                            [
                              D.div 
                                [ DA.klass_ "tx-item-details" ]
                                [
                                  D.div [ DA.klass_ "tx-item-quantity" ] [ text_ (show item.quantity) ],
                                  D.div [ DA.klass_ "tx-item-name" ] 
                                    [ inventoryValue <#~> \invItems ->
                                        let
                                          itemInfo = find (\(MenuItem i) -> i.sku == item.menuItemSku) invItems
                                        in
                                          case itemInfo of
                                            Just (MenuItem i) -> text_ i.name
                                            Nothing -> text_ "Unknown Item"
                                    ]
                                ],
                              D.div [ DA.klass_ "tx-item-price" ] [ text_ (formatMoney' item.pricePerUnit) ],
                              D.div [ DA.klass_ "tx-item-total" ] [ text_ (formatMoney' item.total) ],
                              D.div 
                                [ DA.klass_ "tx-item-actions" ]
                                [
                                  D.button
                                    [ DA.klass_ "tx-delete-btn"
                                    , runOn DL.click $ (\currItems currSubtotal currTaxTotal currTotal -> do
                                        let
                                          updatedItems = filter (\(TransactionItem i) -> i.id /= item.id) currItems
                                        setSubtotal (currSubtotal - toDiscrete item.subtotal)
                                        setTaxTotal (currTaxTotal - taxTotal)
                                        setTotal (currTotal - toDiscrete item.total)
                                        setItems updatedItems
                                        setStatusMessage "Item removed from transaction"
                                      ) <$> itemsValue <*> subtotalValue <*> taxTotalValue <*> totalValue
                                    ]
                                    [ text_ "✕" ]
                                ]
                            ]
                      )
                ],
              
              -- Cart totals area
              D.div
                [ DA.klass_ "tx-cart-totals" ]
                [
                  D.div
                    [ DA.klass_ "tx-total-row" ]
                    [
                      D.div_ [ text_ "Subtotal" ],
                      D.div_ [ subtotalValue <#~> \amount -> text_ (formatMoney' (fromDiscrete' amount)) ]
                    ],
                  D.div
                    [ DA.klass_ "tx-total-row" ]
                    [
                      D.div_ [ text_ "Tax" ],
                      D.div_ [ taxTotalValue <#~> \amount -> text_ (formatMoney' (fromDiscrete' amount)) ]
                    ],
                  D.div
                    [ DA.klass_ "tx-grand-total" ]
                    [
                      D.div_ [ text_ "Total" ],
                      D.div_ [ totalValue <#~> \amount -> text_ (formatMoney' (fromDiscrete' amount)) ]
                    ]
                ]
            ],
          
          -- Inventory container
          D.div
            [ DA.klass_ "tx-inventory-container" ]
            [
              -- Inventory header
              D.div
                [ DA.klass_ "tx-inventory-header" ]
                [
                  D.h3_ [ text_ "Inventory" ],
                  D.div
                    [ DA.klass_ "tx-inventory-controls" ]
                    [
                      D.input
                      [ DA.klass_ "tx-search-input p-2 border rounded"
                      , DA.placeholder_ "Search inventory..."
                      , DA.value_ ""
                      , DL.input_ \evt -> do
                        for_ (Event.target evt >>= Input.fromEventTarget) \el -> do
                          value <- Input.value el
                          setSearchText value
                      ]
                      []
                    ]
                ],
              
              -- Inventory category tabs
              D.div
                [ DA.klass_ "tx-inventory-tabs" ]
                [ getCategories <#~> \categories ->
                    D.div_ (categories <#> \cat ->
                      D.div 
                      [ DA.klass $ activeCategoryValue <#> \active ->
                          "inventory-tab" <> if active == cat then " active" else ""
                      ]
                      [ text_ cat ]
                    )
                ],
              
              -- Inventory grid with scrollable content
              D.div
                [ DA.klass_ "tx-inventory-grid" ]
                [ 
                  (Tuple <$> (Tuple <$> searchTextValue <*> activeCategoryValue) <*> inventoryValue) <#~> \((searchText /\ activeCategory) /\ items) ->
                    let 
                      -- First filter by category
                      categoryFiltered = 
                        if activeCategory == "All Items" 
                        then items 
                        else filter (\(MenuItem i) -> show i.category == activeCategory) items
                        
                      -- Then filter by search text
                      searchFiltered =
                        if searchText == ""
                        then categoryFiltered
                        else filter
                              (\(MenuItem item) ->
                                contains (Pattern (String.toLower searchText))
                                        (String.toLower item.name))
                              categoryFiltered
                    in
                      if null searchFiltered then
                        D.div [ DA.klass_ "tx-text-gray-500 text-center p-4" ] [ text_ "No items found" ]
                      else
                        D.div_ (searchFiltered <#> \item@(MenuItem i) ->
                          -- Keep your existing item rendering code here
                          D.div
                            [ DA.klass_ "tx-inventory-item"
                            , DL.click_ \_ -> do
                                setSelectedItem (Just item)
                                setItemQuantity "1"
                                setNumpadValue ""
                            ]
                            [
                              D.div [ DA.klass_ "tx-item-image" ] [ text_ "IMG" ],
                              D.div [ DA.klass_ "tx-item-name" ] [ text_ i.name ],
                              D.div
                                [ DA.klass_ $
                                    "item-stock" <> if i.quantity <= 5 then " low-stock" else ""
                                ]
                                [ text_ ("Stock: " <> show i.quantity) ],
                              D.div [ DA.klass_ "tx-item-price" ] [ text_ ("$" <> formatCentsToDollars (unwrap i.price)) ]
                            ]
                        )
                ]
            ]
        ],
      
      -- Bottom area with numpad and payment options
      D.div
        [ DA.klass_ "tx-bottom-area" ]
        [
          -- Numpad panel
          D.div
            [ DA.klass_ "tx-numpad-panel" ]
            [
              -- Display selected item and quantity
              selectedItemValue <#~> \maybeItem ->
                case maybeItem of
                  Nothing -> 
                    D.div [ DA.klass_ "tx-p-2 text-gray-500" ] 
                      [ text_ "Select an item from inventory" ]
                  Just (MenuItem item) ->
                    D.div
                      [ DA.klass_ "tx-selected-item p-2 mb-2 border-b" ]
                      [
                        D.div [ DA.klass_ "tx-font-semibold" ] [ text_ item.name ],
                        D.div [ DA.klass_ "tx-flex items-center mt-2" ]
                          [
                            D.label [ DA.klass_ "tx-mr-2" ] [ text_ "Qty:" ],
                            D.input
                              [ DA.klass_ "tx-form-input w-16 p-1 border rounded"
                              , DA.value itemQuantityValue -- Use value instead of value_
                              , DA.xtype_ "text"
                              , DA.readonly_ "readonly"
                              ]
                              []
                          ]
                      ],
              
              -- Numpad value display
              D.div
                [ DA.klass_ "tx-numpad-display mb-2 p-2 border rounded bg-white text-right text-xl" ]
                [ text numpadValueValue ],
                
              -- Numpad grid
              D.div
                [ DA.klass_ "tx-numpad-grid" ]
                [
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "7" setNumpadValue setItemQuantity
                    ]
                    [ text_ "7" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "8" setNumpadValue setItemQuantity
                    ]
                    [ text_ "8" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "9" setNumpadValue setItemQuantity
                    ]
                    [ text_ "9" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "4" setNumpadValue setItemQuantity
                    ]
                    [ text_ "4" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "5" setNumpadValue setItemQuantity
                    ]
                    [ text_ "5" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "6" setNumpadValue setItemQuantity
                    ]
                    [ text_ "6" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "1" setNumpadValue setItemQuantity
                    ]
                    [ text_ "1" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "2" setNumpadValue setItemQuantity
                    ]
                    [ text_ "2" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "3" setNumpadValue setItemQuantity
                    ]
                    [ text_ "3" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "0" setNumpadValue setItemQuantity
                    ]
                    [ text_ "0" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn"
                    , DL.click_ \_ -> updateNumpad "." setNumpadValue setItemQuantity
                    ]
                    [ text_ "." ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn delete"
                    , DL.click_ \_ -> do
                        setNumpadValue ""
                        setItemQuantity "1"
                    ]
                    [ text_ "←" ],
                  D.button
                    [ DA.klass_ "tx-numpad-btn enter"
                    , runOn DL.click $
                        map
                          (\args@{ qty, maybeSelectedItem, currSubtotal, currTaxTotal, currTotal, currItems } ->
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
                                      setNumpadValue
                                      setItemQuantity
                                  Nothing ->
                                    setStatusMessage "No item selected"
                          )
                          ({ qty: _, maybeSelectedItem: _, currSubtotal: _, currTaxTotal: _, currTotal: _, currItems: _ }
                            <$> itemQuantityValue
                            <*> selectedItemValue
                            <*> subtotalValue
                            <*> taxTotalValue
                            <*> totalValue
                            <*> itemsValue
                          )
                    ]
                    [ text_ "Add to Cart" ]
                ]
            ],
          
          -- Payment panel
          D.div
            [ DA.klass_ "tx-payment-panel" ]
            [
              D.div
                [ DA.klass_ "tx-payment-header" ]
                [ text_ "Payment Options" ],
              
              -- Payment methods
              D.div
                [ DA.klass_ "tx-payment-methods" ]
                [
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == Cash then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod Cash
                    ]
                    [ text_ "Cash" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == Credit then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod Credit
                    ]
                    [ text_ "Credit Card" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == Debit then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod Debit
                    ]
                    [ text_ "Debit Card" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == ACH then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod ACH
                    ]
                    [ text_ "ACH" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == GiftCard then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod GiftCard
                    ]
                    [ text_ "Gift Card" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == StoredValue then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod StoredValue
                    ]
                    [ text_ "Stored Value" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == Mixed then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod Mixed
                    ]
                    [ text_ "Split" ],
                  D.div
                    [ DA.klass $ paymentMethodValue <#> \method ->
                        "payment-method" <> if method == Other "" then " active" else ""
                    , DL.click_ \_ -> setPaymentMethod (Other "")
                    ]
                    [ text_ "Other" ]
                ],
              
              -- Payment amount inputs (only show when payment method is selected)
              D.div
                [ DA.klass_ "tx-payment-inputs mt-4" ]
                [
                  D.div
                    [ DA.klass_ "tx-grid grid-cols-2 gap-2 mb-2" ]
                    [
                      D.label [ DA.klass_ "tx-col-span-1" ] [ text_ "Amount:" ],
                      D.input
                        [ DA.klass_ "tx-col-span-1 form-input p-1 border rounded"
                        , DA.xtype_ "text"
                        , DA.value paymentAmountValue -- Use value instead of value_
                        , DL.input_ \evt -> do
                            for_ (target evt >>= Input.fromEventTarget) \el -> do
                              value <- Input.value el
                              setPaymentAmount value
                        ]
                        []
                    ],
                  
                  -- Show tendered amount only for cash payments
                  paymentMethodValue <#~> \method ->
                    if method == Cash then
                      D.div
                        [ DA.klass_ "tx-grid grid-cols-2 gap-2" ]
                        [
                          D.label [ DA.klass_ "tx-col-span-1" ] [ text_ "Tendered:" ],
                          D.input
                            [ DA.klass_ "tx-col-span-1 form-input p-1 border rounded"
                            , DA.xtype_ "text"
                            , DA.value tenderedAmountValue -- Use value instead of value_
                            , DL.input_ \evt -> do
                                for_ (target evt >>= Input.fromEventTarget) \el -> do
                                  value <- Input.value el
                                  setTenderedAmount value
                            ]
                            []
                        ]
                    else
                      D.div_ []
                ],
              
              -- Display existing payments
              D.div
                [ DA.klass_ "tx-existing-payments mt-2" ]
                [ paymentsValue <#~> \payments ->
                    if null payments 
                    then D.div_ []
                    else D.div
                      [ DA.klass_ "tx-border-t pt-2 mb-2" ]
                      [ 
                        D.div [ DA.klass_ "tx-font-semibold mb-1" ] [ text_ "Current Payments:" ],
                        D.div_ (payments <#> \(PaymentTransaction p) ->
                          D.div
                            [ DA.klass_ "tx-flex justify-between py-1" ]
                            [
                              D.div_ [ text_ (show p.method) ],
                              D.div_ [ text_ (formatMoney' p.amount) ],
                              D.button
                                [ DA.klass_ "tx-text-red-600 text-sm"
                                , runOn DL.click $ (\currPayments -> do
                                    let updatedPayments = filter (\(PaymentTransaction pay) -> 
                                                            pay.id /= p.id) currPayments
                                    setPayments updatedPayments
                                  ) <$> paymentsValue
                                ]
                                [ text_ "✕" ]
                            ]
                        )
                      ]
                ],
              
              -- Payment totals
              D.div
                [ DA.klass_ "tx-payment-summary mb-4 pt-2" ]
                [
                  (Tuple <$> totalValue <*> paymentsValue) <#~> \(Tuple total payments) ->
                    let
                      paymentTotal = foldl (\acc (PaymentTransaction p) -> 
                                      acc + toDiscrete p.amount) (Discrete 0) payments
                      remaining = total - paymentTotal
                    in
                      D.div
                        [ DA.klass_ "tx-flex justify-between font-semibold" ]
                        [
                          D.div_ [ text_ "Remaining:" ],
                          D.div
                            [ DA.klass_ if remaining <= Discrete 0 then "text-green-600" else "text-red-600" ]
                            [ text_ (formatMoney' (fromDiscrete' (max (Discrete 0) remaining))) ]
                        ]
                ],
              
              -- Add payment button
              D.button
                [ DA.klass_ (F.buttonClass "blue" <> " mb-4 w-full")
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

                              paymentAmount = fromDiscrete' (Discrete (Int.floor (amount * 100.0)))
                              paymentTendered = fromDiscrete' (Discrete (Int.floor (tenderedAmount * 100.0)))
                              change = if toDiscrete paymentTendered > toDiscrete paymentAmount 
                                      then fromDiscrete' (toDiscrete paymentTendered - toDiscrete paymentAmount)
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
                    ) <$> paymentAmountValue <*> tenderedAmountValue <*> paymentMethodValue <*> paymentsValue
                ]
                [ text_ "Add Payment" ],
              
              -- Payment action buttons
              D.div
                [ DA.klass_ "tx-payment-actions" ]
                [
                  D.button
                    [ DA.klass_ "tx-action-btn cancel-btn"
                    , DL.click_ \_ -> do
                        setItems []
                        setPayments []
                        setSubtotal (Discrete 0)
                        setDiscountTotal (Discrete 0)
                        setTaxTotal (Discrete 0)
                        setTotal (Discrete 0)
                        setStatusMessage "Transaction cleared"
                    ]
                    [ text_ "Cancel Sale" ],
                  D.button
                    [ DA.klass_ "tx-action-btn checkout-btn"
                    , DA.disabled $ isProcessingValue <#> \isProcessing ->
                        if isProcessing then "true" else ""
                    , runOn DL.click $
                        (\currItems currPayments currTotal empId regId locId discTotal taxTotal ->
                          do
                            if null currItems then do
                              setStatusMessage "Cannot complete: No items in transaction"
                            else do
                              let
                                paymentTotal = foldl (\acc (PaymentTransaction p) -> 
                                                acc + toDiscrete p.amount) (Discrete 0) currPayments
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
                                    updatedItems = map (\(TransactionItem item) ->
                                                    TransactionItem (item { transactionId = transactionId })) currItems

                                    updatedPayments = map (\(PaymentTransaction payment) ->
                                                      PaymentTransaction (payment { transactionId = transactionId })) currPayments

                                    employeeUUID = parseUUID empId
                                    registerUUID = parseUUID regId
                                    locationUUID = parseUUID locId

                                  case Tuple (Tuple employeeUUID registerUUID) locationUUID of
                                    Tuple (Tuple (Just empId') (Just regId')) (Just locId') -> do
                                      liftEffect $ Console.log $ "Creating transaction with ID: " <> show transactionId
                                      
                                      let
                                        transaction = Transaction
                                          { id: transactionId
                                          , status: Completed
                                          , created: toDateTime currentTime
                                          , completed: Just curTime
                                          , customer: Nothing
                                          , employee: empId'
                                          , register: regId'
                                          , location: locId'
                                          , items: updatedItems
                                          , payments: updatedPayments
                                          , subtotal: fromDiscrete' (currTotal - taxTotal + discTotal)
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
                        ) <$> itemsValue <*> paymentsValue <*> totalValue <*> employeeValue <*> registerIdValue <*> locationIdValue <*> discountTotalValue <*> taxTotalValue
                    ]
                    [ totalValue <#~> \totalVal -> 
                        text_ ("Process Payment " <> formatMoney' (fromDiscrete' totalVal))
                    ]
                ]
            ]
        ],
      
      -- Status message (placed outside any other container as a floating element)
      statusMessageValue <#~> \msg ->
        if msg == "" then D.div_ []
        else D.div
          [ DA.klass_ "tx-status-message" ]
          [ text_ msg ]
    ]

getInventoryItems :: forall a. a -> Effect (Array MenuItem)
getInventoryItems _ = pure []