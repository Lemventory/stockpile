module AddItem (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Array (intercalate, null)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut, useRant)
import Deku.DOM (HTMLInputElement)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners (valueOn_)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect (useState)
import Deku.Hooks (useDynAtBeginning, useRef, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (Event, makeEvent, sampleOnRight, subscribe)
import FRP.Poll (Poll(..), sample)
import Parsing (runParser)
import Parsing.String.Basic (intDecimal, number)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as HTMLInput
import Web.HTML.Window (alert)

-- input styling
inputClass :: String
inputClass = "border border-gray-300 rounded-md p-2 mb-2"

-- fieldset helper
fieldset
  :: Poll String
  -> String
  -> (String -> Effect Unit)
  -> Nut
fieldset value placeholder pusher = D.fieldset [ DA.klass_ "form-group" ]
  [ D.input
      [ DA.klass_ inputClass
      , DA.placeholder_ placeholder
      , DA.value value
      , valueOn_ DL.input pusher
      ]
      []
  ]

-- field Types for String Fields
textField :: Poll String -> String -> (String -> Effect Unit) -> Nut
textField = fieldset

-- Form Rendering Function Using Fieldsets
renderForm ::
  { name :: Poll String
  , setName :: String -> Effect Unit
  , brand :: Poll String
  , setBrand :: String -> Effect Unit
  , price :: Poll String
  , setPrice :: String -> Effect Unit
  , quantity :: Poll String
  , setQuantity :: String -> Effect Unit
  , description :: Poll String
  , setDescription :: String -> Effect Unit
  , handleSubmit :: Aff Unit
  } -> Nut
renderForm { name, setName, brand, setBrand, price, setPrice, quantity, setQuantity, description, setDescription, handleSubmit } = D.div_
  [ textField name "Name" setName
  , textField brand "Brand" setBrand
  , textField price "Price" setPrice
  , textField quantity "Quantity" setQuantity
  , textField description "Description" setDescription
  , D.button
      [ DA.klass_ "p-2 bg-green-500 text-white rounded-md"
      , runOn_ DL.click (launchAff_ handleSubmit)
      ]
      [ text_ "Create Item" ]
  ]

-- Main App Entry Point
app :: Effect Unit
app = do
  -- Initialize state for each input field as Poll String
  setName /\ name <- useState ""
  setBrand /\ brand <- useState ""
  setPrice /\ price <- useState ""
  setQuantity /\ quantity <- useState ""
  setDescription /\ description <- useState ""

  -- Result display state
  setResult /\ result <- useState ""

  -- handleSubmit function with asynchronous logging and result update
  let handleSubmit :: Aff Unit
      handleSubmit = do
        -- Extract the current values from each field (asynchronous Aff context)
        currentName <- useRant name
        currentBrand <- useRant brand
        currentPrice <- useRant price
        currentQuantity <- useRant quantity
        currentDescription <- useRant description

        -- Log the submission values
        let logMessage = "Submitted values: " <> intercalate ", " [currentName, currentBrand, currentPrice, currentQuantity, currentDescription]
        liftEffect $ log logMessage

        -- Update result state to confirm submission
        liftEffect $ setResult "Item submission logged successfully!"

  -- Render form and result display
  void $ runInBody $ Deku.do
    resultDisplay <- useDynAtBeginning result \res ->
      D.div_ [ text_ res ]

    form <- renderForm
      { name: name
      , setName
      , brand: brand
      , setBrand
      , price: price
      , setPrice
      , quantity: quantity
      , setQuantity
      , description: description
      , setDescription
      , handleSubmit
      }

    D.div_ [ form, resultDisplay ]