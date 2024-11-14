module AddItem where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Hooks (useDynAtBeginning, useRef, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.HTML.Window (alert)
import Web.UIEvent.KeyboardEvent (code, toEvent)

-- Input styling
inputKls :: String
inputKls =
  """rounded-md border-gray-300 shadow-sm
     border-2 mr-2 border-solid
     focus:border-indigo-500 focus:ring-indigo-500
     sm:text-sm"""

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color)
    """mb-3 inline-flex items-center rounded-md
       border border-transparent bg-COLOR-600 px-3 py-2
       text-sm font-medium leading-4 text-white shadow-sm
       hover:bg-COLOR-700 focus:outline-none focus:ring-2
       focus:ring-COLOR-500 focus:ring-offset-2"""

-- Main app with multiple fields
app :: Effect Unit
app = void $ runInBody Deku.do
  -- Initialize state for each input field
  setSku /\ sku <- useState'
  setBrand /\ brand <- useState'
  setCategory /\ category <- useState'
  setItem /\ item <- useState'

  -- Create references for each input field
  skuRef <- useRef Nothing (Just <$> sku)
  brandRef <- useRef Nothing (Just <$> brand)
  categoryRef <- useRef Nothing (Just <$> category)

  -- Guard function to check if a field is empty
  let guardAgainstEmpty e = do
        v <- value e
        if v == ""
          then window >>= alert "All fields must be filled out!"
          else setItem v

      -- Handle button click to validate all fields
      checkInputs :: Effect Unit
      checkInputs = do
        traverse_ (\ref -> ref >>= traverse_ guardAgainstEmpty) [skuRef, brandRef, categoryRef]

      -- Top form with multiple input fields
      top =
        D.div_
          [ D.input
              [ DA.placeholder_ "SKU"
              , DL.keyup_ \evt -> do
                  when (code evt == "Enter") $
                    for_
                      ((target >=> fromEventTarget) (toEvent evt))
                      guardAgainstEmpty
              , Self.selfT_ setSku
              , DA.klass_ inputKls
              ]
              []
          , D.input
              [ DA.placeholder_ "Brand"
              , DL.keyup_ \evt -> do
                  when (code evt == "Enter") $
                    for_
                      ((target >=> fromEventTarget) (toEvent evt))
                      guardAgainstEmpty
              , Self.selfT_ setBrand
              , DA.klass_ inputKls
              ]
              []
          , D.input
              [ DA.placeholder_ "Category"
              , DL.keyup_ \evt -> do
                  when (code evt == "Enter") $
                    for_
                      ((target >=> fromEventTarget) (toEvent evt))
                      guardAgainstEmpty
              , Self.selfT_ setCategory
              , DA.klass_ inputKls
              ]
              []
          , D.button
              [ DL.click_ \_ -> checkInputs
              , DA.klass_ $ buttonClass "green"
              ]
              [ text_ "Add" ]
          ]

  -- Display area
  D.div_
    [ top
    , Deku.do
        { value: t } <- useDynAtBeginning item
        D.div_ [ text_ t ]
    ]