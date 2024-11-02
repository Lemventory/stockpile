module Inventory (app) where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Hooks (useDyn, useRef, useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event.Class ((<|*>))
import Parsing (runParser)
import Parsing.String.Basic (intDecimal, number)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromEventTarget, value)
import Web.HTML.Window (alert)
import Web.UIEvent.KeyboardEvent (code, toEvent)


-- Input styling
inputKls :: String
inputKls =
  """rounded-md border-gray-300 shadow-sm border-2 mr-2 border-solid
  focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm"""

buttonClass :: String -> String
buttonClass color =
  "mb-3 inline-flex items-center rounded-md border border-transparent bg-" <> color <>
  "-600 px-3 py-2 text-sm font-medium leading-4 text-white shadow-sm hover:bg-" <> color <>
  "-700 focus:outline-none focus:ring-2 focus:ring-" <> color <> "-500 focus:ring-offset-2"

-- Minimal handler to display an alert if input is empty
guardAgainstEmpty :: (String -> Effect Unit) -> HTMLInputElement -> Effect Unit
guardAgainstEmpty setItem inputEl = do
  text <- value inputEl
  if text == ""
    then window >>= alert "Item cannot be empty"
    else setItem text

-- Render input fields with event handling
renderInputs :: { pos :: Int, setPos :: Int -> Effect Unit, item :: String, setItem :: String -> Effect Unit, setInput :: HTMLInputElement -> Effect Unit } -> Nut
renderInputs { pos, setPos, item, setItem, setInput } = 
  D.div_
    [ D.input
        [ DA.value_ item
        , DL.input \evt -> case fromEventTarget evt of
            Just target -> guardAgainstEmpty setItem target
            Nothing -> pure unit
        , Self.selfT_ setInput
        , DA.klass_ inputKls
        ]
        []
    , D.input
        [ DA.klass_ inputKls
        , DA.xtypeNumber
        , DA.min_ "0"
        , DA.value_ (show pos)
        , DL.input \evt -> case fromEventTarget evt of
            Just target -> do
              text <- value target
              setPos (parseInt text)
            Nothing -> pure unit
        ]
        []
    , D.button
        [ DL.click_ \_ -> alert "Item added successfully"
        , DA.klass_ $ buttonClass "green"
        ]
        [ text_ "Add" ]
    ]

-- Parse integer from input text
parseInt :: String -> Int
parseInt str = case runParser str intDecimal of
  Right n -> n
  Left _ -> 0

-- Main app function
app :: (Nut -> Effect (Effect Unit)) -> Effect (Effect Unit)
app runExample = runExample $ Deku.do
  setPos /\ pos <- useState 0
  setItem /\ item <- useState ""
  setInput /\ input <- useState' (pure unit)

  -- Main UI rendering
  D.div_
    [ renderInputs { pos, setPos, item, setItem, setInput }
    , Deku.do
        { value: t } <- useDyn
          (Tuple <$> (pure <$> pos) <|*> item)
        D.div_ [ text_ ("Position: " <> show pos <> ", Item: " <> t) ]
    ]