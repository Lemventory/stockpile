module AddItem where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.UIEvent.KeyboardEvent (toEvent)

type ValidationRule = String -> Boolean

type FieldConfig = 
  { label :: String
  , placeholder :: String
  , validation :: ValidationRule
  , errorMessage :: String
  }

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

nonEmpty :: ValidationRule
nonEmpty = (_ /= "")

app :: Effect Unit
app = void $ runInBody Deku.do
  setField1 /\ field1Event <- useState'
  setValid1 /\ valid1Event <- useState (Nothing :: Maybe Boolean)

  let
    field1Config = 
      { label: "Field 1"
      , placeholder: "Enter text"
      , validation: nonEmpty
      , errorMessage: "Field cannot be empty"
      }

    makeField config setValue setValid validEvent = 
      D.div_
        [ D.input
            [ DA.placeholder_ config.placeholder
            , DL.keyup_ \evt -> do
                for_ 
                  ((target >=> fromEventTarget) (toEvent evt))
                  \inputElement -> do
                    v <- value inputElement
                    setValue v
                    setValid (Just (config.validation v))
            , DA.value_ ""
            , DA.klass_ inputKls
            ]
            []
        , D.div
            [ DA.klass_ "text-red-500 text-sm mt-1" ]
            [ text $ map 
                (maybe "" \isValid -> if isValid then "" else config.errorMessage) 
                validEvent
            ]
        ]

    -- Convert Maybe Boolean to "true"/"false" string for disabled attribute
    isButtonDisabled = map (fromMaybe true >>> (\b -> if b then "false" else "true")) valid1Event

  D.div_
    [ makeField field1Config setField1 setValid1 valid1Event
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled isButtonDisabled
        , DL.click_ \_ -> do
            setField1 ""
            setValid1 Nothing
        ]
        [ text_ "Submit" ]
    , D.div_ 
        [ text $ map (\val -> "Current value: " <> val) field1Event ]
    ]