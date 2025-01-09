module Form where

import Prelude

import Types (DropdownConfig, FieldConfig)
import Utils (getAllEnumValues, parseCommaList)

import Data.Array ((:))
import Data.Enum (class BoundedEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Effect (Effect)
import FRP.Poll (Poll)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

makeField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeField config setValue setValid validEvent = 
  D.div_
    [ D.div 
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , if config.label == "Description"
            then D.textarea
                [ DA.placeholder_ config.placeholder
                , DA.cols_ "40" 
                , DA.rows_ "4"
                , DL.keyup_ \evt -> do
                    let targetEvent = toEvent evt
                    for_ 
                      (target targetEvent >>= Input.fromEventTarget)
                      \inputElement -> do
                        v <- Input.value inputElement
                        let formatted = config.formatInput v
                        setValue formatted
                        setValid (Just (config.validation formatted))
                , DL.input_ \evt -> do
                    for_ 
                      (target evt >>= Input.fromEventTarget)
                      \inputElement -> do
                        v <- Input.value inputElement
                        let formatted = config.formatInput v
                        setValue formatted
                        setValid (Just (config.validation formatted))
                , DA.klass_ (inputKls <> " resize-y")
                ]
                [ text_ config.defaultValue ]
            else D.input
                [ DA.placeholder_ config.placeholder
                , DA.value_ config.defaultValue
                , DL.keyup_ \evt -> do
                    let targetEvent = toEvent evt
                    for_ 
                      (target targetEvent >>= Input.fromEventTarget)
                      \inputElement -> do
                        v <- Input.value inputElement
                        let formatted = config.formatInput v
                        setValue formatted
                        setValid (Just (config.validation formatted))
                , DL.input_ \evt -> do
                    for_ 
                      (target evt >>= Input.fromEventTarget)
                      \inputElement -> do
                        v <- Input.value inputElement
                        let formatted = config.formatInput v
                        setValue formatted
                        setValid (Just (config.validation formatted))
                , DA.klass_ inputKls
                ]
                []
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text (map (\mValid -> case mValid of 
                Just false -> config.errorMessage
                _ -> "") validEvent)
            ]
        ]
    ]

makeDropdown :: DropdownConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeDropdown config setValue setValid validEvent = 
  D.div_
    [ D.div 
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , D.select
            [ DA.klass_ inputKls
            , DL.change_ \evt -> do
                for_ 
                  (target evt >>= Select.fromEventTarget)
                  \selectElement -> do
                    v <- Select.value selectElement
                    setValue v
                    setValid (Just (v /= ""))
            ]
            (config.options <#> \opt ->
              D.option
                [ DA.value_ opt.value ]
                [ text_ opt.label ]
            )
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text (map (\mValid -> case mValid of 
                Just false -> "Please select an option"
                _ -> "") validEvent)
            ]
        ]
    ]

makeEnumDropdown :: ∀ a. BoundedEnum a => Bounded a => Show a => 
  { label :: String, enumType :: a } -> DropdownConfig
makeEnumDropdown { label } = 
  { label
  , options: 
      { value: "", label: "Select..." } :
      map (\val -> { value: show val, label: show val }) 
          (getAllEnumValues :: Array a)
  , defaultValue: ""
  }

makeArrayField :: String -> (Array String -> Effect Unit) -> Nut
makeArrayField label setValue = 
  D.div_
    [ D.div 
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ label ]
        , D.input
            [ DA.placeholder_ "Add items (comma-separated)"
            , DL.keyup_ \evt -> do
                for_ 
                  ((target >=> Input.fromEventTarget) (toEvent evt))
                  \inputElement -> do
                    v <- Input.value inputElement
                    setValue $ parseCommaList v
            , DA.klass_ inputKls
            ]
            []
        ]
    ]

-- | Styling
inputKls :: String
inputKls = """
  rounded-md border-gray-300 shadow-sm
  border-2 mr-2 border-solid
  focus:border-indigo-500 focus:ring-indigo-500
  sm:text-sm
"""

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color) """
    mb-3 inline-flex items-center rounded-md
    border border-transparent bg-COLOR-600 px-3 py-2
    text-sm font-medium leading-4 text-white shadow-sm
    hover:bg-COLOR-700 focus:outline-none focus:ring-2
    focus:ring-COLOR-500 focus:ring-offset-2
"""