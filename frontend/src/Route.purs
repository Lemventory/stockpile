module Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import FRP.Poll (Poll)
import Routing.Duplex (RouteDuplex', root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

-- Define our routes
data Route = LiveView | Create | Edit String

derive instance Eq Route
derive instance Ord Route
derive instance genericRoute :: Generic Route _

instance Show Route where
  show = genericShow

-- Define route parsing
route :: RouteDuplex' Route
route = root $ G.sum
  { "LiveView": G.noArgs
  , "Create": "create" / G.noArgs
  , "Edit": "edit" / (string segment)
  }

-- Navigation component 
nav :: Poll Route -> Nut
nav currentRoute = D.nav [ DA.klass_ "navbar navbar-light" ]
  [ D.div [ DA.klass_ "container" ]
      [ D.a
          [ DA.klass_ "navbar-brand"
          , DA.href_ "/#/"
          ]
          [ text_ "Inventory" ]
      , D.ul
          [ DA.klass_ "nav navbar-nav pull-xs-right" ]
          [ navItem LiveView "/#/" "LiveView" currentRoute
          , navItem Create "/#/create" "Create Item" currentRoute
          ]
      ]
  ]

-- Helper for navigation items
navItem :: Route -> String -> String -> Poll Route -> Nut
navItem thisRoute href label currentRoute =
  D.li
    [ DA.klass_ "nav-item" ]
    [ D.a
        [ DA.href_ href
        , DA.klass $ currentRoute <#> \r ->
            "nav-link" <> if r == thisRoute then " active" else ""
        ]
        [ text_ label ]
    ]