module Route where

import Prelude hiding ((/))

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

data Route 
  = LiveView 
  | Create 
  | Edit String 
  | Delete String 
  | CreateTransaction
  | TransactionHistory
  | InventorySelector  -- Added new route

derive instance Eq Route
derive instance Ord Route
derive instance genericRoute :: Generic Route _

instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ G.sum
  { "LiveView": G.noArgs
  , "Create": "create" / G.noArgs
  , "Edit": "edit" / (string segment)
  , "Delete": "delete" / (string segment)
  , "CreateTransaction": "transaction" / "create" / G.noArgs
  , "TransactionHistory": "transaction" / "history" / G.noArgs
  , "InventorySelector": "inventory" / "selector" / G.noArgs  -- Added route path
  }

nav :: Poll Route -> Nut
nav currentRoute = D.nav [ DA.klass_ "navbar navbar-light" ]
  [ D.div [ DA.klass_ "container" ]
      [ D.div
          [ DA.klass_ "nav navbar-nav pull-xs-right d-flex" ]
          [ navItem LiveView "/#/" "LiveView" currentRoute
          , navItem Create "/#/create" "Create Item" currentRoute
          , navItem (Edit "test") "/#/edit/test" "Edit Test Item" currentRoute
          , D.div [ DA.klass_ "border-l mx-2 h-6" ] []  -- Divider
          , navItem CreateTransaction "/#/transaction/create" "New Transaction" currentRoute
          , navItem TransactionHistory "/#/transaction/history" "Transaction History" currentRoute
          , D.div [ DA.klass_ "border-l mx-2 h-6" ] []  -- Divider
          , navItem InventorySelector "/#/inventory/selector" "Inventory Selector" currentRoute
          ]
      ]
  ]

navItem :: Route -> String -> String -> Poll Route -> Nut
navItem thisRoute href label currentRoute =
  D.div
    [ DA.klass_ "nav-item mx-2" ]
    [ D.a
        [ DA.href_ href
        , DA.klass $ currentRoute <#> \r ->
            "nav-link" <> if r == thisRoute then " active" else ""
        ]
        [ text_ label ]
    ]