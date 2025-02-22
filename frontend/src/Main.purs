module Main where

import Prelude

import Control.Monad.ST.Class (liftST)
import CreateItem (createItem)
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.Hooks (cycle)
import Deku.Toplevel (runInBody)
import EditItem (editItem)
import Effect (Effect)
import FRP.Poll as Poll
import MenuLiveView (runLiveView)
import Route (Route(..), nav, route)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

-- Hardcoded test UUID for edit
testItemUUID :: String
testItemUUID = "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"

routeToComponent :: Route -> Nut
routeToComponent = case _ of
  LiveView -> runLiveView
  Create -> createItem
  Edit uuid -> editItem (if uuid == "test" then testItemUUID else uuid)

main :: Effect Unit
main = do
  currentRoute <- liftST Poll.create

  -- Set up routing
  _ <- matchesWith (parse route) \_ r ->
    currentRoute.push $ Tuple r (routeToComponent r)

  -- Create and run the app layout
  _ <- runInBody
    ( fixed
        [ nav (fst <$> currentRoute.poll)
        , D.div_ [ cycle (snd <$> currentRoute.poll) ]
        ]
    )

  -- Initialize with home route and redirect to test item for development
  currentRoute.push $ Tuple (Edit "test") (editItem testItemUUID)