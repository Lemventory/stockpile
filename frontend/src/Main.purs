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
import Effect.Class.Console as Console
import FRP.Poll as Poll
import MenuLiveView (runLiveView)
import Route (Route(..), nav, route)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

testItemUUID :: String
testItemUUID = "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"

routeToComponent :: Route -> Nut
routeToComponent = case _ of
  LiveView -> runLiveView
  Create -> createItem
  Edit uuid -> editItem (if uuid == "test" then testItemUUID else uuid)

main :: Effect Unit
main = do
  Console.log "Application starting"

  currentRoute <- liftST Poll.create

  void $ matchesWith (parse route) \_ r -> do
    Console.log $ "Route changed to: " <> show r
    currentRoute.push $ Tuple r (routeToComponent r)

  void $ runInBody
    ( fixed
        [ nav (fst <$> currentRoute.poll)
        , D.div_ [ cycle (snd <$> currentRoute.poll) ]
        ]
    )

  -- Initialize with the LiveView route
  currentRoute.push $ Tuple LiveView (routeToComponent LiveView)