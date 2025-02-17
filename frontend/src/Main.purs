module Main where

import Prelude
import MenuLiveView (runLiveView)
-- import CreateItem (createItem)
-- import EditItem (editItem)

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"
  runLiveView
-- createItem  
-- editItem "8eacb499-76e6-42cb-aa05-103713dd2bd6"