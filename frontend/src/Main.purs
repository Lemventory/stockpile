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
-- editItem "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"