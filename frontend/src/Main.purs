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
-- editItem "1ef71137-37cd-4e8c-91aa-7fada016a7cc"