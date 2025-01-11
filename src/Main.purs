module Main where

import Prelude
-- import MenuLiveView (runLiveView)
import CreateItem (createItem)

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"
  -- runLiveView
  createItem