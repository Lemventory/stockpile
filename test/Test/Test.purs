module Test.Main where

import MenuLiveView (runLiveView)
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"
  runLiveView