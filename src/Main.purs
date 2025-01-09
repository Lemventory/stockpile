module Main where

import Prelude
import CreateItem (createItem)

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"
  createItem