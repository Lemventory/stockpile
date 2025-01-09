module Test.Main where

import Prelude

import Data.Foldable (traverse_)
import Data.List.Lazy (replicateM)
import Effect (Effect)
import Effect.Class.Console (log)
-- import MenuLiveView (runLiveView)
import UUID (genUUID, uuidToString)

main :: Effect Unit
main = do
  log "Generating 200 UUIDs:"
  
  -- Generate 200 UUIDs using replicateM
  uuids <- replicateM 200 genUUID

  -- Print each UUID to the console
  traverse_ (log <<< uuidToString) uuids
  -- log "Do those look random to you?"
  -- log "."
  -- log ".."
  -- log "..."
  -- log "...."
  -- log "....."
  -- log "......"    
  -- log "Starting Live Display"
  -- runLiveView