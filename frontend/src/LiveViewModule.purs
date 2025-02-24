module LiveViewModule
  ( runLiveView
  ) where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import MenuLiveView as MenuLiveView

-- Legacy module kept for backward compatibility
runLiveView :: Effect Unit
runLiveView = do
  Console.log "LiveViewModule is deprecated. Using MenuLiveView instead."
  MenuLiveView.runLiveView