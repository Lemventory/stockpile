module FileSystem where

import Prelude
import Effect (Effect)
import Control.Promise (Promise)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect.Class.Console (log)

foreign import readLocalFileImpl :: String -> Effect (Promise ArrayBuffer)
foreign import writeLocalFileImpl :: String -> String -> Effect (Promise Unit)

readLocalSystemFile :: String -> Effect (Promise ArrayBuffer)
readLocalSystemFile filename = do
  log $ "PureScript: Reading file " <> filename
  readLocalFileImpl filename

writeLocalSystemFile :: String -> String -> Effect (Promise Unit)
writeLocalSystemFile filename content = do
  log $ "PureScript: Writing to file " <> filename
  writeLocalFileImpl filename content