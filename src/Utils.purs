module Utils where

import Prelude
import Data.Array (replicate)
import Data.Int (floor, toNumber)
import Data.String (joinWith, length)
import Effect (Effect)
import Effect.Random (random)



-- | Generate a random integer in a given range (inclusive)
randomInt :: Int -> Int -> Effect Int
randomInt min max = do
  r <- random
  pure $ floor $ r * toNumber (max - min + 1) + toNumber min

padStart :: Int -> String -> String
padStart targetLength str =
  let
    paddingLength = max 0 (targetLength - length str) -- Ensure no negative padding
    padding = replicate paddingLength "0" -- Create an Array String
  in joinWith "" padding <> str


