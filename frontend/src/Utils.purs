module Utils where

import Prelude

import Data.Int (floor, toNumber)
import Effect (Effect)
import Effect.Random (random)
import Data.Array (catMaybes, filter, range, replicate, (!!))
import Data.String (Pattern(..), joinWith, length, split, take, trim)
import Data.Array (length) as Array
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (length) as String

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
  in
    joinWith "" padding <> str

parseCommaList :: String -> Array String
parseCommaList str =
  if str == "" then []
  else
    str
      # split (Pattern ",")
      # map trim
      # filter (_ /= "")

formatDollarAmount :: String -> String
formatDollarAmount str =
  if str == "" then ""
  else case Number.fromString str of
    Just n ->
      let
        fixed = show n
        parts = split (Pattern ".") fixed
      in
        case Array.length parts of
          1 -> fixed <> ".00"
          2 ->
            let
              decimals = fromMaybe "" $ parts !! 1
            in
              if String.length decimals >= 2 then fromMaybe "" (parts !! 0) <> "." <> take 2 decimals
              else fromMaybe "" (parts !! 0) <> "." <> decimals <> "0"
          _ -> str
    Nothing -> str

getAllEnumValues :: ∀ a. BoundedEnum a => Bounded a => Array a
getAllEnumValues = catMaybes $ map toEnum $ range 0 (fromEnum (top :: a))