module Utils where

import Prelude

import Data.Array (catMaybes, filter, range, replicate, (!!))
import Data.Array (length) as Array
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (floor, fromString, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), joinWith, replace, split, take, toLower, trim)
import Data.String (length) as String
import Effect (Effect)
import Effect.Random (random)
import Data.Either (Either(..))
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits ((.|.))

import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Pattern (Replacement(..))
import Types (ItemCategory, Species)
import Types.UUID (UUID(..))

parseUUID :: String -> Maybe UUID
parseUUID str =
  case regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" noFlags of
    Left _ -> Nothing
    Right r ->
      if test r str then Just $ UUID str
      else Nothing

uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"

-- | Generate a UUID v4
genUUID :: Effect UUID
genUUID = do
  -- Generate random 16-bit integers for smaller chunks
  r1 <- randomInt 0 0xFFFF -- First half of time_low
  r2 <- randomInt 0 0xFFFF -- Second half of time_low
  r3 <- randomInt 0 0xFFFF -- time_mid
  r4 <- randomInt 0 0x0FFF -- time_hi (12 bits for randomness)
  r5 <- randomInt 0 0x3FFF -- clock_seq (14 bits for randomness)
  r6 <- randomInt 0 0xFFFF -- First part of node
  r7 <- randomInt 0 0xFFFF -- Second part of node
  r8 <- randomInt 0 0xFFFF -- Third part of node

  -- Set the version (4) and variant (10)
  let
    versioned = r4 .|. 0x4000 -- Set version to 4 (binary OR with 0100 0000 0000 0000)
    variant = r5 .|. 0x8000 -- Set variant to 10xx (binary OR with 1000 0000 0000 0000)

  -- Convert to hex and pad as needed
  let
    hex1 = padStart 4 (toHex r1) <> padStart 4 (toHex r2) -- time_low
    hex2 = padStart 4 (toHex r3) -- time_mid
    hex3 = padStart 4 (toHex versioned) -- time_hi_and_version
    hex4 = padStart 4 (toHex variant) -- clock_seq
    hex5 = padStart 4 (toHex r6) <> padStart 4 (toHex r7) <> padStart 4 (toHex r8) -- node
    uuid = joinWith "-" [ hex1, hex2, hex3, hex4, hex5 ]

  pure $ UUID uuid
  where
  toHex = toStringAs hexadecimal

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species)
    <> " category-"
    <> toClassName (show item.category)
    <> " subcategory-"
    <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

-- | Generate a random integer in a given range (inclusive)
randomInt :: Int -> Int -> Effect Int
randomInt min max = do
  r <- random
  pure $ Int.floor $ r * Int.toNumber (max - min + 1) + Int.toNumber min

ensureNumber :: String -> String
ensureNumber str = fromMaybe "0.0" $ map show $ Number.fromString $ trim str

ensureInt :: String -> String
ensureInt str = fromMaybe "0" $ map show $ Int.fromString $ trim str

padStart :: Int -> String -> String
padStart targetLength str =
  let
    paddingLength = max 0 (targetLength - String.length str) -- Ensure no negative padding
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