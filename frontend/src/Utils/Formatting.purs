module Utils.Formatting where

import Prelude

import Data.Array (catMaybes, filter, range, replicate, (!!))
import Data.Array (length, uncons) as Array
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (floor, fromString, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), joinWith, replace, split, take, toLower, trim)
import Data.String.Pattern (Replacement(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Random (random)
import Types.Inventory (ItemCategory, MenuItem(..), Species, StrainLineage(..)) 
import Config.LiveView (LiveViewConfig, SortField(..), SortOrder(..))
import Types.UUID (UUID(..))
import Data.String as String
import Data.String.Regex (regex, replace) as Regex
import Data.String.Regex.Flags (global) as Regex
import Partial.Unsafe (unsafePartial)

parseUUID :: String -> Maybe UUID
parseUUID str =
  case
    regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
      noFlags
    of
    Left _ -> Nothing
    Right r ->
      if test r str then Just $ UUID str
      else Nothing

uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"

generateClassName
  :: { category :: ItemCategory, subcategory :: String, species :: Species }
  -> String
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
              if String.length decimals >= 2 then fromMaybe "" (parts !! 0)
                <> "."
                <> take 2 decimals
              else fromMaybe "" (parts !! 0) <> "." <> decimals <> "0"
          _ -> str
    Nothing -> str

getAllEnumValues :: ∀ a. BoundedEnum a => Bounded a => Array a
getAllEnumValues = catMaybes $ map toEnum $ range 0 (fromEnum (top :: a))

compareMenuItems :: LiveViewConfig -> MenuItem -> MenuItem -> Ordering
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    StrainLineage meta1 = item1.strain_lineage
    StrainLineage meta2 = item2.strain_lineage

    compareByField :: Tuple SortField SortOrder -> Ordering
    compareByField (sortField /\ sortOrder) =
      let
        fieldComparison = case sortField of
          SortByOrder -> compare item1.sort item2.sort
          SortByName -> compare item1.name item2.name
          SortByCategory -> compare item1.category item2.category
          SortBySubCategory -> compare item1.subcategory item2.subcategory
          SortBySpecies -> compare meta1.species meta2.species
          SortBySKU -> compare item1.sku item2.sku
          SortByPrice -> compare item1.price item2.price
          SortByQuantity -> compare item1.quantity item2.quantity
      in
        case sortOrder of
          Ascending -> fieldComparison
          Descending -> invertOrdering fieldComparison

    compareWithPriority :: Array (Tuple SortField SortOrder) -> Ordering
    compareWithPriority priorities = case Array.uncons priorities of
      Nothing -> EQ
      Just { head: priority, tail: rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

summarizeLongText :: String -> String
summarizeLongText desc =
  let
    noLinebreaks = String.replace (String.Pattern "\n") (String.Replacement " ")
      desc

    condensedSpaces = unsafePartial case Regex.regex "\\s+" Regex.global of
      Right r -> Regex.replace r " " noLinebreaks

    maxLength = 100
    truncated =
      if String.length condensedSpaces > maxLength then
        String.take maxLength condensedSpaces <> "..."
      else condensedSpaces
  in
    truncated