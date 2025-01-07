module Types where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Array (replicate)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (floor, hexadecimal, toNumber, toStringAs)
import Data.Int.Bits ((.|.))
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, length)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Effect.Random (random)
import Foreign (Foreign, ForeignError(..), fail)
import Foreign.Index (readProp)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype ForeignRequestBody = ForeignRequestBody Foreign

data InventoryResponse
  = InventoryData Inventory
  | Message String

data QueryMode = JsonMode | HttpMode

newtype Inventory = Inventory (Array MenuItem)

data MenuItem = MenuItem
  { sort :: Int
  , sku :: UUID
  , brand :: String
  , name :: String
  , price :: Number
  , measure_unit :: String
  , per_package :: String
  , quantity :: Int
  , category :: ItemCategory
  , subcategory :: String
  , description :: String
  , tags :: Array String
  , effects :: Array String
  , strain_lineage :: StrainLineage
  }

data ItemCategory 
  = Flower 
  | PreRolls 
  | Vaporizers 
  | Edibles 
  | Drinks 
  | Concentrates 
  | Topicals 
  | Tinctures 
  | Accessories

newtype UUID = UUID String

derive instance genericMenuItem :: Generic MenuItem _
derive instance eqItemCategory :: Eq ItemCategory
derive instance ordItemCategory :: Ord ItemCategory
derive instance eqUUID :: Eq UUID
derive instance ordUUID :: Ord UUID

instance Enum ItemCategory where
  succ Flower = Just PreRolls
  succ PreRolls = Just Vaporizers
  succ Vaporizers = Just Edibles
  succ Edibles = Just Drinks
  succ Drinks = Just Concentrates
  succ Concentrates = Just Topicals
  succ Topicals = Just Tinctures
  succ Tinctures = Just Accessories
  succ Accessories = Nothing
  
  pred PreRolls = Just Flower
  pred Vaporizers = Just PreRolls
  pred Edibles = Just Vaporizers
  pred Drinks = Just Edibles
  pred Concentrates = Just Drinks
  pred Topicals = Just Concentrates
  pred Tinctures = Just Topicals
  pred Accessories = Just Tinctures
  pred Flower = Nothing

instance Bounded ItemCategory where
  bottom = Flower
  top = Accessories

instance BoundedEnum ItemCategory where
  cardinality = Cardinality 9
  fromEnum Flower = 0
  fromEnum PreRolls = 1
  fromEnum Vaporizers = 2
  fromEnum Edibles = 3
  fromEnum Drinks = 4
  fromEnum Concentrates = 5
  fromEnum Topicals = 6
  fromEnum Tinctures = 7
  fromEnum Accessories = 8
  
  toEnum 0 = Just Flower
  toEnum 1 = Just PreRolls
  toEnum 2 = Just Vaporizers
  toEnum 3 = Just Edibles
  toEnum 4 = Just Drinks
  toEnum 5 = Just Concentrates
  toEnum 6 = Just Topicals
  toEnum 7 = Just Tinctures
  toEnum 8 = Just Accessories
  toEnum _ = Nothing

instance Show ItemCategory where
  show Flower = "Flower"
  show PreRolls = "PreRolls"
  show Vaporizers = "Vaporizers"
  show Edibles = "Edibles"
  show Drinks = "Drinks"
  show Concentrates = "Concentrates"
  show Topicals = "Topicals"
  show Tinctures = "Tinctures"
  show Accessories = "Accessories"

data StrainLineage = StrainLineage
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: Species
  , dominant_tarpene :: String
  , tarpenes :: Array String
  , lineage :: Array String
  , leafly_url :: String
  , img :: String
  }

derive instance genericStrainLineage :: Generic StrainLineage _

-- | Validate and parse a string into a UUID
parseUUID :: String -> Maybe UUID
parseUUID str = 
  case regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" noFlags of
    Left _ -> Nothing
    Right r -> if test r str
               then Just $ UUID str
               else Nothing

-- | Convert UUID to String
uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

data Species 
  = Indica 
  | IndicaDominantHybrid 
  | Hybrid 
  | SativaDominantHybrid 
  | Sativa 

derive instance eqItemSpecies :: Eq Species
derive instance ordItemSpecies :: Ord Species

instance Enum Species where
  succ Indica = Just IndicaDominantHybrid
  succ IndicaDominantHybrid = Just Hybrid
  succ Hybrid = Just SativaDominantHybrid
  succ SativaDominantHybrid = Just Sativa
  succ Sativa = Nothing
  
  pred IndicaDominantHybrid = Just Indica
  pred Hybrid = Just IndicaDominantHybrid
  pred SativaDominantHybrid = Just Hybrid
  pred Sativa = Just SativaDominantHybrid
  pred Indica = Nothing

instance Bounded Species where
  bottom = Indica
  top = Sativa

instance BoundedEnum Species where
  cardinality = Cardinality 5
  fromEnum Indica = 0
  fromEnum IndicaDominantHybrid = 1
  fromEnum Hybrid = 2
  fromEnum SativaDominantHybrid = 3
  fromEnum Sativa = 4
  
  toEnum 0 = Just Indica
  toEnum 1 = Just IndicaDominantHybrid
  toEnum 2 = Just Hybrid
  toEnum 3 = Just SativaDominantHybrid
  toEnum 4 = Just Sativa
  toEnum _ = Nothing

instance Show Species where
  show Indica = "Indica"
  show IndicaDominantHybrid = "IndicaDominantHybrid"
  show Hybrid = "Hybrid"
  show SativaDominantHybrid = "SativaDominantHybrid"
  show Sativa = "Sativa"

instance showUUID :: Show UUID where
  show (UUID uuid) = uuid

-- | Empty UUID (all zeros)
emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"

-- | Generate a UUID v4
genUUID :: Effect UUID
genUUID = do
  -- Generate random 16-bit integers for smaller chunks
  r1 <- randomInt 0 0xFFFF  -- First half of time_low
  r2 <- randomInt 0 0xFFFF  -- Second half of time_low
  r3 <- randomInt 0 0xFFFF  -- time_mid
  r4 <- randomInt 0 0x0FFF  -- time_hi (12 bits for randomness)
  r5 <- randomInt 0 0x3FFF  -- clock_seq (14 bits for randomness)
  r6 <- randomInt 0 0xFFFF  -- First part of node
  r7 <- randomInt 0 0xFFFF  -- Second part of node
  r8 <- randomInt 0 0xFFFF  -- Third part of node

  -- Set the version (4) and variant (10)
  let versioned = r4 .|. 0x4000  -- Set version to 4 (binary OR with 0100 0000 0000 0000)
      variant = r5 .|. 0x8000    -- Set variant to 10xx (binary OR with 1000 0000 0000 0000)

  -- Convert to hex and pad as needed
  let hex1 = padStart 4 (toHex r1) <> padStart 4 (toHex r2)  -- time_low
      hex2 = padStart 4 (toHex r3)                           -- time_mid
      hex3 = padStart 4 (toHex versioned)                    -- time_hi_and_version
      hex4 = padStart 4 (toHex variant)                      -- clock_seq
      hex5 = padStart 4 (toHex r6) <> padStart 4 (toHex r7) <> padStart 4 (toHex r8) -- node
      uuid = joinWith "-" [hex1, hex2, hex3, hex4, hex5]

  pure $ UUID uuid
  where
    toHex = toStringAs hexadecimal

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

instance writeForeignMenuItem :: WriteForeign MenuItem where
  writeImpl (MenuItem item) = writeImpl
    { sort: item.sort
    , sku: show item.sku
    , brand: item.brand
    , name: item.name
    , price: item.price
    , measure_unit: item.measure_unit
    , per_package: item.per_package
    , quantity: item.quantity
    , category: show item.category
    , subcategory: item.subcategory
    , description: item.description
    , tags: item.tags
    , strain_lineage: item.strain_lineage
    }

instance readForeignMenuItem :: ReadForeign MenuItem where
  readImpl json = do
    sort <- readProp "sort" json >>= readImpl
    skuStr <- readProp "sku" json >>= readImpl
    sku <- case parseUUID skuStr of
      Just uuid -> pure uuid
      Nothing -> fail $ ForeignError "Invalid UUID format for sku"
    brand <- readProp "brand" json >>= readImpl
    name <- readProp "name" json >>= readImpl
    price <- readProp "price" json >>= readImpl
    measure_unit <- readProp "measure_unit" json >>= readImpl
    per_package <- readProp "per_package" json >>= readImpl
    quantity <- readProp "quantity" json >>= readImpl
    categoryStr <- readProp "category" json >>= readImpl
    category <- case categoryStr of
      "Flower" -> pure Flower
      "PreRolls" -> pure PreRolls
      "Vaporizers" -> pure Vaporizers
      "Edibles" -> pure Edibles
      "Drinks" -> pure Drinks
      "Concentrates" -> pure Concentrates
      "Topicals" -> pure Topicals
      "Tinctures" -> pure Tinctures
      "Accessories" -> pure Accessories
      _ -> fail (ForeignError "Invalid ItemCategory value")
    subcategory <- readProp "subcategory" json >>= readImpl
    description <- readProp "description" json >>= readImpl
    tags <- readProp "tags" json >>= readImpl
    effects <- readProp "effects" json >>= readImpl
    strain_lineage <- readProp "strain_lineage" json >>= readImpl
    pure $ MenuItem
      { sort
      , sku
      , brand
      , name
      , price
      , measure_unit
      , per_package
      , quantity
      , category
      , subcategory
      , description
      , tags
      , effects
      , strain_lineage
      }

instance writeForeignInventory :: WriteForeign Inventory where
  writeImpl (Inventory items) = writeImpl items

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure $ Inventory items

instance writeForeignStrainLineage :: WriteForeign StrainLineage where
  writeImpl (StrainLineage lineage) = writeImpl lineage

instance writeForeignSpecies :: WriteForeign Species where 
  writeImpl = writeImpl <<< show

instance readForeignSpecies :: ReadForeign Species where
  readImpl json = do
    str <- readImpl json
    case str of 
      "Indica" -> pure Indica
      "IndicaDominantHybrid" -> pure IndicaDominantHybrid
      "Hybrid" -> pure Hybrid
      "SativaDominantHybrid" -> pure SativaDominantHybrid
      "Sativa" -> pure Sativa
      _ -> fail (ForeignError "Invalid Species value")

instance readForeignStrainLineage :: ReadForeign StrainLineage where
  readImpl json = do
    thc <- readProp "thc" json >>= readImpl
    cbg <- readProp "cbg" json >>= readImpl
    strain <- readProp "strain" json >>= readImpl
    creator <- readProp "creator" json >>= readImpl
    species <- readProp "species" json >>= readImpl
    dominant_tarpene <- readProp "dominant_tarpene" json >>= readImpl
    tarpenes <- readProp "tarpenes" json >>= readImpl
    lineage <- readProp "lineage" json >>= readImpl
    leafly_url <- readProp "leafly_url" json >>= readImpl
    img <- readProp "img" json >>= readImpl
    pure $ StrainLineage
      { thc
      , cbg
      , strain
      , creator
      , species
      , dominant_tarpene
      , tarpenes
      , lineage
      , leafly_url
      , img
      }
      
instance showStrainLineage :: Show StrainLineage where
  show (StrainLineage lineage) = 
    "StrainLineage " <> show lineage
    
instance showMenuItem :: Show MenuItem where
  show (MenuItem item) = 
    "MenuItem " <> show item