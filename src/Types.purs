module Types where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import UUID (UUID, parseUUID)
import Foreign (Foreign, ForeignError(..), F, fail)
import Foreign.Index (readProp)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Data.Int as Int
import Data.Number (fromString) as Number
import Data.String (trim)

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
derive instance genericMenuItem :: Generic MenuItem _

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
derive instance eqItemCategory :: Eq ItemCategory
derive instance ordItemCategory :: Ord ItemCategory

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

data Species 
  = Indica 
  | IndicaDominantHybrid 
  | Hybrid 
  | SativaDominantHybrid 
  | Sativa 
derive instance eqItemSpecies :: Eq Species
derive instance ordItemSpecies :: Ord Species

-- | Form input types
type MenuItemFormInput = 
  { name :: String
  , sku :: String
  , brand :: String
  , price :: String
  , quantity :: String
  , category :: String
  , description :: String
  , tags :: String
  , effects :: String
  , strainLineage :: StrainLineageFormInput
  }

type StrainLineageFormInput = 
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: String
  , dominant_tarpene :: String
  , tarpenes :: String
  , lineage :: String
  }

-- | Field configuration types
type FieldConfig = 
  { label :: String
  , placeholder :: String
  , defaultValue :: String
  , validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

type DropdownConfig = 
  { label :: String
  , options :: Array { value :: String, label :: String }
  , defaultValue :: String
  }

-- | Core validation types and type classes
data ValidationResult a = 
  ValidationSuccess a 
  | ValidationError String

-- | Validation types
type ValidationRule = String -> Boolean

type FieldValidator a = 
  { validate :: String -> Boolean
  , convert :: String -> Maybe a
  , error :: String
  }

type ValidationPreset = 
  { validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

class FormValue a where
  fromFormValue :: String -> ValidationResult a

-- | Instances 
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

instance writeForeignInventory :: WriteForeign Inventory where
  writeImpl (Inventory items) = writeImpl items

instance writeForeignStrainLineage :: WriteForeign StrainLineage where
  writeImpl (StrainLineage lineage) = writeImpl lineage

instance writeForeignSpecies :: WriteForeign Species where 
  writeImpl = writeImpl <<< show

instance writeForeignInventoryResponse :: WriteForeign InventoryResponse where
  writeImpl (InventoryData inventory) = writeImpl { type: "data", value: inventory }
  writeImpl (Message msg) = writeImpl { type: "message", value: msg }

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

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure $ Inventory items

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

instance readForeignInventoryResponse :: ReadForeign InventoryResponse where
  readImpl f = do
    obj <- readImpl f
    messageField <- readProp "message" obj >>= readImpl :: F (Maybe String)
    case messageField of
      Just msg -> pure $ Message msg
      Nothing -> do
        inventory <- readImpl f
        pure $ InventoryData inventory
      
instance showStrainLineage :: Show StrainLineage where
  show (StrainLineage lineage) = 
    "StrainLineage " <> show lineage
    
instance showMenuItem :: Show MenuItem where
  show (MenuItem item) = 
    "MenuItem " <> show item

-- | FormValue instances
instance formValueString :: FormValue String where
  fromFormValue = ValidationSuccess <<< trim

instance formValueNumber :: FormValue Number where
  fromFormValue str = case Number.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid number format"

instance formValueInt :: FormValue Int where
  fromFormValue str = case Int.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid integer format"

instance formValueItemCategory :: FormValue ItemCategory where
  fromFormValue str = case str of
    "Flower" -> ValidationSuccess Flower
    "PreRolls" -> ValidationSuccess PreRolls
    "Vaporizers" -> ValidationSuccess Vaporizers
    "Edibles" -> ValidationSuccess Edibles
    "Drinks" -> ValidationSuccess Drinks
    "Concentrates" -> ValidationSuccess Concentrates
    "Topicals" -> ValidationSuccess Topicals
    "Tinctures" -> ValidationSuccess Tinctures
    "Accessories" -> ValidationSuccess Accessories
    _ -> ValidationError "Invalid category value"

instance formValueSpecies :: FormValue Species where
  fromFormValue str = case str of
    "Indica" -> ValidationSuccess Indica
    "IndicaDominant" -> ValidationSuccess IndicaDominantHybrid
    "Hybrid" -> ValidationSuccess Hybrid
    "SativaDominant" -> ValidationSuccess SativaDominantHybrid
    "Sativa" -> ValidationSuccess Sativa
    _ -> ValidationError "Invalid species value"

instance formValueUUID :: FormValue UUID where
  fromFormValue str = case parseUUID (trim str) of
    Just uuid -> ValidationSuccess uuid
    Nothing -> ValidationError "Invalid UUID format"