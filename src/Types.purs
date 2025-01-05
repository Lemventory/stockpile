module Types where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
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
  , sku :: String
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

derive instance eqItemCategory :: Eq ItemCategory
derive instance ordItemCategory :: Ord ItemCategory

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

data StrainLineage = StrainLineage
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: String
  , dominant_tarpene :: String
  , tarpenes :: Array String
  , lineage :: Array String
  , leafly_url :: String
  , img :: String
  }

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

instance writeForeignMenuItem :: WriteForeign MenuItem where
  writeImpl (MenuItem item) = writeImpl
    { sort: item.sort
    , sku: item.sku
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
    sku <- readProp "sku" json >>= readImpl
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
    strain_lineage <- readProp "strain_lineage" json >>= readImpl
    pure $ MenuItem { sort, sku, brand, name, price, measure_unit, per_package, quantity, category, subcategory, description, tags, strain_lineage }

instance writeForeignInventory :: WriteForeign Inventory where
  writeImpl (Inventory items) = writeImpl items

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure $ Inventory items

instance writeForeignStrainLineage :: WriteForeign StrainLineage where
  writeImpl (StrainLineage lineage) = writeImpl lineage

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
    pure $ StrainLineage { thc, cbg, strain, creator, species, dominant_tarpene, tarpenes, lineage, leafly_url, img }

instance showStrainLineage :: Show StrainLineage where
  show (StrainLineage lineage) = 
    "StrainLineage " <> show lineage
    
instance showMenuItem :: Show MenuItem where
  show (MenuItem item) = 
    "MenuItem " <> show item