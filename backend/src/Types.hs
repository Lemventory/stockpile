{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson
import Data.UUID
import GHC.Generics
import Data.Text (Text)
import qualified Data.Vector as V

data Species
  = Indica
  | IndicaDominantHybrid
  | Hybrid
  | SativaDominantHybrid
  | Sativa
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

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
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data StrainLineage = StrainLineage
  { thc :: Text
  , cbg :: Text
  , strain :: Text
  , creator :: Text
  , species :: Species
  , dominant_tarpene :: Text
  , tarpenes :: V.Vector Text
  , lineage :: V.Vector Text
  , leafly_url :: Text
  , img :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data MenuItem = MenuItem
  { sort :: Int
  , sku :: UUID
  , brand :: Text
  , name :: Text
  , price :: Scientific
  , measure_unit :: Text
  , per_package :: Text
  , quantity :: Int
  , category :: ItemCategory
  , subcategory :: Text
  , description :: Text
  , tags :: V.Vector Text
  , effects :: V.Vector Text
  , strain_lineage :: StrainLineage
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype Inventory = Inventory
  { items :: V.Vector MenuItem
  } deriving (Show, Generic, FromJSON, ToJSON)

data InventoryResponse
  = InventoryData Inventory
  | Message Text
  deriving (Show, Generic, FromJSON, ToJSON)
