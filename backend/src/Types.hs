{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson
import Data.UUID
import GHC.Generics
import Data.Text (Text)
import Data.Scientific
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))

data Species
    = Indica
    | IndicaDominantHybrid
    | Hybrid
    | SativaDominantHybrid
    | Sativa
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Read)

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
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Read)

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

instance ToRow MenuItem where
    toRow MenuItem{..} = [
        toField sort,
        toField sku,
        toField brand,
        toField name,
        toField price,
        toField measure_unit,
        toField per_package,
        toField quantity,
        toField (show category),
        toField subcategory,
        toField description,
        toField (PGArray $ V.toList tags),
        toField (PGArray $ V.toList effects)
        ]

instance FromRow MenuItem where
    fromRow = MenuItem
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> (read <$> field)
        <*> field
        <*> field
        <*> (V.fromList . fromPGArray <$> field)
        <*> (V.fromList . fromPGArray <$> field)
        <*> (StrainLineage
            <$> field
            <*> field
            <*> field
            <*> field
            <*> (read <$> field)
            <*> field
            <*> (V.fromList . fromPGArray <$> field)
            <*> (V.fromList . fromPGArray <$> field)
            <*> field
            <*> field)

newtype Inventory = Inventory
  { items :: V.Vector MenuItem
  } deriving (Show, Generic, FromJSON, ToJSON)

data InventoryResponse
  = InventoryData Inventory
  | Message Text
  deriving (Show, Generic, FromJSON, ToJSON)