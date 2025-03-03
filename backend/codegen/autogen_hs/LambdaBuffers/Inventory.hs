

module LambdaBuffers.Inventory (Inventory(..)
                               , InventoryResponse(..)
                               , ItemCategory(..)
                               , MenuItem(..)
                               , Species(..)
                               , StrainLineage(..)) where

import qualified LambdaBuffers.Prelude
import qualified Prelude


newtype Inventory = Inventory { inventory'items :: LambdaBuffers.Prelude.List MenuItem} deriving Prelude.Show

data InventoryResponse = InventoryResponse'InventoryData Inventory
                          | InventoryResponse'Message LambdaBuffers.Prelude.Text deriving Prelude.Show

data ItemCategory = ItemCategory'Flower 
                     | ItemCategory'PreRolls 
                     | ItemCategory'Vaporizers 
                     | ItemCategory'Edibles 
                     | ItemCategory'Drinks 
                     | ItemCategory'Concentrates 
                     | ItemCategory'Topicals 
                     | ItemCategory'Tinctures 
                     | ItemCategory'Accessories  deriving Prelude.Show

data MenuItem = MenuItem { menuItem'sort :: LambdaBuffers.Prelude.Integer
                         , menuItem'sku :: LambdaBuffers.Prelude.Text
                         , menuItem'brand :: LambdaBuffers.Prelude.Text
                         , menuItem'name :: LambdaBuffers.Prelude.Text
                         , menuItem'price :: LambdaBuffers.Prelude.Integer
                         , menuItem'measureUnit :: LambdaBuffers.Prelude.Text
                         , menuItem'perPackage :: LambdaBuffers.Prelude.Text
                         , menuItem'quantity :: LambdaBuffers.Prelude.Integer
                         , menuItem'category :: ItemCategory
                         , menuItem'subcategory :: LambdaBuffers.Prelude.Text
                         , menuItem'description :: LambdaBuffers.Prelude.Text
                         , menuItem'tags :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                         , menuItem'effects :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                         , menuItem'strainLineage :: StrainLineage} deriving Prelude.Show

data Species = Species'Indica 
                | Species'IndicaDominantHybrid 
                | Species'Hybrid 
                | Species'SativaDominantHybrid 
                | Species'Sativa  deriving Prelude.Show

data StrainLineage = StrainLineage { strainLineage'thc :: LambdaBuffers.Prelude.Text
                                   , strainLineage'cbg :: LambdaBuffers.Prelude.Text
                                   , strainLineage'strain :: LambdaBuffers.Prelude.Text
                                   , strainLineage'creator :: LambdaBuffers.Prelude.Text
                                   , strainLineage'species :: Species
                                   , strainLineage'dominantTerpene :: LambdaBuffers.Prelude.Text
                                   , strainLineage'terpenes :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                                   , strainLineage'lineage :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                                   , strainLineage'leaflyUrl :: LambdaBuffers.Prelude.Text
                                   , strainLineage'img :: LambdaBuffers.Prelude.Text} deriving Prelude.Show

