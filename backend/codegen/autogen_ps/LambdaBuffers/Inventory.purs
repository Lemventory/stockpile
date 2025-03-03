module LambdaBuffers.Inventory (Inventory(..)
                               , InventoryResponse(..)
                               , ItemCategory(..)
                               , MenuItem(..)
                               , Species(..)
                               , StrainLineage(..)) where

import LambdaBuffers.Prelude as LambdaBuffers.Prelude
import Data.Generic.Rep as Data.Generic.Rep
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic


newtype Inventory = Inventory { items :: LambdaBuffers.Prelude.List MenuItem}
derive instance Data.Newtype.Newtype Inventory _
derive instance Data.Generic.Rep.Generic Inventory _
instance Data.Show.Show Inventory where
  show x = Data.Show.Generic.genericShow x

data InventoryResponse = InventoryResponse'InventoryData Inventory
                          | InventoryResponse'Message LambdaBuffers.Prelude.Text
derive instance Data.Generic.Rep.Generic InventoryResponse _
instance Data.Show.Show InventoryResponse where
  show x = Data.Show.Generic.genericShow x

data ItemCategory = ItemCategory'Flower 
                     | ItemCategory'PreRolls 
                     | ItemCategory'Vaporizers 
                     | ItemCategory'Edibles 
                     | ItemCategory'Drinks 
                     | ItemCategory'Concentrates 
                     | ItemCategory'Topicals 
                     | ItemCategory'Tinctures 
                     | ItemCategory'Accessories 
derive instance Data.Generic.Rep.Generic ItemCategory _
instance Data.Show.Show ItemCategory where
  show x = Data.Show.Generic.genericShow x

newtype MenuItem = MenuItem { sort :: LambdaBuffers.Prelude.Integer
                            , sku :: LambdaBuffers.Prelude.Text
                            , brand :: LambdaBuffers.Prelude.Text
                            , name :: LambdaBuffers.Prelude.Text
                            , price :: LambdaBuffers.Prelude.Integer
                            , measureUnit :: LambdaBuffers.Prelude.Text
                            , perPackage :: LambdaBuffers.Prelude.Text
                            , quantity :: LambdaBuffers.Prelude.Integer
                            , category :: ItemCategory
                            , subcategory :: LambdaBuffers.Prelude.Text
                            , description :: LambdaBuffers.Prelude.Text
                            , tags :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                            , effects :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                            , strainLineage :: StrainLineage}
derive instance Data.Newtype.Newtype MenuItem _
derive instance Data.Generic.Rep.Generic MenuItem _
instance Data.Show.Show MenuItem where
  show x = Data.Show.Generic.genericShow x

data Species = Species'Indica 
                | Species'IndicaDominantHybrid 
                | Species'Hybrid 
                | Species'SativaDominantHybrid 
                | Species'Sativa 
derive instance Data.Generic.Rep.Generic Species _
instance Data.Show.Show Species where
  show x = Data.Show.Generic.genericShow x

newtype StrainLineage = StrainLineage { thc :: LambdaBuffers.Prelude.Text
                                      , cbg :: LambdaBuffers.Prelude.Text
                                      , strain :: LambdaBuffers.Prelude.Text
                                      , creator :: LambdaBuffers.Prelude.Text
                                      , species :: Species
                                      , dominantTerpene :: LambdaBuffers.Prelude.Text
                                      , terpenes :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                                      , lineage :: LambdaBuffers.Prelude.List LambdaBuffers.Prelude.Text
                                      , leaflyUrl :: LambdaBuffers.Prelude.Text
                                      , img :: LambdaBuffers.Prelude.Text}
derive instance Data.Newtype.Newtype StrainLineage _
derive instance Data.Generic.Rep.Generic StrainLineage _
instance Data.Show.Show StrainLineage where
  show x = Data.Show.Generic.genericShow x

