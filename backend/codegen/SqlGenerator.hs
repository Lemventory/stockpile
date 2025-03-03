module SqlGenerator where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.List as L

-- | Convert a camelCase name to snake_case for SQL
camelToSnake :: Text -> Text
camelToSnake = T.pack . concatMap transform . T.unpack
  where
    transform c
      | c >= 'A' && c <= 'Z' = ['_', toLower c]
      | otherwise = [c]

-- | Convert a type name to table name (lowercase with underscores)
toTableName :: Text -> Text
toTableName = camelToSnake . T.map toLower

-- | Mapping of LambdaBuffers types to SQL types
-- This is similar to your existing simpleSqlType function
lbTypeToSqlType :: Text -> Text
lbTypeToSqlType typeName = case typeName of
  "Text"     -> "TEXT"
  "Integer"  -> "INTEGER"
  "Bool"     -> "BOOLEAN"
  "UUID"     -> "UUID"
  _          -> "JSONB"  -- Default to JSONB for complex types

-- | Generate SQL schema for MenuItem
generateMenuItemSQL :: Text
generateMenuItemSQL = T.unlines
  [ "-- Generated SQL Schema for MenuItem"
  , ""
  , "CREATE TABLE menu_item ("
  , "  sort INT NOT NULL,"
  , "  sku UUID NOT NULL,"
  , "  brand TEXT NOT NULL,"
  , "  name TEXT NOT NULL,"
  , "  price INT NOT NULL,"
  , "  measure_unit TEXT NOT NULL,"
  , "  per_package TEXT NOT NULL,"
  , "  quantity INT NOT NULL,"
  , "  category TEXT NOT NULL,"
  , "  subcategory TEXT NOT NULL,"
  , "  description TEXT NOT NULL,"
  , "  tags TEXT[] NOT NULL,"
  , "  effects TEXT[] NOT NULL,"
  , "  PRIMARY KEY (sku)"
  , ");"
  ]

-- | Generate SQL schema for StrainLineage
generateStrainLineageSQL :: Text
generateStrainLineageSQL = T.unlines
  [ "-- Generated SQL Schema for StrainLineage"
  , ""
  , "CREATE TABLE strain_lineage ("
  , "  id SERIAL PRIMARY KEY,"
  , "  thc TEXT NOT NULL,"
  , "  cbg TEXT NOT NULL,"
  , "  strain TEXT NOT NULL,"
  , "  creator TEXT NOT NULL,"
  , "  species TEXT NOT NULL,"
  , "  dominant_terpene TEXT NOT NULL,"
  , "  terpenes TEXT[] NOT NULL,"
  , "  lineage TEXT[] NOT NULL,"
  , "  leafly_url TEXT NOT NULL,"
  , "  img TEXT NOT NULL,"
  , "  menu_item_sku UUID REFERENCES menu_item(sku)"
  , ");"
  ]

-- | Generate complete SQL schema
generateInventorySQL :: Text
generateInventorySQL = T.unlines
  [ generateMenuItemSQL
  , ""
  , generateStrainLineageSQL
  ]

-- Main function to write the SQL schema to a file
writeInventorySQL :: FilePath -> IO ()
writeInventorySQL outputPath = do
  writeFile outputPath (T.unpack generateInventorySQL)
  putStrLn $ "Generated SQL schema: " ++ outputPath