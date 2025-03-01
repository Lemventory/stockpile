{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}

-- Enhanced Schema Definition Types
module Main where

import Prelude hiding (map)
import qualified Prelude as P (map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.Environment (getArgs)
import GHC.Generics
import Dhall (FromDhall, input, auto)
import Control.Exception (try, SomeException)
import Data.Char (isAsciiUpper, isAsciiLower)

data SimpleFieldType =
    FTText
  | FTInt
  | FTNatural
  | FTDouble
  | FTScientific
  | FTUuid
  | FTBool
  | FTEnum Data.Text.Text 
  | FTRecord Data.Text.Text  
  | FTVariant Data.Text.Text 
  deriving (Show, Eq, Generic, FromDhall)

data Field = Field
  { fieldName :: Data.Text.Text
  , fieldType :: SimpleFieldType
  , fieldArray :: Bool
  , fieldRequired :: Bool
  , fieldDescription :: Data.Text.Text
  , fieldIsInDB :: Bool
  , fieldCustomJSON :: Maybe Data.Text.Text
  } deriving (Show, Eq, Generic, FromDhall)

data VariantConstructor = VariantConstructor
  { constructorName :: Data.Text.Text
  , constructorFields :: [Field]
  , constructorDescription :: Data.Text.Text
  } deriving (Show, Eq, Generic, FromDhall)

data EntityType =
    Record 
  | Newtype 
  | Variant
  deriving (Show, Eq, Generic, FromDhall)

data Entity = Entity
  { entityName :: Data.Text.Text
  , entityType :: EntityType
  , entityFields :: [Field]
  , entityVariants :: [VariantConstructor] 
  , entityPrimaryKey :: [Data.Text.Text]
  , entityDescription :: Data.Text.Text
  , entityCustomJSON :: Maybe Data.Text.Text 
  } deriving (Show, Eq, Generic, FromDhall)

data EnumDef = EnumDef
  { enumName :: Data.Text.Text
  , enumValues :: [Data.Text.Text]
  , enumDescription :: Data.Text.Text
  } deriving (Show, Eq, Generic, FromDhall)

data Schema = Schema
  { schemaName :: Data.Text.Text
  , schemaEntities :: [Entity]
  , schemaEnums :: [EnumDef]
  , schemaDescription :: Data.Text.Text
  } deriving (Show, Eq, Generic, FromDhall)


toCamelCase :: Data.Text.Text -> Data.Text.Text
toCamelCase t = case T.uncons t of
  Nothing -> ""
  Just (c, rest) -> T.cons (toLower c) rest
  where
    toLower :: Char -> Char
    toLower c = if isAsciiUpper c then toEnum (fromEnum c + 32) else c

toPascalCase :: Data.Text.Text -> Data.Text.Text
toPascalCase t = case T.uncons t of
  Nothing -> ""
  Just (c, rest) -> T.cons (toUpper c) rest
  where
    toUpper :: Char -> Char
    toUpper c = if isAsciiLower c then toEnum (fromEnum c - 32) else c

snakeToCamel :: Data.Text.Text -> Data.Text.Text
snakeToCamel t =
  case T.breakOn "_" t of
    (pre, post)
      | T.null post -> pre
      | otherwise -> pre <> toPascalCase (snakeToCamel (T.drop 1 post))

getHaskellType :: Field -> Data.Text.Text
getHaskellType field
  | fieldArray field = "V.Vector " <> simpleHaskellType (fieldType field)
  | otherwise = simpleHaskellType (fieldType field)

simpleHaskellType :: SimpleFieldType -> Data.Text.Text
simpleHaskellType ft = case ft of
  FTText -> "Text"
  FTInt -> "Int"
  FTNatural -> "Int"
  FTDouble -> "Double"
  FTScientific -> "Scientific"
  FTUuid -> "UUID"
  FTBool -> "Bool"
  FTEnum name -> name
  FTRecord name -> name
  FTVariant name -> name

getPurscriptType :: Field -> Data.Text.Text
getPurscriptType field
  | fieldArray field = "Array " <> simplePurscriptType (fieldType field)
  | otherwise = simplePurscriptType (fieldType field)

-- Convert from SimpleFieldType to a PureScript type string
simplePurscriptType :: SimpleFieldType -> Data.Text.Text
simplePurscriptType ft = case ft of
  FTText -> "String"
  FTInt -> "Int"
  FTNatural -> "Int"
  FTDouble -> "Number"
  FTScientific -> "Number"
  FTUuid -> "UUID"
  FTBool -> "Boolean"
  FTEnum name -> name
  FTRecord name -> name
  FTVariant name -> name

-- Helper to expand field type with array for SQL
getSqlType :: Field -> Data.Text.Text
getSqlType field
  | fieldArray field = if fieldType field == FTText then "TEXT[]" else "JSONB"
  | otherwise = simpleSqlType (fieldType field)

-- Convert from SimpleFieldType to an SQL type string
simpleSqlType :: SimpleFieldType -> Data.Text.Text
simpleSqlType ft = case ft of
  FTText -> "TEXT"
  FTInt -> "INT"
  FTNatural -> "INT"
  FTDouble -> "DECIMAL(10,2)"
  FTScientific -> "DECIMAL(10,2)"
  FTUuid -> "UUID"
  FTBool -> "BOOLEAN"
  FTEnum _ -> "TEXT"
  FTRecord _ -> "JSONB"
  FTVariant _ -> "JSONB"

-- Find fields in an entity that should be stored in the database
dbFields :: Entity -> [Field]
dbFields entity = filter fieldIsInDB (entityFields entity)

-- Generate Haskell type definitions with improved handling of variant types and custom JSON
generateHaskellTypes :: Schema -> Data.Text.Text
generateHaskellTypes schema = T.unlines
  [ "-- Generated Haskell code for " <> schemaName schema
  , "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE DeriveAnyClass #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE RecordWildCards #-}"
  , ""
  , "module Types where" -- Use fixed module name
  , ""
  , "import Data.Aeson"
  , "import Data.Aeson.Types (Parser)"
  , "import Data.Scientific"
  , "import Data.Text (Text)"
  , "import qualified Data.Text as T"
  , "import Data.UUID"
  , "import qualified Data.Vector as V"
  , "import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)"
  , "import Database.PostgreSQL.Simple.ToField (ToField (..))"
  , "import Database.PostgreSQL.Simple.ToRow (ToRow (..))"
  , "import Database.PostgreSQL.Simple.FromField (FromField (..), fromPGArray)"
  , "import Database.PostgreSQL.Simple.Types (PGArray (..))"
  , "import GHC.Generics"
  , ""
  ] <> generateEnums <> generateDataTypes <> generateInstances
  where
    generateEnums = T.concat $ P.map generateEnum (schemaEnums schema)

    generateEnum enumDef = T.unlines
      [ "data " <> enumName enumDef
      , "  = " <> T.intercalate "\n  | " (enumValues enumDef)
      , "  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Read)"
      , ""
      ]

    generateDataTypes = T.concat $ P.map generateDataType (schemaEntities schema)

    generateDataType entity = case entityType entity of
      Newtype -> T.unlines
        [ "newtype " <> entityName entity <> " = " <> entityName entity
        , "  { " <> recordName <> " :: " <> newtypeInnerType
        , "  }"
        , "  deriving (Show, Generic)"
        , ""
        ]
        where
          -- Use the actual field name instead of constructing one
          recordName = case entityFields entity of
            [field] -> fieldName field
            _ -> "ERROR: Newtype must have exactly one field"
          newtypeInnerType = case entityFields entity of
            [field] -> getHaskellType field
            _ -> "ERROR: Newtype must have exactly one field"

      Record -> T.unlines
        [ "data " <> entityName entity <> " = " <> entityName entity
        , "  { " <> T.intercalate "\n  , " (P.map generateField (entityFields entity))
        , "  }"
        , "  deriving (Show, Generic)"
        , ""
        ]

      Variant -> T.unlines
        [ "data " <> entityName entity
        , "  = " <> T.intercalate "\n  | " (P.map generateVariantConstructor (entityVariants entity))
        , "  deriving (Show, Generic)"
        , ""
        ]

    generateField field =
      fieldName field <> " :: " <> getHaskellType field

    generateVariantConstructor varCons =
      if null (constructorFields varCons)
        then constructorName varCons
        else constructorName varCons <> " " <> T.intercalate " " (P.map getHaskellType (constructorFields varCons))

    generateInstances = T.concat $ concatMap genInstancesForEntity (schemaEntities schema)

    genInstancesForEntity entity =
      genJSONInstances entity ++
      if not (null (dbFields entity)) && entityType entity /= Variant
        then genDBInstances entity
        else []

    genJSONInstances entity = case entityCustomJSON entity of
      Just custom -> T.lines custom
      Nothing -> case entityType entity of
        Variant ->
          [ "instance ToJSON " <> entityName entity <> " where"
          , "  toJSON = \\case"
          ] ++ concatMap genVariantToJSON (entityVariants entity) ++
          [ ""
          , "instance FromJSON " <> entityName entity <> " where"
          , "  parseJSON = withObject \"" <> entityName entity <> "\" $ \\obj -> do"
          , "    typ <- obj .: \"type\""
          , "    case typ of"
          ] ++ concatMap genVariantFromJSON (entityVariants entity) ++
          [ "      _ -> fail $ \"Unknown " <> entityName entity <> " type: \" ++ typ"
          , ""
          ]
        _ ->
          [ "instance ToJSON " <> entityName entity
          , "instance FromJSON " <> entityName entity
          , ""
          ]

    genVariantToJSON varCons =
      [ "    " <> constructorName varCons <> (if null (constructorFields varCons) then "" else " val") <> " ->"
      , "      object [ \"type\" .= (\"" <> constructorName varCons <> "\" :: Text)"
      ] ++ (if null (constructorFields varCons)
            then ["]"]
            else [", \"value\" .= val", "]"])

    genVariantFromJSON varCons =
      ("      \"" <> constructorName varCons <> "\" -> do") : (if null (constructorFields varCons)
            then ["        pure " <> constructorName varCons]
            else ["        val <- obj .: \"value\"", "        pure $ " <> constructorName varCons <> " val"])

    genDBInstances entity =
      [ "instance ToRow " <> entityName entity <> " where"
      , "  toRow " <> entityName entity <> " {..} ="
      , "    [ " <> T.intercalate "\n    , " (genToFieldEntries entity) <> ""
      , "    ]"
      , ""
      , "instance FromRow " <> entityName entity <> " where"
      , "  fromRow ="
      , "    " <> entityName entity
      ] ++ (case entityType entity of
              Record -> ["      " <> T.intercalate "\n      " (genFromFieldEntries entity)]
              _ -> ["      " <> T.intercalate "\n      " (genNewtypeFromFieldEntries entity)])
      ++ [""]

    genToFieldEntries entity =
      P.map genToFieldEntry (dbFields entity)

    genToFieldEntry field =
      case fieldType field of
        FTEnum _ -> "toField (show " <> fieldName field <> ")"
        _ | fieldArray field -> "toField (PGArray $ V.toList " <> fieldName field <> ")"
        _ -> "toField " <> fieldName field

    genFromFieldEntries entity =
      ("<$> " <> genFromFieldEntry (head (dbFields entity))) : P.map (\f -> "<*> " <> genFromFieldEntry f) (tail (dbFields entity))

    genNewtypeFromFieldEntries entity =
      ["<$> " <> genFromFieldEntry (head (dbFields entity))]

    genFromFieldEntry field = case fieldType field of
      FTEnum _ -> "(read <$> field)"
      _ | fieldArray field -> "(V.fromList . fromPGArray <$> field)"
      FTRecord recName ->
        let recEntity = findEntityByName recName (schemaEntities schema)
        in case recEntity of
             Just e ->
               -- Special case handling for StrainLineage in MenuItem
               if entityName e == "StrainLineage" && fieldName field == "strain_lineage"
               then
                 "( StrainLineage\n" <>
                 "              <$> field\n" <>
                 "              <*> field\n" <>
                 "              <*> field\n" <>
                 "              <*> field\n" <>
                 "              <*> (read <$> field)\n" <>
                 "              <*> field\n" <>
                 "              <*> (V.fromList . fromPGArray <$> field)\n" <>
                 "              <*> (V.fromList . fromPGArray <$> field)\n" <>
                 "              <*> field\n" <>
                 "              <*> field\n" <>
                 "          )"
               else "(" <> genNestedRecord e <> ")"
             Nothing -> "field" -- Fallback if record not found
      _ -> "field"

    genNestedRecord entity =
      entityName entity <> "\n" <>
      T.intercalate "\n" (P.map (\f -> "              <*> " <> genFromFieldEntry f) (dbFields entity))

    findEntityByName name entities =
      case filter (\e -> entityName e == name) entities of
        (x:_) -> Just x
        _ -> Nothing

-- Generate PureScript type definitions with improved support for variant types
generatePurescriptTypes :: Schema -> Data.Text.Text
generatePurescriptTypes schema = T.unlines
  [ "-- Generated PureScript code for " <> schemaName schema
  , "module " <> toPascalCase (schemaName schema) <> ".Types where"
  , ""
  , "import Prelude"
  , ""
  , "import Data.Generic.Rep (class Generic)"
  , "import Data.Show.Generic (genericShow)"
  , "import Data.Either (Either(..))"
  , "import Data.UUID (UUID)"
  , "import Data.Newtype (class Newtype)"
  , "import Data.Maybe (Maybe(..))"
  , "import Foreign (Foreign, F, ForeignError(..), fail, typeOf)"
  , "import Foreign.Index (readProp)"
  , "import Foreign.Object (Object)"
  , "import Foreign.Object as Object"
  , "import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)"
  , ""
  ] <> generateEnums <> generateDataTypes <> generateInstances
  where
    generateEnums = T.concat $ P.map generateEnum (schemaEnums schema)

    generateEnum enumDef = T.unlines
      [ "data " <> enumName enumDef
      , "  = " <> T.intercalate "\n  | " (enumValues enumDef)
      , ""
      , "derive instance eq" <> enumName enumDef <> " :: Eq " <> enumName enumDef
      , "derive instance ord" <> enumName enumDef <> " :: Ord " <> enumName enumDef
      , "derive instance generic" <> enumName enumDef <> " :: Generic " <> enumName enumDef <> " _"
      , ""
      ]

    generateDataTypes = T.concat $ P.map generateDataType (schemaEntities schema)

    generateDataType entity = case entityType entity of
      Newtype -> T.unlines
        [ "newtype " <> entityName entity <> " = " <> entityName entity <> " " <>
          case entityFields entity of
            [field] -> getPurscriptType field
            _ -> "ERROR: Newtype must have exactly one field"
        , ""
        , "derive instance newtype" <> entityName entity <> " :: Newtype " <> entityName entity <> " _"
        , "derive instance generic" <> entityName entity <> " :: Generic " <> entityName entity <> " _"
        , ""
        ]

      Record -> T.unlines
        [ "type " <> entityName entity <> "Record ="
        , "  { " <> T.intercalate "\n  , " (P.map generateField (entityFields entity))
        , "  }"
        , ""
        , "newtype " <> entityName entity <> " = " <> entityName entity <> " " <> entityName entity <> "Record"
        , ""
        , "derive instance newtype" <> entityName entity <> " :: Newtype " <> entityName entity <> " _"
        , "derive instance generic" <> entityName entity <> " :: Generic " <> entityName entity <> " _"
        , ""
        ]

      Variant -> T.unlines
        [ "data " <> entityName entity
        , "  = " <> T.intercalate "\n  | " (P.map generateVariantConstructor (entityVariants entity))
        , ""
        , "derive instance eq" <> entityName entity <> " :: Eq " <> entityName entity
        , "derive instance generic" <> entityName entity <> " :: Generic " <> entityName entity <> " _"
        , ""
        ]

    generateField field =
      fieldName field <> " :: " <> getPurscriptType field

    generateVariantConstructor varCons =
      if null (constructorFields varCons)
        then constructorName varCons
        else constructorName varCons <> " " <> T.intercalate " " (P.map getPurscriptType (constructorFields varCons))

    generateInstances = T.concat $ concatMap genInstancesForEntity (schemaEntities schema)

    genInstancesForEntity entity =
      [ "instance Show " <> entityName entity <> " where"
      , "  show = genericShow"
      , ""
      ] ++
      case entityType entity of
        Variant -> genVariantInstances entity
        _ -> genRegularInstances entity

    genRegularInstances entity =
      [ "instance WriteForeign " <> entityName entity <> " where"
      , "  writeImpl " <> generateWriteForeignImpl (entityName entity) (entityType entity)
      , ""
      , "instance ReadForeign " <> entityName entity <> " where"
      , "  readImpl " <> generateReadForeignImpl (entityName entity) (entityType entity)
      , ""
      ]

    genVariantInstances entity =
      [ "instance WriteForeign " <> entityName entity <> " where"
      , "  writeImpl = case _ of"
      ] ++ concatMap genVariantWriteForeign (entityVariants entity) ++
      [ ""
      , "instance ReadForeign " <> entityName entity <> " where"
      , "  readImpl f = do"
      , "    obj <- readImpl f"
      , "    typeField <- readProp \"type\" obj"
      , "    case typeField of"
      ] ++ concatMap genVariantReadForeign (entityVariants entity) ++
      [ "      other -> fail $ ForeignError $ \"Unknown " <> entityName entity <> " type: \" <> other"
      , ""
      ]

    genVariantWriteForeign varCons =
      if null (constructorFields varCons)
        then ["    " <> constructorName varCons <> " -> writeImpl { \"type\": \"" <> constructorName varCons <> "\" }"]
        else ["    " <> constructorName varCons <> " val -> writeImpl { \"type\": \"" <> constructorName varCons <> "\", \"value\": val }"]

    genVariantReadForeign varCons =
      if null (constructorFields varCons)
        then ["      \"" <> constructorName varCons <> "\" -> pure " <> constructorName varCons]
        else ["      \"" <> constructorName varCons <> "\" -> do", "        val <- readProp \"value\" obj", "        " <> constructorName varCons <> " <$> readImpl val"]

    generateWriteForeignImpl name Newtype = "(" <> name <> " val) = writeImpl val"
    generateWriteForeignImpl name Record = "(" <> name <> " record) = writeImpl record"
    generateWriteForeignImpl _ Variant = "error \"Should use pattern matching for variants\""

    generateReadForeignImpl name Newtype = "f = " <> name <> " <$> readImpl f"
    generateReadForeignImpl name Record = "f = " <> name <> " <$> readImpl f"
    generateReadForeignImpl _ Variant = "error \"Should use custom variant read implementation\""

-- Generate SQL schema (similar to original, but with support for variant types)
generateSQLSchema :: Schema -> Data.Text.Text
generateSQLSchema schema = T.unlines $
  ("-- Generated SQL Schema for " <> schemaName schema) : concatMap generateTableDef (schemaEntities schema)
  where
    generateTableDef entity
      | null (dbFields entity) || entityType entity == Variant = [] -- Skip entities that don't have DB fields or are variants
      | otherwise =
        [ ""
        , "CREATE TABLE " <> T.toLower (entityName entity) <> " ("
        , T.intercalate ",\n" $ P.map generateColumnDef (dbFields entity) ++ generatePrimaryKey (entityPrimaryKey entity)
        , ");"
        ]

    generateColumnDef field =
      "  " <> fieldName field <> " " <> getSqlType field <> nullable
      where
        nullable = if fieldRequired field then " NOT NULL" else ""

    generatePrimaryKey [] = []
    generatePrimaryKey keys = ["  PRIMARY KEY (" <> T.intercalate ", " keys <> ")"]

-- Read schema from Dhall file
readSchemaFromDhall :: FilePath -> IO (Either String Schema)
readSchemaFromDhall path = do
  result <- try $ input auto (T.pack path)
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Error reading Dhall file: " ++ show e
      return $ Left $ "Error reading Dhall file: " ++ show e
    Right schema -> return $ Right schema

-- Generate all code from a schema
generateAllFromSchema :: Schema -> FilePath -> IO ()
generateAllFromSchema schema outputDir = do
  createDirectoryIfMissing True outputDir

  -- Generate Haskell code
  let hsPath = outputDir </> T.unpack (toPascalCase (schemaName schema)) <> "Types.hs"
  TIO.writeFile hsPath (generateHaskellTypes schema)
  putStrLn $ "Generated Haskell code: " ++ hsPath

  -- Generate PureScript code
  let pursPath = outputDir </> T.unpack (toPascalCase (schemaName schema)) <> "Types.purs"
  TIO.writeFile pursPath (generatePurescriptTypes schema)
  putStrLn $ "Generated PureScript code: " ++ pursPath

  -- Generate SQL schema
  let sqlPath = outputDir </> T.unpack (schemaName schema) <> ".sql"
  TIO.writeFile sqlPath (generateSQLSchema schema)
  putStrLn $ "Generated SQL schema: " ++ sqlPath

-- Default paths for project
defaultSchemaPath :: FilePath
defaultSchemaPath = "/home/bismuth/workdir/cheeblr/backend/codegen/schema.dhall"

defaultGeneratedDir :: FilePath
defaultGeneratedDir = "/home/bismuth/workdir/cheeblr/backend/codegen/generated"

backendTypesPath :: FilePath
backendTypesPath = "/home/bismuth/workdir/cheeblr/backend/src/Types.hs"

frontendTypesPath :: FilePath
frontendTypesPath = "/home/bismuth/workdir/cheeblr/frontend/src/Types.purs"

-- Create parent directories if they don't exist
ensureDirectoryForFile :: FilePath -> IO ()
ensureDirectoryForFile path = createDirectoryIfMissing True (takeDirectory path)

-- Main entry point
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate"] -> do
      putStrLn "Using default paths:"
      putStrLn $ "  Schema: " ++ defaultSchemaPath
      putStrLn $ "  Output: " ++ defaultGeneratedDir
      result <- readSchemaFromDhall defaultSchemaPath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right schema -> do
          putStrLn "Schema loaded successfully. Generating code..."
          ensureDirectoryForFile (defaultGeneratedDir </> "test.txt")
          generateAllFromSchema schema defaultGeneratedDir
          putStrLn "Code generation complete!"

    ["generate", dhallFile] -> do
      putStrLn $ "Reading schema from: " ++ dhallFile
      putStrLn $ "Output to: " ++ defaultGeneratedDir
      result <- readSchemaFromDhall dhallFile
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right schema -> do
          putStrLn "Schema loaded successfully. Generating code..."
          ensureDirectoryForFile (defaultGeneratedDir </> "test.txt")
          generateAllFromSchema schema defaultGeneratedDir
          putStrLn "Code generation complete!"

    ["generate", dhallFile, outputDir] -> do
      putStrLn $ "Reading schema from " ++ dhallFile
      putStrLn $ "Output to: " ++ outputDir
      result <- readSchemaFromDhall dhallFile
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right schema -> do
          putStrLn "Schema loaded successfully. Generating code..."
          ensureDirectoryForFile (outputDir </> "test.txt")
          generateAllFromSchema schema outputDir
          putStrLn "Code generation complete!"

    ["deploy"] -> do
      putStrLn "Deploying generated code to actual project files..."
      putStrLn $ "Reading schema from: " ++ defaultSchemaPath
      result <- readSchemaFromDhall defaultSchemaPath
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right schema -> do
          putStrLn "Schema loaded successfully. Generating and deploying code..."

          -- First generate to the test directory
          ensureDirectoryForFile (defaultGeneratedDir </> "test.txt")
          generateAllFromSchema schema defaultGeneratedDir

          -- Then copy the generated files to their final destinations
          let hsPath = defaultGeneratedDir </> T.unpack (toPascalCase (schemaName schema)) <> "Types.hs"
          let pursPath = defaultGeneratedDir </> T.unpack (toPascalCase (schemaName schema)) <> "Types.purs"

          putStrLn $ "Copying " ++ hsPath ++ " to " ++ backendTypesPath
          ensureDirectoryForFile backendTypesPath
          readFile hsPath >>= writeFile backendTypesPath

          putStrLn $ "Copying " ++ pursPath ++ " to " ++ frontendTypesPath
          ensureDirectoryForFile frontendTypesPath
          readFile pursPath >>= writeFile frontendTypesPath

          putStrLn "Deployment complete!"

    _ ->  do
        putStrLn "Enhanced Schema Generator"
        putStrLn "Usage:"
        putStrLn "  domain-modeler generate"
        putStrLn "    - Generates code from the default schema file to the test directory"
        putStrLn "  domain-modeler generate <schema.dhall>"
        putStrLn "    - Generates code from the specified schema file to the test directory"
        putStrLn "  domain-modeler generate <schema.dhall> <output-dir>"
        putStrLn "    - Generates code from a schema file to the specified directory"
        putStrLn "  domain-modeler deploy"
        putStrLn "    - Generates code and copies it to the actual project files"