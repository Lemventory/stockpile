{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Database where

import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
-- import Database.PostgreSQL.Simple.ToRow (ToRow(..))
-- import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import qualified Data.Vector as V
import Types

data DBConfig = DBConfig
    { dbHost :: String
    , dbPort :: Int
    , dbName :: String
    , dbUser :: String
    , dbPassword :: String
    }

initializeDB :: DBConfig -> IO Connection
initializeDB DBConfig{..} = connect defaultConnectInfo
    { connectHost = dbHost
    , connectPort = fromIntegral dbPort
    , connectDatabase = dbName
    , connectUser = dbUser
    , connectPassword = dbPassword
    }

createTables :: Connection -> IO ()
createTables conn = do
    _ <- execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS menu_items (
            sort INT NOT NULL,
            sku UUID PRIMARY KEY,
            brand TEXT NOT NULL,
            name TEXT NOT NULL,
            price DECIMAL(10,2) NOT NULL,
            measure_unit TEXT NOT NULL,
            per_package TEXT NOT NULL,
            quantity INT NOT NULL,
            category TEXT NOT NULL,
            subcategory TEXT NOT NULL,
            description TEXT NOT NULL,
            tags TEXT[] NOT NULL,
            effects TEXT[] NOT NULL
        )
    |]
    
    _ <- execute_ conn [sql|
        CREATE TABLE IF NOT EXISTS strain_lineage (
            sku UUID PRIMARY KEY REFERENCES menu_items(sku),
            thc TEXT NOT NULL,
            cbg TEXT NOT NULL,
            strain TEXT NOT NULL,
            creator TEXT NOT NULL,
            species TEXT NOT NULL,
            dominant_tarpene TEXT NOT NULL,
            tarpenes TEXT[] NOT NULL,
            lineage TEXT[] NOT NULL,
            leafly_url TEXT NOT NULL,
            img TEXT NOT NULL
        )
    |]
    pure ()

insertMenuItem :: Connection -> MenuItem -> IO ()
insertMenuItem conn MenuItem{..} = do
    _ <- execute conn [sql|
        INSERT INTO menu_items
            (sort, sku, brand, name, price, measure_unit, per_package, 
             quantity, category, subcategory, description, tags, effects)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |] (sort, sku, brand, name, price, measure_unit, per_package,
        quantity, show category, subcategory, description,
        PGArray $ V.toList tags, PGArray $ V.toList effects)
    
    let StrainLineage{..} = strain_lineage
    _ <- execute conn [sql|
        INSERT INTO strain_lineage
            (sku, thc, cbg, strain, creator, species, dominant_tarpene,
             tarpenes, lineage, leafly_url, img)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |] (sku, thc, cbg, strain, creator, show species, dominant_tarpene,
        PGArray $ V.toList tarpenes, PGArray $ V.toList lineage, leafly_url, img)
    pure ()

getAllMenuItems :: Connection -> IO Inventory
getAllMenuItems conn = do
    items <- query_ conn [sql|
        SELECT m.*, 
               s.thc, s.cbg, s.strain, s.creator, s.species,
               s.dominant_tarpene, s.tarpenes, s.lineage,
               s.leafly_url, s.img
        FROM menu_items m
        JOIN strain_lineage s ON m.sku = s.sku
        ORDER BY m.sort
    |]
    return $ Inventory $ V.fromList items