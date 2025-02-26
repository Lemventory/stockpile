{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, throwIO)
import qualified Data.Pool as Pool
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import System.IO (hPutStrLn, stderr)
import Types

data DBConfig = DBConfig
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbUser :: String
  , dbPassword :: String
  , poolSize :: Int
  }

initializeDB :: DBConfig -> IO (Pool.Pool Connection)
initializeDB config = do
  let poolConfig =
        Pool.defaultPoolConfig
          (connectWithRetry config)
          close
          0.5 -- # of stripes
          10 -- keep unused connections (seconds_
  pool <- Pool.newPool poolConfig

  Pool.withResource pool $ \conn -> do
    _ <- query_ conn "SELECT 1" :: IO [Only Int]
    pure ()

  pure pool

connectWithRetry :: DBConfig -> IO Connection
connectWithRetry DBConfig {..} = go 5
  where
    go :: Int -> IO Connection
    go retriesLeft = do
      let connInfo =
            defaultConnectInfo
              { connectHost = dbHost
              , connectPort = fromIntegral dbPort
              , connectDatabase = dbName
              , connectUser = dbUser
              , connectPassword = dbPassword
              }

      catch
        (connect connInfo)
        (`handleConnError` retriesLeft)

    handleConnError :: SqlError -> Int -> IO Connection
    handleConnError e retriesLeft
      | retriesLeft == 0 = do
          hPutStrLn stderr $ "Failed to connect to database after 5 attempts: " ++ show e
          throwIO e
      | otherwise = do
          hPutStrLn stderr "Database connection attempt failed, retrying in 5 seconds..."
          threadDelay 5000000
          go (retriesLeft - 1)

withConnection :: Pool.Pool Connection -> (Connection -> IO a) -> IO a
withConnection = Pool.withResource

createTables :: Pool.Pool Connection -> IO ()
createTables pool = withConnection pool $ \conn -> do
  _ <-
    execute_
      conn
      [sql|
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

  _ <-
    execute_
      conn
      [sql|
        CREATE TABLE IF NOT EXISTS strain_lineage (
            sku UUID PRIMARY KEY REFERENCES menu_items(sku),
            thc TEXT NOT NULL,
            cbg TEXT NOT NULL,
            strain TEXT NOT NULL,
            creator TEXT NOT NULL,
            species TEXT NOT NULL,
            dominant_terpene TEXT NOT NULL,
            terpenes TEXT[] NOT NULL,
            lineage TEXT[] NOT NULL,
            leafly_url TEXT NOT NULL,
            img TEXT NOT NULL
        )
    |]
  pure ()

insertMenuItem :: Pool.Pool Connection -> MenuItem -> IO ()
insertMenuItem pool MenuItem {..} = withConnection pool $ \conn -> do
  _ <-
    execute
      conn
      [sql|
        INSERT INTO menu_items
            (sort, sku, brand, name, price, measure_unit, per_package, 
             quantity, category, subcategory, description, tags, effects)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]
      ( sort
      , sku
      , brand
      , name
      , price
      , measure_unit
      , per_package
      , quantity
      , show category
      , subcategory
      , description
      , PGArray $ V.toList tags
      , PGArray $ V.toList effects
      )

  let StrainLineage {..} = strain_lineage
  _ <-
    execute
      conn
      [sql|
        INSERT INTO strain_lineage
            (sku, thc, cbg, strain, creator, species, dominant_terpene,
             terpenes, lineage, leafly_url, img)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]
      ( sku
      , thc
      , cbg
      , strain
      , creator
      , show species
      , dominant_terpene
      , PGArray $ V.toList terpenes
      , PGArray $ V.toList lineage
      , leafly_url
      , img
      )
  pure ()

getAllMenuItems :: Pool.Pool Connection -> IO Inventory
getAllMenuItems pool = withConnection pool $ \conn -> do
  items <-
    query_
      conn
      [sql|
        SELECT m.*, 
               s.thc, s.cbg, s.strain, s.creator, s.species,
               s.dominant_terpene, s.terpenes, s.lineage,
               s.leafly_url, s.img
        FROM menu_items m
        JOIN strain_lineage s ON m.sku = s.sku
        ORDER BY m.sort
    |]
  return $ Inventory $ V.fromList items

updateExistingMenuItem :: Pool.Pool Connection -> MenuItem -> IO ()
updateExistingMenuItem pool MenuItem {..} = withConnection pool $ \conn -> do
  _ <-
    execute
      conn
      [sql|
        UPDATE menu_items
        SET sort = ?, brand = ?, name = ?, price = ?, measure_unit = ?, 
            per_package = ?, quantity = ?, category = ?, subcategory = ?,
            description = ?, tags = ?, effects = ?
        WHERE sku = ?
    |]
      ( sort
      , brand
      , name
      , price
      , measure_unit
      , per_package
      , quantity
      , show category
      , subcategory
      , description
      , PGArray $ V.toList tags
      , PGArray $ V.toList effects
      , sku
      )

  let StrainLineage {..} = strain_lineage
  _ <-
    execute
      conn
      [sql|
        UPDATE strain_lineage
        SET thc = ?, cbg = ?, strain = ?, creator = ?, species = ?,
            dominant_terpene = ?, terpenes = ?, lineage = ?,
            leafly_url = ?, img = ?
        WHERE sku = ?
    |]
      ( thc
      , cbg
      , strain
      , creator
      , show species
      , dominant_terpene
      , PGArray $ V.toList terpenes
      , PGArray $ V.toList lineage
      , leafly_url
      , img
      , sku
      )
  pure ()