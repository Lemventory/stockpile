{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Inventory where

import Data.UUID
import Servant
import Types.Inventory
import API.Transaction (PosAPI)

type InventoryAPI =
  "inventory" :> Get '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Post '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Put '[JSON] InventoryResponse
    :<|> "inventory" :> Capture "sku" UUID :> Delete '[JSON] InventoryResponse

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

type API =
  InventoryAPI
    :<|> PosAPI

api :: Proxy API
api = Proxy