{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Inventory where

import Data.UUID
import Servant
import Types.Inventory
import API.Transaction (PosAPI, posAPI)

-- Original inventory API
type InventoryAPI =
  "inventory" :> Get '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Post '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Put '[JSON] InventoryResponse
    :<|> "inventory" :> Capture "sku" UUID :> Delete '[JSON] InventoryResponse

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

-- Combined API
type API =
  InventoryAPI
    :<|> PosAPI

api :: Proxy API
api = Proxy