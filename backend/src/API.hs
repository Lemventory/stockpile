-- src/API.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant
import Types
import Data.UUID

type InventoryAPI = 
       "inventory" :> Get '[JSON] InventoryResponse
  :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Post '[JSON] InventoryResponse
  :<|> "inventory" :> Capture "sku" UUID :> Delete '[JSON] InventoryResponse

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy
