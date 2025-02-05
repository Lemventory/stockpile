{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.UUID
import Servant
import Types

type InventoryAPI =
  "inventory" :> Get '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Post '[JSON] InventoryResponse
    :<|> "inventory" :> Capture "sku" UUID :> Delete '[JSON] InventoryResponse

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy
