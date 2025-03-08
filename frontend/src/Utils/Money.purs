module Utils.Money where

import Prelude

import Data.Finance.Currency (USD)
import Data.Finance.Money (Dense, Discrete(..), Rounding(Nearest), formatDiscrete, fromDense)
import Data.Finance.Money.Extended (DiscreteMoney, fromDiscrete', toDiscrete)
import Data.Finance.Money.Format (numeric, numericC)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.String (trim)

-- Format money values with currency code (e.g., "USD 123.45")
formatMoney :: DiscreteMoney USD -> String
formatMoney money = formatDiscrete numericC (toDiscrete money)

-- Format money values without currency code (e.g., "123.45")
formatMoney' :: DiscreteMoney USD -> String
formatMoney' money = formatDiscrete numeric (toDiscrete money)

-- Format Discrete USD directly
formatDiscreteUSD :: Discrete USD -> String
formatDiscreteUSD = formatDiscrete numericC

-- Format Discrete USD directly without currency code
formatDiscreteUSD' :: Discrete USD -> String
formatDiscreteUSD' = formatDiscrete numeric

-- Format money for Dense USD
formatDenseMoney :: Dense USD -> String
formatDenseMoney dense =
  let
    discrete = fromDense Nearest dense
  in
    formatDiscrete numeric discrete

-- Convert between types
fromDiscrete :: Discrete USD -> DiscreteMoney USD
fromDiscrete = fromDiscrete'

-- Utility function for parsing string to Discrete USD
parseMoneyString :: String -> Maybe (Discrete USD)
parseMoneyString str = do
  num <- Number.fromString (trim str)
  pure (Discrete (Int.floor (num * 100.0)))