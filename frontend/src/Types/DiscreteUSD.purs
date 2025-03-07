module Types.DiscreteUSD where

import Prelude

import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..), formatDiscrete)
import Data.Finance.Money.Format (numeric, numericC)
import Data.Newtype (class Newtype)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

-- Move DiscreteUSD and related functions to a separate module to avoid circular dependencies
newtype DiscreteUSD = DiscreteUSD (Discrete USD)

derive instance newtypeDiscreteUSD :: Newtype DiscreteUSD _
derive instance eqDiscreteUSD :: Eq DiscreteUSD
derive instance ordDiscreteUSD :: Ord DiscreteUSD

instance showDiscreteUSD :: Show DiscreteUSD where
  show (DiscreteUSD d) = "(DiscreteUSD " <> show d <> ")"

instance writeForeignDiscreteUSD :: WriteForeign DiscreteUSD where
  writeImpl (DiscreteUSD (Discrete n)) = writeImpl n

instance readForeignDiscreteUSD :: ReadForeign DiscreteUSD where
  readImpl f = do
    n <- readImpl f
    pure $ DiscreteUSD (Discrete n)

fromDiscrete :: Discrete USD -> DiscreteUSD
fromDiscrete = DiscreteUSD

toDiscrete :: DiscreteUSD -> Discrete USD
toDiscrete (DiscreteUSD d) = d

formatUSD :: DiscreteUSD -> String
formatUSD (DiscreteUSD d) = "$" <> formatDiscrete numeric d

formatUSDWithCode :: DiscreteUSD -> String
formatUSDWithCode (DiscreteUSD d) = formatDiscrete numericC d

instance semiringDiscreteUSD :: Semiring DiscreteUSD where
  add (DiscreteUSD a) (DiscreteUSD b) = DiscreteUSD (a + b)
  zero = DiscreteUSD (Discrete 0)
  mul (DiscreteUSD a) (DiscreteUSD b) = DiscreteUSD (a * b)
  one = DiscreteUSD (Discrete 100)

instance ringDiscreteUSD :: Ring DiscreteUSD where
  sub (DiscreteUSD a) (DiscreteUSD b) = DiscreteUSD (a - b)

fromCents :: Int -> DiscreteUSD
fromCents cents = DiscreteUSD (Discrete cents)

toCents :: DiscreteUSD -> Int
toCents (DiscreteUSD (Discrete cents)) = cents