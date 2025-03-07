module Types.DiscreteUSD where

import Prelude

import Data.Finance.Currency (USD)
import Data.Finance.Money (Dense, Discrete(..), formatDiscrete)
import Data.Finance.Money as Data.Finance.Money
import Data.Finance.Money.Format (numeric, numericC)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Number as Number
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

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

-- | Convert a Discrete USD to DiscreteUSD
fromDiscrete :: Discrete USD -> DiscreteUSD
fromDiscrete = DiscreteUSD

-- | Convert a DiscreteUSD to Discrete USD
toDiscrete :: DiscreteUSD -> Discrete USD
toDiscrete (DiscreteUSD d) = d

-- | Convert from Dense USD to DiscreteUSD
fromDense :: Dense USD -> DiscreteUSD
fromDense dense = DiscreteUSD (Data.Finance.Money.fromDense Data.Finance.Money.Nearest dense)

-- | Convert a Number to DiscreteUSD (useful for prices)
fromNumber :: Number -> DiscreteUSD
fromNumber n = DiscreteUSD (Discrete (Int.floor (n * 100.0)))

-- | Convert a DiscreteUSD to a Number (dollars and cents)
toNumber :: DiscreteUSD -> Number
toNumber (DiscreteUSD (Discrete cents)) = Int.toNumber cents / 100.0

-- | Parse from String to DiscreteUSD
fromString :: String -> Maybe DiscreteUSD
fromString str = case Number.fromString str of
  Just n -> Just (fromNumber n)
  Nothing -> Nothing

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

-- Helper functions for working with both types
-- | Helper function to add a DiscreteUSD with a Discrete USD value
addDiscrete :: DiscreteUSD -> Discrete USD -> DiscreteUSD
addDiscrete a b = add a (fromDiscrete b)

-- | Negate a DiscreteUSD value
negate :: DiscreteUSD -> DiscreteUSD
negate (DiscreteUSD a) = DiscreteUSD (sub (Discrete 0) a)

-- | Check if a DiscreteUSD value is zero
isZero :: DiscreteUSD -> Boolean
isZero (DiscreteUSD (Discrete cents)) = cents == 0

-- | Check if a DiscreteUSD value is positive
isPositive :: DiscreteUSD -> Boolean
isPositive (DiscreteUSD (Discrete cents)) = cents > 0

-- | Check if a DiscreteUSD value is negative
isNegative :: DiscreteUSD -> Boolean
isNegative (DiscreteUSD (Discrete cents)) = cents < 0

-- | Zero value
zero :: DiscreteUSD
zero = DiscreteUSD (Discrete 0)