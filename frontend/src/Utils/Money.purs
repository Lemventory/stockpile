-- FILE: frontend/src/Utils/Money.purs
module Utils.Money where

import Prelude

import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..), formatDiscrete)
import Data.Finance.Money.Format (numeric, numericC)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (fromString)
import Data.String (trim)
import Types.DiscreteUSD (DiscreteUSD, fromDiscrete, toDiscrete)
import Data.String as String

stringToDiscreteUSD :: String -> Maybe DiscreteUSD
stringToDiscreteUSD str = do
  num <- fromString (trim str)
  pure $ fromDiscrete $ Discrete (floor (num * 100.0))

numberToDiscreteUSD :: Number -> DiscreteUSD
numberToDiscreteUSD num = fromDiscrete $ Discrete (floor (num * 100.0))

formatMoney :: DiscreteUSD -> String
formatMoney discreteUSD =
  let
    discrete = toDiscrete discreteUSD
    formatted = formatDiscrete numericC discrete
  in
    if formatted == "USD 0.00"
      then "$0.00"
      else "$" <> drop 4 formatted
  where
    drop :: Int -> String -> String
    drop n inputStr =
      if n >= 0 && n < length inputStr
        then substring n (length inputStr) inputStr
        else inputStr

    substring :: Int -> Int -> String -> String
    substring start end inputStr =
      if start < 0 || end < 0 || start > end || start >= length inputStr
        then ""
        else take (end - start) (drop start inputStr)

    take :: Int -> String -> String
    take n inputStr =
      if n <= 0
        then ""
        else substring 0 n inputStr

    length :: String -> Int
    length inputStr = stringLength inputStr

    stringLength :: String -> Int
    stringLength "" = 0
    stringLength input = 1 + stringLength (String.drop 1 input)

centsToFormattedString :: Int -> String
centsToFormattedString cents =
  let dollars = toNumber cents / 100.0
  in "$" <> show dollars

discreteUSDToCents :: DiscreteUSD -> Int
discreteUSDToCents dUSD =
  case toDiscrete dUSD of
    Discrete cents -> cents

discreteToDiscreteUSD :: Discrete USD -> DiscreteUSD
discreteToDiscreteUSD = fromDiscrete

discreteUSDToDiscrete :: DiscreteUSD -> Discrete USD
discreteUSDToDiscrete = toDiscrete

parseMoney :: String -> DiscreteUSD
parseMoney str = fromMaybe (fromDiscrete $ Discrete 0) (stringToDiscreteUSD str)

ensureDollarSign :: String -> String
ensureDollarSign inputStr =
  if take 1 inputStr == "$"
    then inputStr
    else "$" <> inputStr
  where
    take :: Int -> String -> String
    take n str = String.take n str