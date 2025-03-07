module Types.UUID where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Foreign (ForeignError(..), fail)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype UUID = UUID String

derive instance genericUUID :: Generic UUID _
derive instance newtypeUUID :: Newtype UUID _
derive instance eqUUID :: Eq UUID
derive instance ordUUID :: Ord UUID

instance showUUID :: Show UUID where
  show (UUID uuid) = uuid

instance writeForeignUUID :: WriteForeign UUID where
  writeImpl (UUID str) = writeImpl str

instance readForeignUUID :: ReadForeign UUID where
  readImpl f = do
    str <- readImpl f
    case parseUUID str of
      Just uuid -> pure uuid
      Nothing -> fail (ForeignError $ "Invalid UUID format: " <> str)

parseUUID :: String -> Maybe UUID
parseUUID str =
  case
    regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
      noFlags
    of
    Left _ -> Nothing
    Right r ->
      if test r str then Just $ UUID str
      else Nothing

uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"