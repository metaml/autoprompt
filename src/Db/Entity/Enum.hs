{-# language UndecidableInstances #-}
module Db.Entity.Enum where

import Data.ByteString (ByteString)
import Data.Strings (strCapitalize)
import Data.Text (Text, pack, toTitle, unpack)
import Database.Beam ( (==.), (&&.)
                     , all_, filter_, limit_, runSelectReturningList, select, val_
                     )
import Database.Beam.Backend.SQL (fromBackendRow, sqlValueSyntax, autoSqlValueSyntax)
import Database.Beam.Backend.Types (BeamBackend)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.CustomTypes (FromBackendRow, HasSqlValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS

data FriendEnum  = Human | Ami
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be FriendEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres FriendEnum
-- monad fromBackendRow is dependent on, as in calls, fromField below

instance FromField FriendEnum where
  fromField _ bs = case bs of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: FriendEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--
data MessageEnum = Itm | Stm | Ltm
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MessageEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MessageEnum

instance FromField MessageEnum where
  fromField _ bs = case bs of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MessageEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"

--
data MetaEnum = Detail | Entity | Event | Sentiment | Summary | Theme
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be MetaEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres MetaEnum

instance FromField MetaEnum where
  fromField _ bs = case bs of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: MetaEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
--
data SpeakerEnum = Member | Friend
  deriving (Enum, Eq, Generic, Ord, Read, Show)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be SpeakerEnum where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres SpeakerEnum

instance FromField SpeakerEnum where
  fromField _ bs = case bs of
                     Just enum -> pure (read (strCapitalize . BS.unpack $ enum) :: SpeakerEnum)
                     Nothing   -> error "could not 'read' value for 'FriendEnum'"
