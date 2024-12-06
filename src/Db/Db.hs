module Db.Db where

import Data.Text (splitOn, unpack)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema.Tables ( Database, DatabaseSettings, TableEntity
                                   , defaultDbSettings, defaultFieldName, renamingFields, withDbModification
                                   )
import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo)
import Db.Conversation (ConversationT)
import Etc.Context (dbDatabase, dbHost, dbPassword, dbUser)
import GHC.Generics (Generic)

data AmiDb f = AmitDb { conversation :: f (TableEntity ConversationT)
                      } deriving (Generic)
                        deriving anyclass (Database Postgres)

amiDb :: DatabaseSettings Postgres AmiDb
amiDb = defaultDbSettings `withDbModification` renamingFields (rename . defaultFieldName)
  where rename = last . splitOn "__"

connection :: IO Connection
connection = do
  database <- dbDatabase
  host     <- dbHost
  password <- dbPassword
  user     <- dbUser
  connect $ defaultConnectInfo { connectDatabase = unpack database
                               , connectHost     = unpack host
                               , connectUser     = unpack user
                               , connectPassword = unpack password
                               }
