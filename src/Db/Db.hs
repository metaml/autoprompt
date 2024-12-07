module Db.Db where

import Data.Text (splitOn, unpack)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema.Tables ( Database, DatabaseSettings, TableEntity
                                   , defaultDbSettings, defaultFieldName, renamingFields, withDbModification
                                   )
import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, connect, defaultConnectInfo)
import Db.Entity.Conversation (ConversationT)
import Db.Entity.ConversationMeta (ConversationMetaT)
import Db.Entity.Member (MemberT)
import Db.Entity.Prompt (PromptT)
import Etc.Context (dbDatabase, dbHost, dbPassword, dbUser)
import GHC.Generics (Generic)

data AmiDb f = AmitDb { conversation :: f (TableEntity ConversationT)
                      , conversationMeta :: f (TableEntity ConversationMetaT)
                      , member :: f (TableEntity MemberT)
                      , prompt :: f (TableEntity PromptT)
                      } deriving (Generic)
                        deriving anyclass (Database Postgres)

amiDb :: DatabaseSettings Postgres AmiDb
amiDb = defaultDbSettings
-- amiDb = defaultDbSettings `withDbModification` renamingFields (rename . defaultFieldName)
--   where rename = last . splitOn "__"

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
