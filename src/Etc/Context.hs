module Etc.Context where

import Data.Text (Text, pack, unpack)
import System.Environment (lookupEnv)

openAiKey :: IO Text
openAiKey = envVar "OPENAI_API_KEY"

dbDatabase :: IO Text
dbDatabase = envVar "PGDATABASE"

dbHost :: IO Text
dbHost = envVar "PGHOST"

dbPassword :: IO Text
dbPassword = envVar "PGPASSWORD"

dbUser :: IO Text
dbUser = envVar "PGUSER"

envVar :: Text -> IO Text
envVar env = lookupEnv (unpack env) >>= \case
  Just e  -> pure (pack e)
  Nothing -> error $ "undefined env var: " <> unpack env
