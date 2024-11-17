module Etc.Context where

import Data.Text (Text, pack)
import System.Environment (getEnv)

openAiKey :: IO Text
openAiKey = do
  key <- pack <$> getEnv "OPENAI_API_KEY"
  pure key
