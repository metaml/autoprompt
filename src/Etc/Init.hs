module Etc.Init where

import System.Environment (lookupEnv, setEnv)

-- only setEnv when an env var doesn't exist
setEnvs :: [(String, String)] -> IO ()
setEnvs es = mapM_ (uncurry setEnv') es
  where setEnv' :: String -> String -> IO ()
        setEnv' e v = lookupEnv e >>= \case
          Nothing -> setEnv e v
          _       -> pure ()
