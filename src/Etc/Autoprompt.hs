module Etc.Autoprompt where

import Api.Api (apiApp)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

port :: Int
port = 8000

run :: IO ()
run = runTLS tls warp apiApp
  where tls  = tlsSettings "etc/ssl/cert.pem" "etc/ssl/key.pem"
        warp = setPort port defaultSettings

-- hacked up debugging
-- runC :: IO [Value]
-- runC = do
--   c <- connection
--   history c "Hal" "Courtney" (Just 3)

-- runP :: IO [Prompt]
-- runP = do
--   c <- connection
--   prompts c "system" Nothing
