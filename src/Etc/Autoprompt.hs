module Etc.Autoprompt where

import Model.Api.Api (apiApp)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

port :: Int
port = 8443

run :: IO ()
run = runTLS tls warp apiApp
  where
    tls  = tlsSettings "etc/ssl/cert.pem" "etc/ssl/key.pem"
    warp = setPort port defaultSettings
