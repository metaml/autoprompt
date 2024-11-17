module Model.Api.Static where

import GHC.Generics (Generic)
import Servant ((:-), Application, Proxy(..), NamedRoutes, serve)
import Servant.API (Raw)
import Servant.Server.Internal (AsServerT, Handler)
import Servant.Server.StaticFiles (serveDirectoryWebApp)

type StaticRoutes = NamedRoutes StaticApi

data StaticApi mode = StaticApi
  { static :: mode :- Raw
  } deriving Generic

staticApi :: StaticApi (AsServerT Handler)
staticApi = StaticApi { static = serveDirectoryWebApp "static" }

-- NB: https://<server>/index.html
staticApp :: Application
staticApp = serve (Proxy @StaticRoutes) staticApi
