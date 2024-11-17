module Model.Api.Api where

import GHC.Generics (Generic)
import Model.Api.Ping (PingApi, pingApi)
import Model.Api.Static (StaticApi, staticApi)
import Servant ((:>), (:-), Application, Proxy(..), NamedRoutes, serve)

type ApiRoutes = NamedRoutes Api

data Api mode = Api
  { ops :: mode :- "ops" :> NamedRoutes PingApi
  , static :: mode :- NamedRoutes StaticApi
  } deriving Generic

apiApp :: Application
apiApp = serve (Proxy @ApiRoutes) api
  where api = Api { ops    = pingApi
                  , static = staticApi
                  }
