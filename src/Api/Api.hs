module Api.Api where

import Api.Ping (PingApi, pingApi)
import Api.Static (StaticApi, staticApi)
import Api.Chat (ChatApi, chatApi)
import GHC.Generics (Generic)
import Servant ((:>), (:-), Application, Proxy(..), NamedRoutes, serve)

type ApiRoutes = NamedRoutes Api

data Api mode = Api
  { chat   :: mode :- "chat" :> NamedRoutes ChatApi
  , ops    :: mode :- "ops"  :> NamedRoutes PingApi
  , static :: mode :- NamedRoutes StaticApi
  } deriving Generic

apiApp :: Application
apiApp = serve (Proxy @ApiRoutes) api
  where api = Api { chat   = chatApi
                  , ops    = pingApi
                  , static = staticApi
                  }
