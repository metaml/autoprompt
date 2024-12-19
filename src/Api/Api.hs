module Api.Api where

import Api.Chat (ChatApi, chatApi)
import Api.Ping (PingApi, pingApi)
import Api.Prompt (PromptsApi, promptsApi)
import Api.Static (StaticApi, staticApi)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant ( (:>), (:-)
               , Application, Proxy(..), NamedRoutes
               , serve
               )

type ApiRoutes = NamedRoutes Api

data Api mode = Api
  { chat    :: mode :- "chat"   :> NamedRoutes ChatApi
  , ops     :: mode :- "ops"    :> NamedRoutes PingApi
  , prompts :: mode :- "prompts" :> NamedRoutes PromptsApi
  , static  :: mode :- NamedRoutes StaticApi
  } deriving Generic

apiApp :: Application
apiApp = simpleCors $ serve (Proxy @ApiRoutes) api
  where api = Api { chat    = chatApi
                  , ops     = pingApi
                  , prompts = promptsApi
                  , static  = staticApi
                  }
