{-# LANGUAGE AllowAmbiguousTypes #-}

module API where

import Database

import Data.Kind (Type)
import Data.OpenApi hiding (Server)
import Servant
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Swagger
import UnliftIO (MonadIO (..))
import Users

import Control.Lens
import Data.Text qualified as T
import Servant.OpenApi
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8

type API :: Type -> Type
data API route = MkAPI
  { users :: route :- "users" :> NamedRoutes UserRoutes
  }
  deriving (Generic)

instance HasOpenApi API where
  toOpenApi :: Proxy API -> OpenApi
  toOpenApi _ = swaggerDocs

data ApiServer route = MkApiServer
  { api :: route :- NamedRoutes API
  , docs :: route :- SwaggerAPI
  , swg :: route :- "dc" :> Get '[JSON] OpenApi
  }
  deriving (Generic)

apiServer :: Proxy ApiServer
apiServer = Proxy

endP :: Proxy (ToServantApi API)
endP = Proxy

-------------------------------- Openapi config ----------------------------

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDocs :: OpenApi
-- swaggerDocs = toSwagger endP & mempty swaggerExample
swaggerDocs =
  toOpenApi endP
    & info
      .~ ( mempty
             & title .~ "Servant demo API"
             & license ?~ "MIT"
             & contact
               ?~ ( mempty
                      & name ?~ "API Support"
                      & url ?~ URL "http://www.swagger.io/support"
                  )
             & description ?~ "This is a an API that tests servant-swagger support for a Todo API"
             & version .~ "1.0"
         )

-------------------------------- Openapi config ----------------------------

apiEndpoints :: (PoolSql) => API AsServer
apiEndpoints = MkAPI{users = userApi}

mkServer :: (PoolSql) => ApiServer AsServer
mkServer =
  MkApiServer
    { api = apiEndpoints
    , docs = swaggerSchemaUIServer swaggerDocs
    , swg = pure swaggerDocs
    }

app :: (PoolSql) => Application
app = genericServe mkServer

writeSwaggerJSON = BL8.writeFile "data/swagger.json" (encode swaggerDocs)
