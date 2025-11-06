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
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.ByteString.UTF8 qualified as UTF8
import Data.Text (Text)
import Network.HTTP.Media.RenderHeader qualified as HTTPMedia
import Network.HTTP.Types qualified as HTTP

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
app =
  serveWithContext
    (Proxy @(ToServantApi ApiServer))
    millewares
    (toServant mkServer)

millewares :: (PoolSql) => Servant.Server.Context '[ErrorFormatters]
millewares = (customFormatters :. EmptyContext)

customFormatters :: Servant.ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserErrorFormatter'
    }

bodyParserErrorFormatter' :: ErrorFormatter
bodyParserErrorFormatter' _ _ errMsg =
  Servant.ServerError
    { Servant.errHTTPCode = HTTP.statusCode HTTP.status400
    , Servant.errReasonPhrase = UTF8.toString $ HTTP.statusMessage HTTP.status400
    , Servant.errBody =
        Aeson.encode $
          Aeson.object
            [ "code" Aeson..= Aeson.Number 400
            , "message" Aeson..= errMsg
            , "label" Aeson..= ("bad-request" :: Text)
            ]
    , Servant.errHeaders = [(HTTP.hContentType, HTTPMedia.renderHeader (Servant.contentType (Proxy @Servant.JSON)))]
    }
writeSwaggerJSON = BL8.writeFile "data/swagger.json" (encode swaggerDocs)
