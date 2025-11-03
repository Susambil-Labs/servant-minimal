{-# LANGUAGE AllowAmbiguousTypes #-}

module API where

import Database

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (runReader)
import Control.Monad.State (MonadTrans (lift))
import Data.Kind (Type)
import Data.Swagger hiding (Server)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger
import UnliftIO (MonadIO (..))
import Users

type API =
    "users"
        :> UserRouters

type ApiServer = API :<|> SwaggerAPI

apiServer :: Proxy ApiServer
apiServer = Proxy

-------------------------------- Openapi config ----------------------------

-- type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi
type SwaggerAPI = "docs" :> Get '[JSON] Swagger

swaggerDocs :: Swagger
swaggerDocs = toSwagger @API Proxy -- Type error

-------------------------------- Openapi config ----------------------------

-- uList :: (PoolSql) => Handler [User]
-- uList = liftIO userList

-- userApi :: (PoolSql) => Server UserRouters
-- userApi = (uList :<|> _create)

apiEndpoints :: (PoolSql) => Server API
apiEndpoints = userApi

endP :: Proxy API
endP = Proxy

mkServer :: (PoolSql) => Server ApiServer
mkServer = apiEndpoints :<|> pure swaggerDocs

-- app :: (PoolSql) => Application
-- app = serve endP apiEndpoints

app :: (PoolSql) => Application
app = serve apiServer mkServer

-- _create :: (MonadIO m, PoolSql, MonadError ServerError m) => User -> m UserId
-- _create u = do
--     uExists <- liftIO $ getByName u.username
--     case uExists of
--         Just (_) -> throwError $ err400{errBody = "Bad Request: "}
--         Nothing -> liftIO $ createUser u
