{-# LANGUAGE AllowAmbiguousTypes #-}

module API where

import Database

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (runReader)
import Control.Monad.State (MonadTrans (lift))
import Data.Kind (Type)
import Data.OpenApi hiding (Server)
import Servant
import Servant.API.Generic
import Servant.OpenApi
import Servant.Server.Generic
import UnliftIO (MonadIO (..))

type API =
    "users"
        :> UserRouters

type UserRouters =
    ( Get '[JSON] [User]
        --  :<|> Capture "id" UserId :> Get '[JSON] User
        :<|> ReqBody '[JSON] User :> Post '[JSON] UserId
    )

type ApiServer = API :<|> SwaggerAPI

apiServer :: Proxy ApiServer
apiServer = Proxy

-------------------------------- Openapi config ----------------------------

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

todoSwagger :: OpenApi
todoSwagger = toOpenApi @API Proxy -- Type error

-------------------------------- Openapi config ----------------------------

uList :: (PoolSql) => Handler [User]
uList = liftIO userList

userApi :: (PoolSql) => Server UserRouters
userApi = (uList :<|> _create)

apiEndpoints :: (PoolSql) => Server API
apiEndpoints = userApi

mkServer :: (PoolSql) => Server ApiServer
mkServer = undefined

app :: (PoolSql) => Application
app = serve apiServer mkServer

_create :: (MonadIO m, PoolSql, MonadError ServerError m) => User -> m UserId
_create u = do
    uExists <- liftIO $ getByName u.username
    case uExists of
        Just (_) -> throwError $ err400{errBody = "Bad Request: "}
        Nothing -> liftIO $ createUser u
