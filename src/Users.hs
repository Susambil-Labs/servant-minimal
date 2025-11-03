{-# LANGUAGE TemplateHaskell #-}

module Users where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Kind (Type)
import Database
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant
import Types

type User :: Type
data User = User
    { username :: Text
    , fullName :: Text
    , email :: Text
    , status :: UserStatus
    }
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type UserId :: Type
newtype UserId = UserId Int64
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

type UserRouters =
    ( Get '[JSON] [User]
        --  :<|> Capture "id" UserId :> Get '[JSON] User
        :<|> ReqBody '[JSON] User :> Post '[JSON] UserId
    )

_FromUserModel :: Iso' User MUser
_FromUserModel = iso userToMUser mUserToUser
  where
    userToMUser :: User -> MUser
    userToMUser (User u f e s) = MUser u f e s

    mUserToUser :: MUser -> User
    mUserToUser (MUser u f e s) = User u f e s

_FromUserId :: Iso' UserId MUserId
_FromUserId = iso (\(UserId i) -> toSqlKey i) (\x -> UserId $ fromSqlKey x)
  where

_create :: (MonadIO m, PoolSql, MonadError ServerError m) => User -> m UserId
_create u = do
    let us = (u ^. _FromUserModel)
    uExists <- liftIO $ getByName us.username
    case uExists of
        Just (_) -> throwError $ err400{errBody = "Bad Request: "}
        Nothing -> do
            uId <- liftIO $ createUser us
            return (uId ^. from _FromUserId)

uList :: (PoolSql) => Handler [User]
uList = do
    x <- liftIO userList
    pure $ (^. from _FromUserModel) <$> x
  where

userApi :: (PoolSql) => Server UserRouters
userApi = (uList :<|> _create)
