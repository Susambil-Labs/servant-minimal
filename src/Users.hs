{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Users where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.OpenApi (HasSchema (schema), HasTitle (title), NamedSchema, Schema, ToParamSchema, ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API

import Control.Lens
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.OpenApi.Internal.Schema
import Data.OpenApi.Schema
import Database
import Database.Persist
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Generics.SOP qualified as GSOP
import Servant
import Servant.API.MultiVerb
import Servant.API.Verbs
import Servant.OpenApi
import Servant.Server.Generic (AsServer)
import Types

import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.RWS (MonadTrans (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Lazy qualified as BLS

newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving stock (Functor)
  deriving anyclass (MonadTrans)

deriving anyclass instance (Applicative a) => Applicative (UVerbT xs a)

deriving anyclass instance (Monad m) => Monad (UVerbT xs m)

-- (>>=) :: (Monad m) => UVerbT xs m a -> (a -> UVerbT xs m b) -> UVerbT xs m b
-- x >>= f = let a = unUVerbT x in undefined
-- where
deriving anyclass instance (MonadIO m) => MonadIO (UVerbT xs m)

-- liftIO a = do
--   b <- liftIO a
--   return b

instance (MonadError e m) => MonadError e (UVerbT xs m) where
  throwError = lift . throwError
  catchError (UVerbT act) h =
    UVerbT $
      ExceptT $
        runExceptT act `catchError` (runExceptT . unUVerbT . h)

{- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
may use the usual 'return'.
-}
runUVerbT :: (PoolSql, Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb :: (PoolSql, Monad m, HasStatus x, IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond

class Evaluable c where
  type Return c :: Type

type User :: Type
data User = User
  { username :: Text
  , fullName :: Text
  , email :: Text
  , status :: UserStatus
  }
  deriving stock (Show, Eq, Generic, Typeable)
  -- deriving anyclass (ToJSON, FromJSON, ToSchema)
  deriving anyclass (ToJSON, FromJSON)

type UserId :: Type
newtype UserId = UserId Int64
  deriving stock (Show, Eq, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)
instance ToSchema User where
  declareNamedSchema proxy = hp proxy & mapped . schema . title ?~ "User schema"
   where
    hp = genericDeclareNamedSchema defaultSchemaOptions

data UserRoutes route = MkUserRoutes
  { list :: route :- Get '[JSON] [MUser]
  , create :: route :- CreateUser
  }
  deriving (Generic)

type CreateUser = ReqBody '[JSON] MUser :> UVerb 'POST '[JSON] ('[BadRequest, WithStatus 201 MUserId])
type CreateUserResponse = Union '[BadRequest, WithStatus 201 MUserId]

data BadRequest = BadRequest {error :: String}
  deriving (Eq, Show, Generic)

instance ToJSON BadRequest
instance FromJSON BadRequest
instance ToSchema BadRequest

instance HasStatus BadRequest where
  type StatusOf BadRequest = 400

_FromUserModel :: Iso' User MUser
_FromUserModel = iso userToMUser mUserToUser
 where
  userToMUser :: User -> MUser
  userToMUser (User u f e s) = MUser u f e s

  mUserToUser :: MUser -> User
  mUserToUser (MUser u f e s) = User u f e s

_FromUserId :: Iso' UserId MUserId
_FromUserId = iso (\(UserId i) -> toSqlKey i) (\x -> UserId $ fromSqlKey x)

_create :: (MonadIO m, PoolSql, MonadError ServerError m) => User -> m UserId
_create u = do
  let us = (u ^. _FromUserModel)
  uExists <- liftIO $ getByName us.username
  case uExists of
    Just (_) -> throwError $ err400{errBody = "Bad Request: "}
    Nothing -> do
      uId <- liftIO $ createUser us
      return (uId ^. from _FromUserId)

create1 :: (PoolSql) => MUser -> Handler (CreateUserResponse)
create1 u = runUVerbT $ do
  uExists <- liftIO $ getByName u.username
  when (isJust uExists) $ throwUVerb BadRequest{error = "fuck you"}

  uId <- liftIO $ createUser u
  return $ WithStatus @201 uId

uuList :: (PoolSql) => Handler [MUser]
uuList = liftIO userList

uList :: (PoolSql) => Handler [MUser]
uList = do
  x <- liftIO userList
  pure x
 where

userApi :: (PoolSql) => UserRoutes AsServer
userApi =
  MkUserRoutes
    { list = uList
    , create = create1
    }
