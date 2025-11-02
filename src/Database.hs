{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Control.Monad.Reader (ReaderT, runReaderT)

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Constraint, Type)
import Data.Pool (Pool, withResource)
import Data.Text (Text)

import Database.Esqueleto.Experimental
import Database.Persist.TH

import Data.Data (Typeable)
import Data.OpenApi hiding (Server)
import GHC.Generics (Generic)
import Servant.OpenApi

type Status :: Type
data Status = Male | Female
    deriving stock (Show, Read, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, Typeable, ToSchema)
derivePersistField "Status"

share
    [mkPersist sqlSettings{mpsPrefixFields = False}, mkMigrate "migrateAll"]
    [persistLowerCase|
  User
    username Text
    fullName Text
    email Text
    status Status
    deriving Eq Show
|]

type User :: Type

deriving stock instance Generic User
deriving stock instance Typeable User
deriving anyclass instance ToJSON User
deriving anyclass instance FromJSON User

type UserId :: Type
deriving stock instance Generic UserId
deriving stock instance Typeable UserId

----------------------------------- OpenAPI schema -----------------------------
deriving anyclass instance ToSchema User

instance (ToParamSchema (BackendKey SqlBackend)) => ToParamSchema UserId
instance (ToSchema (BackendKey SqlBackend)) => ToSchema UserId

----------------------------------- OpenAPI schema -----------------------------

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

migrate' :: (PoolSql) => IO ()
migrate' = withPool $ runMigration migrateAll

createUser :: (PoolSql) => User -> IO UserId
createUser = withPool . insert

userList :: (PoolSql) => IO [User]
userList = map entityVal <$> withPool (select $ from table)

oneUser :: (PoolSql) => UserId -> IO (Maybe User)
oneUser _id =
    fmap entityVal <$> withPool do
        selectOne do
            p <- from table
            where_ (p ^. UserId ==. val _id)
            pure p

getByName :: (PoolSql) => Text -> IO (Maybe User)
getByName nm =
    fmap entityVal <$> withPool do
        selectOne do
            p <- from table
            where_ ((p ^. Username) ==. val nm)
            pure p
