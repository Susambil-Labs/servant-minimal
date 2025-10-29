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

import GHC.Generics (Generic)

type Status :: Type
data Status = Male | Female
    deriving stock (Show, Read, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
derivePersistField "Status"

share
    [mkPersist sqlSettings{mpsPrefixFields = False}, mkMigrate "migrateAll"]
    [persistLowerCase|
  User
    username Text
    fullName Text
    email Text
    status Status
|]

type User :: Type

deriving stock instance Show User

deriving stock instance Generic User
deriving anyclass instance ToJSON User
deriving anyclass instance FromJSON User

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

migrate' :: (PoolSql) => IO ()
migrate' = withPool $ runMigration migrateAll
