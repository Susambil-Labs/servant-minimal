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
import GHC.Generics (Generic)
import Types

derivePersistField "UserStatus"

share
    [mkPersist sqlSettings{mpsPrefixFields = False}, mkMigrate "migrateAll"]
    [persistLowerCase|
  MUser
    username Text
    fullName Text
    email Text
    status UserStatus
    deriving Eq Show
|]

type MUser :: Type

deriving stock instance Generic MUser
deriving stock instance Typeable MUser
deriving anyclass instance ToJSON MUser
deriving anyclass instance FromJSON MUser

type MUserId :: Type
deriving stock instance Generic MUserId
deriving stock instance Typeable MUserId

-- >>>  MUserId 1
-- Couldn't match expected type: t0_a6ooI[tau:1] -> t_a6ooK[sk:1]
--             with actual type: EntityField MUser MUserId
-- The function `MUserId' is applied to one visible argument,
--   but its type `(typ ~ MUserId) => EntityField MUser typ' has none
-- In the expression: MUserId 1
-- In an equation for `it_a6ona': it_a6ona = MUserId 1
-- Relevant bindings include
--   it_a6ona :: t_a6ooK[sk:1]
--     (bound at /Users/lambdajon/expriements/servant-minimal/src/Database.hs:49:3)

----------------------------------- OpenAPI schema -----------------------------
-- deriving anyclass instance ToSchema MUser

-- instance (ToParamSchema (BackendKey SqlBackend)) => ToParamSchema MUserId
-- instance (ToSchema (BackendKey SqlBackend)) => ToSchema MUserId

----------------------------------- OpenAPI schema -----------------------------

type PoolSql :: Constraint
type PoolSql = (?pool :: Pool SqlBackend)

withPool :: (?pool :: Pool s) => ReaderT s IO r -> IO r
withPool = withResource ?pool . runReaderT

migrate' :: (PoolSql) => IO ()
migrate' = withPool $ runMigration migrateAll

createUser :: (PoolSql) => MUser -> IO MUserId
createUser = withPool . insert

userList :: (PoolSql) => IO [MUser]
userList = map entityVal <$> withPool (select $ from table)

oneUser :: (PoolSql) => MUserId -> IO (Maybe MUser)
oneUser _id =
    fmap entityVal <$> withPool do
        selectOne do
            p <- from table
            where_ (p ^. MUserId ==. val _id)
            pure p

getByName :: (PoolSql) => Text -> IO (Maybe MUser)
getByName nm =
    fmap entityVal <$> withPool do
        selectOne do
            p <- from table
            where_ ((p ^. Username) ==. val nm)
            pure p
