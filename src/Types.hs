module Types where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Data (Typeable)
import Data.Kind (Type)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

type UserStatus :: Type
data UserStatus = Active | Blocked
    deriving stock (Show, Read, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, Typeable, ToSchema)
