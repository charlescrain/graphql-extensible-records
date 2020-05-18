module Language.GraphQL.Extensible.Types where

import Data.Text (Text)
import qualified Data.Aeson as A
import GHC.Generics (Generic)

data Nullable a = Null | NonNull a
  deriving (Eq, Show, Generic)

instance Functor Nullable where
  fmap _ Null = Null
  fmap f (NonNull x) = NonNull (f x)

instance A.ToJSON a => A.ToJSON (Nullable a) where
  toJSON Null        = A.toJSON ("null" :: Text)
  toJSON (NonNull a) = A.toJSON a

instance A.FromJSON a => A.FromJSON (Nullable a) where
  parseJSON A.Null = pure Null
  parseJSON v      = NonNull <$> A.parseJSON v
