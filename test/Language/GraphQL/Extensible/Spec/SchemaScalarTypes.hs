module Language.GraphQL.Extensible.Spec.SchemaScalarTypes where

import           Data.Aeson                     ( Value
                                                , ToJSON
                                                , FromJSON
                                                )
import           Data.Text                      ( Text )


newtype BigInt = BigInt {unBigInt :: Integer}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Cursor = Cursor {unCursor :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Datetime = Datetime {unDatetime :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype JSON = JSON {unJSON :: Value}
  deriving (Eq, Show, ToJSON, FromJSON)
