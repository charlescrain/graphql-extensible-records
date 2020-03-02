module Data.Morpheus.Client.Extensible.TestSchemaScalarTypes where

import Data.Aeson (Value)
import Data.Text (Text)


newtype BigInt = BigInt {unBigInt :: Integer}
  deriving (Eq, Show)

newtype Cursor = Cursor {unCursor :: Text}
  deriving (Eq, Show)

newtype Datetime = Datetime {unDatetime :: Text}
  deriving (Eq, Show)

newtype JSON = JSON {unJSON :: Value}
  deriving (Eq, Show)