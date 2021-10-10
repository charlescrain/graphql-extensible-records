{-# language DeriveLift #-}
module Language.GraphQL.Extensible.Class where

import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Aeson.Types (ToJSON, FromJSON)
import Language.Haskell.TH.Syntax (Lift(..))

class GraphQLQuery args response | response -> args where
    queryText :: Proxy response -> Text

data Schema = Admin | Standard
    deriving (Show, Eq, Lift)

--- | This tells us on what postgres/graphql schema does make sense to run
--- | this query. Only response types have instances of this class.
class (ToJSON res, FromJSON res) => HasSchemaInfo res where
    schemaInfo :: proxy res -> Schema
