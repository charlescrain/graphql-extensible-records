module Language.GraphQL.Extensible.Class where

import Data.Text (Text)
import Data.Proxy (Proxy)

class GraphQLQuery args response | response -> args where
  queryText :: Proxy response -> Text
