module Language.GraphQL.Extensible
  ( buildTypes
  , module Data.Extensible
  , module Language.GraphQL.Extensible.Class
  , module Language.GraphQL.Extensible.Types
  , makeRelativeToProject
  , FromJSON(..)
  , ToJSON(..)
  )
where

import           Data.Extensible hiding (Nullable)
import           Data.FileEmbed                 ( makeRelativeToProject )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Language.GraphQL.Extensible.TH ( buildTypes )
import           Language.GraphQL.Extensible.Class
import           Language.GraphQL.Extensible.Types
