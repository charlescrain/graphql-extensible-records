module Language.GraphQL.Extensible
  ( buildTypes
  , module Data.Extensible
  , makeRelativeToProject
  , FromJSON(..)
  , ToJSON(..)
  )
where

import           Data.Extensible
import           Data.FileEmbed                 ( makeRelativeToProject )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Language.GraphQL.Extensible.TH ( buildTypes )
