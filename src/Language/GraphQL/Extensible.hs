module Language.GraphQL.Extensible
  ( buildTypes
  , module Data.Extensible
  , makeRelativeToProject
  )
where

import           Data.Extensible
import           Data.FileEmbed                 ( makeRelativeToProject )
import           Language.GraphQL.Extensible.TH ( buildTypes )
