module Language.GraphQL.Extensible
  ( buildTypes
  , module Data.Extensible
  ) where

import Data.Extensible 
import Language.GraphQL.Extensible.TH (buildTypes)