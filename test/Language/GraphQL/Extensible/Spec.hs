module Language.GraphQL.Extensible.Spec where

import           Language.GraphQL.Extensible.Spec.Gen
                                                ( NoArgsTestResponse
                                                  ( NoArgsTestResponse
                                                  )
                                                , GetUserArgs(GetUserArgs)
                                                , GetUserResponse
                                                  ( GetUserResponse
                                                  )
                                                , FilmsArgs(FilmsArgs)
                                                , FilmsResponse(FilmsResponse)
                                                )
import           Language.GraphQL.Extensible.Spec.ExpectedTypes
                                                ( FilmsResponseRec
                                                , FilmsArgsRec
                                                , GetUserResponseRec
                                                , GetUserArgsRec
                                                , NoArgsTestResponseRec
                                                )

spec :: IO ()
spec = do
  noArgsQuerySpec
  queryTestSpec
  starwarsQuerySpec

noArgsQuerySpec :: IO ()
noArgsQuerySpec = do
  let _noArgsResp = NoArgsTestResponse (undefined :: NoArgsTestResponseRec)
  pure ()

queryTestSpec :: IO ()
queryTestSpec = do
  let _getUserArgs     = GetUserArgs (undefined :: GetUserArgsRec)
      _getUserResponse = GetUserResponse (undefined :: GetUserResponseRec)
  pure ()

starwarsQuerySpec :: IO ()
starwarsQuerySpec = do
  let _filmsArgs = FilmsArgs (undefined :: FilmsArgsRec)
      _filmsRes  = FilmsResponse (undefined :: FilmsResponseRec)
  pure ()
