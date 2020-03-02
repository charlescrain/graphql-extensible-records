module Data.Morpheus.Client.ExtensibleSpec where

import Data.Morpheus.Client.Extensible.TestSchemaScalarTypes
import Data.Morpheus.Client.Extensible


$(buildTypes 
    "./test/assets/graphql/schema.gql" 
    [ "./test/assets/graphql/queryTest.gql"
    , "./test/assets/graphql/mutationTest.gql"
    ]
 )

spec :: IO ()
spec = putStrLn "ExtensibleSpec"