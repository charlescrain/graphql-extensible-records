module Language.GraphQL.ExtensibleSpec where

import Language.GraphQL.Extensible.TestSchemaScalarTypes
import Language.GraphQL.Extensible


$(buildTypes 
    "./test/assets/graphql/schema.gql" 
    [ "./test/assets/graphql/queryTest.gql"
    , "./test/assets/graphql/noArgsQuery.gql"
    ]
 )

$(buildTypes 
    "./test/assets/graphql/starwarsSchema.gql" 
    [ "./test/assets/graphql/starwarsQuery.gql"
    ]
 )

spec :: IO ()
spec = putStrLn "ExtensibleSpec"