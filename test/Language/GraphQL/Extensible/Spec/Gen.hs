module Language.GraphQL.Extensible.Spec.Gen where

import           Language.GraphQL.Extensible    ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , GraphQLQuery(queryText)
                                                , buildTypes
                                                , type (>:)
                                                )
import           Language.GraphQL.Extensible.Spec.SchemaScalarTypes
                                                ( BigInt
                                                , Datetime
                                                )

$( buildTypes
       "./test/assets/graphql/schema.gql"
       [ "./test/assets/graphql/queryTest.gql"
       , "./test/assets/graphql/noArgsQuery.gql"
       , "./test/assets/graphql/testSameEnumValue.gql"
       ]
 )

$( buildTypes
       "./test/assets/graphql/starwarsSchema.gql"
       [ "./test/assets/graphql/starwarsQuery.gql"
       ]
 )
