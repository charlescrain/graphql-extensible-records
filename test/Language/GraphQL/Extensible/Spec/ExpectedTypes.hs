module Language.GraphQL.Extensible.Spec.ExpectedTypes where

import           Language.GraphQL.Extensible    ( Record
                                                , type (>:)
                                                )
import           Language.GraphQL.Extensible.Spec.Gen
                                                ( UsersOrderBy )
import           Language.GraphQL.Extensible.Spec.SchemaScalarTypes
                                                ( BigInt )
import           Data.Text                      ( Text )

type NoArgsTestResponseRec = Record 
    '[ "user" >: Maybe 
      ( Record 
        '[ "nodes" >: 
          [ Maybe 
            ( Record 
              '["email" >: Text
              , "id" >: BigInt
              , "settings" >: Maybe 
                  (Record '["emailGiftReceiver" >: Bool])
              ]
            )
          ]
         ]
      )
     ]

type GetUserArgsRec = Record '["address" >: Text, "orderBy" >: Maybe [UsersOrderBy]]
type GetUserResponseRec = Record 
  '[ "user" >: Maybe
      (Record 
        '["nodes" >: 
            [ Maybe 
              (Record 
                '[ "email" >: Text
                 , "id">: BigInt
                 , "settings">: Maybe (Record '["emailGiftReceiver" >: Bool])
                 ]
              )
            ]
         ]
      )
   ]

type FilmsArgsRec = Record '["firstFilms" >: Int, "firstStarships" >: Int]
type FilmsResponseRec = Record 
  '["allFilms" >: Maybe
    (Record
     '["pageInfo" >: Record '["hasNextPage" >: Bool, "endCursor" >: Maybe Text]
      , "edges" >: Maybe
        [Maybe
           (Record
              '["node" >: Maybe
                (Record
                   '["title" >: Maybe Text
                    , "openingCrawl" >: Maybe Text
                    , "releaseDate" >: Maybe Text
                    , "starshipConnection" >: Maybe
                      (Record
                         '["edges" >: Maybe
                            [Maybe
                               (Record
                                  '["node" >: Maybe
                                      (Record
                                         '["name" >: Maybe Text
                                          , "model" >: Maybe Text
                                          , "starshipClass" >: Maybe Text
                                          ]
                                      )
                                   ]
                               )
                            ]
                          ]
                      )
                    ]
                )
               ]
            )
        ]
      ]
    )
   ]
