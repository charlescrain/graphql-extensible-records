
module Language.GraphQL.Extensible.TH where

import           Control.Applicative            ( (<|>) )
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.GraphQL.Draft.Parser
import           Data.Void                      ( Void )
import           GHC.Generics
import           Data.Char                      ( toUpper )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , mapMaybe
                                                , fromMaybe
                                                )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , defaultOptions
                                                , genericToJSON
                                                , genericParseJSON
                                                )
import           System.Directory               ( makeAbsolute )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Extensible                ( Record )
import           Data.List                      ( nub
                                                , find
                                                )

import qualified Language.GraphQL.Draft.Syntax as GQL
import           Control.Monad                  ( when
                                                , void
                                                , forM
                                                )
import           Language.GraphQL.Extensible.Class
import           Data.Aeson.Types               ( Options
                                                  ( constructorTagModifier
                                                  )
                                                )


--------------------------------------------------------------------------------
--- | buildTypes
--------------------------------------------------------------------------------
-- | Construct type declarations for any query args, response, and any types required from the schema
buildTypes :: String -> [String] -> Q [Dec]
buildTypes schemaFilePath queryFilePaths = do
  absolutePaths <- mapM (runIO . makeAbsolute) (schemaFilePath : queryFilePaths)
  mapM_ addDependentFile absolutePaths
  schemaText <- runIO $ T.readFile schemaFilePath
  queryTexts <- runIO $ mapM T.readFile queryFilePaths
  let execDocs = flip map queryTexts $ \queryText' ->
        case parseExecutableDoc queryText' of
          Left  errs -> error $ show errs
          Right res  -> (queryText', res)
      typeSystemDef = case parseTypeSysDefinition schemaText of
        Left  errs -> error $ show errs
        Right res  -> res

  standaloneTypeDecs <-
    case mapM (buildEnumDecs typeSystemDef . snd) execDocs of
      Left  err  -> error $ T.unpack err
      Right decs -> nub <$> (sequence . concat $ decs)

  queryTypes <- sequence $ concatMap errorOnLeft $ sequence $ mapM
    (buildQueryDecs typeSystemDef)
    execDocs

  pure $ standaloneTypeDecs <> queryTypes
 where
  errorOnLeft = \case
    Left  err -> error $ T.unpack err
    Right a   -> a

-- | Construct type declarations for enums used by the query
buildEnumDecs
  :: [GQL.TypeSystemDefinition] -> GQL.ExecutableDocument -> Either Text [DecQ]
buildEnumDecs sd (GQL.ExecutableDocument eds) = do
  rootName <- getRootOperationName sd eds
  rootType <- case lookUpTypeInSchema sd rootName of
    Nothing ->
      Left
        $  "buildEnumDecs: Root Operation `"
        <> GQL.unName rootName
        <> "` not found"
    Just rt -> pure rt
  let (selSets, tods, frags) = GQL.partitionExDefs eds
  selEnums  <- forM selSets (findEnumNameFromSelectionSet rootType)
  todEnums  <- forM tods (findEnumNameFromTypedOperationDefinition rootType)
  fragEnums <- forM frags findEnumNameFromFragmentDefinition
  let enums = nub $ concat $ selEnums <> todEnums <> fragEnums
  mkEnumDefs enums
 where
  findEnumNameFromSelectionSet td =
    fmap concat . mapM (findEnumNamesFromSelection td)
  findEnumNamesFromSelection
    :: GQL.TypeDefinition -> GQL.Selection -> Either Text [GQL.Name]
  findEnumNamesFromSelection td = \case
    GQL.SelectionField fd@GQL.Field {..} -> do
      (td', _) <- case lookupFieldType sd td fd of
        Nothing ->
          Left
            $  "findEnumNamesFromSelection: didn't find field `"
            <> GQL.unName _fName
            <> "` on typedefinion: `"
            <> GQL.unName (getTypeName td)
            <> "`."
        Just td' -> pure td'
      enums <- case td' of
        GQL.TypeDefinitionEnum etd -> pure [GQL._etdName etd]
        _                          -> pure []
      nextEnums <- findEnumNameFromSelectionSet td' _fSelectionSet
      pure $ enums <> nextEnums
    GQL.SelectionFragmentSpread _ ->
      Left "findEnumNamesFromSelection: Fragment spreads not supported"
    GQL.SelectionInlineFragment _ ->
      Left "findEnumNamesFromSelection: Inline fragments not supported"

  findEnumNameFromTypedOperationDefinition
    :: GQL.TypeDefinition
    -> GQL.TypedOperationDefinition
    -> Either Text [GQL.Name]
  findEnumNameFromTypedOperationDefinition td tod = do
    enums    <- findEnumNameFromSelectionSet td (GQL._todSelectionSet tod)
    argEnums <- concat
      <$> mapM getVariableEnum (GQL._todVariableDefinitions tod)
    pure $ enums <> argEnums
  findEnumNameFromFragmentDefinition _ =
    Left "findEnumNameFromFragmentDefinition: Fragments not supported"

  getVariableEnum GQL.VariableDefinition {..} = do
    let name = GQL.unNamedType (GQL.getBaseType _vdType)
    case lookUpTypeInSchema sd name of
      Nothing ->
        Left
          $  "getVariableEnum: Type not found in schema `"
          <> GQL.unName name
          <> "`"
      Just td -> enumLookup td
   where
    enumLookup :: GQL.TypeDefinition -> Either Text [GQL.Name]
    enumLookup = \case
      GQL.TypeDefinitionEnum        etd -> Right [GQL._etdName etd]
      GQL.TypeDefinitionInputObject td  -> do
        let tds = mapMaybe
              ( lookUpTypeInSchema sd
              . GQL.unNamedType
              . GQL.getBaseType
              . GQL._ivdType
              )
              (GQL._iotdValueDefinitions td)
        concat <$> mapM enumLookup tds
      _ -> pure []
  mkEnumDefs :: [GQL.Name] -> Either Text [DecQ]
  mkEnumDefs ns = do
    decs <- forM ns $ \n -> case lookUpTypeInSchema sd n of
      Just (GQL.TypeDefinitionEnum GQL.EnumTypeDefinition {..}) -> do
        let
          typeNameStr       = T.unpack . GQL.unName $ n
          constructorPrefix = typeNameStr <> "_"
          constrs =
            flip map _etdValueDefinitions $ \GQL.EnumValueDefinition {..} ->
              normalC
                ( mkName
                . (<>) constructorPrefix
                . T.unpack
                . GQL.unName
                $ GQL.unEnumValue _evdName
                )
                []
          typeName = mkName typeNameStr
          jsonOptionsExpr
            = [|defaultOptions {constructorTagModifier = drop (length (constructorPrefix :: String))} |]
          toJSON'    = [| genericToJSON $(jsonOptionsExpr) |]
          parseJSON' = [| genericParseJSON $(jsonOptionsExpr) |]
          dec        = dataD
            (cxt [])
            typeName
            []
            Nothing
            constrs
            [derivClause Nothing [conT ''Eq, conT ''Show, conT ''Generic]]
          toJSONInstance = instanceD
            (cxt [])
            (appT (conT ''ToJSON) (conT typeName))
            [funD (mkName "toJSON") [clause [] (normalB toJSON') []]]
          fromJSONInstance = instanceD
            (cxt [])
            (appT (conT ''FromJSON) (conT typeName))
            [funD (mkName "parseJSON") [clause [] (normalB parseJSON') []]]
            -- mkInstance className =
            --   instanceD (cxt []) (appT (conT className) (conT typeName)) []
        pure [dec, toJSONInstance, fromJSONInstance]
      Just _ ->
        Left
          $  "mkEnumDefs: Found non-enum type for name `"
          <> GQL.unName n
          <> "`"
      Nothing ->
        Left $ "mkEnumDefs: Type not found in schema `" <> GQL.unName n <> "`"
    pure $ concat decs

-- | Constructs type declarations for the query args and response types as well as instances for the types. Does not include dependent types such as enums.
buildQueryDecs
  :: [GQL.TypeSystemDefinition]
  -> (Text, GQL.ExecutableDocument)
  -> Either Text [DecQ]
buildQueryDecs schemaDoc (queryText', GQL.ExecutableDocument eds) = do
  rootOpName <- getRootOperationName schemaDoc eds
  case eds of
    [GQL.ExecutableDefinitionOperation opd] -> do
      (responseName, recTypeDecs) <- buildReturnTypeDecs schemaDoc
                                                         rootOpName
                                                         opd
      argInputTypes <- buildArgInputTypeDecs schemaDoc opd
      margDecs <- buildArgTypeDecs schemaDoc opd
      let
        (argName, argDecs) = fromMaybe (''Void, []) margDecs
        gqlInstance        = instanceD
          (cxt [])
          (appT (appT (conT ''GraphQLQuery) (conT argName)) (conT responseName))
          [ funD (mkName "queryText")
                 [clause [wildP] (normalB $ stringE $ T.unpack queryText') []]
          ]
      pure $ recTypeDecs <> argDecs <> [gqlInstance] <> argInputTypes
    [GQL.ExecutableDefinitionFragment _] ->
      Left "buildQueryDecs: Fragments not supported."
    _ -> Left "buildQueryDecs: Only single executable definitions."

-- | Looks up the TypeDefinition and GType
lookupFieldType
  :: [GQL.TypeSystemDefinition]
  -> GQL.TypeDefinition
  -> GQL.Field
  -> Maybe (GQL.TypeDefinition, GQL.GType)
lookupFieldType tsds td GQL.Field {..} = case td of
  GQL.TypeDefinitionObject GQL.ObjectTypeDefinition {..} -> do
    GQL.FieldDefinition {..} <- lookupFieldDefinition _fName
                                                      _otdFieldsDefinition
    t <- lookUpTypeInSchema tsds (GQL.unNamedType $ GQL.getBaseType _fldType)
    pure (t, _fldType)
  GQL.TypeDefinitionInterface GQL.InterfaceTypeDefinition {..} -> do
    GQL.FieldDefinition {..} <- lookupFieldDefinition _fName
                                                      _itdFieldsDefinition
    t <- lookUpTypeInSchema tsds _fldName
    pure (t, _fldType)
  GQL.TypeDefinitionInputObject GQL.InputObjectTypeDefinition {..} -> do
    GQL.InputValueDefinition {..} <- find ((==) _fName . GQL._ivdName)
                                          _iotdValueDefinitions
    t <- lookUpTypeInSchema tsds (GQL.unNamedType $ GQL.getBaseType _ivdType)
    pure (t, _ivdType)
  _ -> Nothing

lookupFieldDefinition
  :: GQL.Name -> [GQL.FieldDefinition] -> Maybe GQL.FieldDefinition
lookupFieldDefinition n = find (\GQL.FieldDefinition {..} -> n == _fldName)

getRootOperationName
  :: [GQL.TypeSystemDefinition]
  -> [GQL.ExecutableDefinition]
  -> Either Text GQL.Name
getRootOperationName sd eds = case GQL.partitionExDefs eds of
  (_, [tod], _) -> do
    let opType = GQL._todType tod
    case lookUpRootOperationTypeDefinition sd opType of
      Nothing ->
        Left
          $  "getRootOperationName: No Root operation found for `"
          <> T.pack (show opType)
          <> "`"
      Just rOpt -> Right . GQL.unNamedType $ GQL._rotdOperationTypeType rOpt
  _ -> Left
    "getRootOperationName: Only single `TypedOperationDefinition` supported."

buildArgTypeDecs
  :: [GQL.TypeSystemDefinition]
  -> GQL.OperationDefinition
  -> Either Text (Maybe (Name, [DecQ]))
buildArgTypeDecs sd od = case od of
  GQL.OperationDefinitionTyped tod -> case GQL._todName tod of
    Nothing           -> Right Nothing
    Just (GQL.Name n) -> do
      let queryName = mkName $ mkUpperWord $ T.unpack (n <> "Args")
      case GQL._todVariableDefinitions tod of
        []      -> pure Nothing
        varDefs -> do
          records <- buildArgRecords sd varDefs
          let
            typeDef = mkNewtypeForRecords queryName records
          Right $ Just (queryName, [typeDef])
  _ -> Right Nothing

buildArgInputTypeDecs
  :: [GQL.TypeSystemDefinition]
  -> GQL.OperationDefinition
  -> Either Text ([DecQ])
buildArgInputTypeDecs sd od = case od of
  GQL.OperationDefinitionTyped tod -> case GQL._todName tod of
    Nothing           -> Right []
    Just _ -> case GQL._todVariableDefinitions tod of
      []      -> pure []
      varDefs -> concat <$> forM varDefs inputTypeDecsFromVarDef
  _ -> Right []
  where
  inputTypeDecsFromVarDef (GQL.VariableDefinition _ n _) = buildInputTypeDecFromGType n

  buildInputTypeDecFromGType (GQL.TypeList _ (GQL.ListType gt))  = buildInputTypeDecFromGType gt
  buildInputTypeDecFromGType (GQL.TypeNamed _ (GQL.NamedType n)) = 
    case lookUpTypeInSchema sd n of
      Nothing -> Left $ "buildArgInputTypeDecs: No type found in schema for: `" <> GQL.unName n <> "`"
      Just (GQL.TypeDefinitionInputObject (GQL.InputObjectTypeDefinition _ typeName _ valDefs )) -> do
        decs <- concat <$> mapM buildInputTypeDecFromGType (map (GQL._ivdType) valDefs)
        dec <- mkNewtypeForRecords (mkName . T.unpack . GQL.unName $ typeName) <$> mapM buildInputTypeRecord valDefs
        pure (dec : decs)
      Just (GQL.TypeDefinitionScalar _) -> pure []
      Just (GQL.TypeDefinitionEnum _) -> pure []
      Just (GQL.TypeDefinitionObject _) -> Left $ "buildArgInputTypeDecs: Object type found in schema for: `" <> GQL.unName n <> "`"
      Just (GQL.TypeDefinitionInterface _) -> Left $ "buildArgInputTypeDecs: Interface type found in schema for: `" <> GQL.unName n <> "`"
      Just (GQL.TypeDefinitionUnion _) -> Left $ "buildArgInputTypeDecs: Union type found in schema for: `" <> GQL.unName n <> "`"
      -- _ -> Left $ "buildArgInputTypeDecs: Non-input type found in schema for: `" <> GQL.unName n <> "`"

  buildInputTypeRecord ivdef = do
    let argName                  = GQL._ivdName ivdef
        recordName = litT (strTyLit $ T.unpack $ GQL.unName argName)
        (GQL.NamedType typeName) = GQL.getBaseType (GQL._ivdType ivdef)
        isOptional               = case GQL._ivdDefaultValue ivdef of
          Nothing -> False
          Just _  -> True
    when isOptional
      $  Left
      $  "buildInputTypeRecord: Default values for arguments not supported. `"
      <> GQL.unName argName
      <> "` has default value"
    typeQ <- case lookUpTypeInSchema sd typeName of
      Nothing ->
        Left
          $  "buildInputTypeRecord: Type not found in schema: "
          <> GQL.unName typeName
      Just _ -> Right $ mkTypeQFromName typeName
    let recordType = wrap (GQL._ivdType ivdef) typeQ
    Right $ uInfixT recordName (mkName ">:") recordType



buildArgRecords
  :: [GQL.TypeSystemDefinition]
  -> [GQL.VariableDefinition]
  -> Either Text [TypeQ]
buildArgRecords sd = mapM buildArgType
 where
  buildArgType vdef = do
    let argName                  = GQL.unVariable $ GQL._vdVariable vdef
        recordName = litT (strTyLit $ T.unpack $ GQL.unName argName)
        (GQL.NamedType typeName) = GQL.getBaseType (GQL._vdType vdef)
        isOptional               = case GQL._vdDefaultValue vdef of
          Nothing -> False
          Just _  -> True
    when isOptional
      $  Left
      $  "Default values for arguments not supported. `"
      <> GQL.unName argName
      <> "` has default value"
    typeQ <- case lookUpTypeInSchema sd typeName of
      Nothing ->
        Left
          $  "buildArgRecords: Type not found in schema: "
          <> GQL.unName typeName
      Just _ -> Right $ mkTypeQFromName typeName
    let recordType = wrap (GQL._vdType vdef) typeQ
    Right $ uInfixT recordName (mkName ">:") recordType

wrap :: GQL.GType -> TypeQ -> TypeQ
wrap gt@(GQL.TypeNamed _ (GQL.NamedType _)) tq = wrapNull gt tq
wrap gt@(GQL.TypeList _ lt) tq =
  wrapNull gt $ wrapList gt $ wrap (GQL.unListType lt) tq

wrapNull :: GQL.GType -> TypeQ -> TypeQ
wrapNull gt = if GQL.isNullable gt then appT (conT ''Maybe) else id

wrapList :: GQL.GType -> TypeQ -> TypeQ
wrapList gt = if GQL.isListType gt then appT listT else id

getTypeName :: GQL.TypeDefinition -> GQL.Name
getTypeName = \case
  GQL.TypeDefinitionScalar      std  -> GQL._stdName std
  GQL.TypeDefinitionObject      otd  -> GQL._otdName otd
  GQL.TypeDefinitionInterface   itd  -> GQL._itdName itd
  GQL.TypeDefinitionUnion       utd  -> GQL._utdName utd
  GQL.TypeDefinitionEnum        etd  -> GQL._etdName etd
  GQL.TypeDefinitionInputObject iotd -> GQL._iotdName iotd

lookUpRootOperationTypeDefinition
  :: [GQL.TypeSystemDefinition]
  -> GQL.OperationType
  -> Maybe GQL.RootOperationTypeDefinition
lookUpRootOperationTypeDefinition tds opt =
  findInSchemaDefinitions <|> findInTypeDefinitions
 where
  findInSchemaDefinitions = listToMaybe $ catMaybes $ flip map tds $ \case
    GQL.TypeSystemDefinitionType   _  -> Nothing
    GQL.TypeSystemDefinitionSchema sd -> find
      (\rotd -> GQL._rotdOperationType rotd == opt)
      (GQL._sdRootOperationTypeDefinitions sd)
  findInTypeDefinitions = do
    let name = case opt of
          GQL.OperationTypeQuery        -> GQL.Name "Query"
          GQL.OperationTypeMutation     -> GQL.Name "Mutation"
          GQL.OperationTypeSubscription -> GQL.Name "Subscription"
    void $ lookUpTypeInSchema tds name
    Just $ GQL.RootOperationTypeDefinition opt (GQL.NamedType name)

lookUpTypeInSchema
  :: [GQL.TypeSystemDefinition] -> GQL.Name -> Maybe GQL.TypeDefinition
lookUpTypeInSchema _ n@(GQL.Name "String") =
  pure $ GQL.TypeDefinitionScalar $ GQL.ScalarTypeDefinition Nothing n []
lookUpTypeInSchema _ n@(GQL.Name "Float") =
  pure $ GQL.TypeDefinitionScalar $ GQL.ScalarTypeDefinition Nothing n []
lookUpTypeInSchema _ n@(GQL.Name "Int") =
  pure $ GQL.TypeDefinitionScalar $ GQL.ScalarTypeDefinition Nothing n []
lookUpTypeInSchema _ n@(GQL.Name "Boolean") =
  pure $ GQL.TypeDefinitionScalar $ GQL.ScalarTypeDefinition Nothing n []
lookUpTypeInSchema tds n = listToMaybe $ catMaybes $ flip map tds $ \case
  GQL.TypeSystemDefinitionSchema _ -> Nothing
  GQL.TypeSystemDefinitionType td ->
    if getTypeName td == n then Just td else Nothing

buildReturnTypeDecs
  :: [GQL.TypeSystemDefinition]
  -> GQL.Name
  -> GQL.OperationDefinition
  -> Either Text (Name, [DecQ])
buildReturnTypeDecs tds rootName opd = do
  (selset, GQL.Name n) <- case opd of
    GQL.OperationDefinitionTyped GQL.TypedOperationDefinition { _todName = Just n, _todSelectionSet = ss }
      -> pure (ss, n)
    _ -> Left "buildReturnTypeDec: Only TypedOpertions with names supported."
  rootTd <- case lookUpTypeInSchema tds rootName of
    Nothing ->
      Left
        $  "buildReturnTypeDec: Root TypeDefinition not found for `"
        <> GQL.unName rootName
        <> "`"
    Just td -> pure td
  typeQs <-  mapM (buildFromSel rootTd) selset
  let responseName = mkName $ mkUpperWord $ T.unpack (n <> "Response")
      retTypeDec   = mkNewtypeForRecords responseName typeQs
  pure (responseName, [retTypeDec])
 where
  buildFromSel :: GQL.TypeDefinition -> GQL.Selection -> Either Text TypeQ
  buildFromSel td = \case
    GQL.SelectionField fd@GQL.Field {..} -> do
      let recName = GQL.unName $ maybe _fName GQL.unAlias _fAlias
      typeq <- case lookupFieldType tds td fd of
        Just (GQL.TypeDefinitionScalar std, gt) ->
          pure . wrap gt . mkTypeQFromName $ GQL._stdName std
        Just (GQL.TypeDefinitionEnum etd, gt) ->
          pure . wrap gt . mkTypeQFromName $ GQL._etdName etd
        Just (GQL.TypeDefinitionInputObject _, _) ->
          Left
            $  "InputType found as type for field `"
            <> GQL.unName _fName
            <> "`"
        Just (td', gt) ->
          wrap gt . mkRecord <$> mapM (buildFromSel td') _fSelectionSet
        Nothing ->
          Left $ "No type found for field `" <> GQL.unName _fName <> "`"

      pure $ addRecord recName typeq

    _ -> Left "buildFromSel: Only Fields supported."

mkNewtypeForRecords :: Name -> [TypeQ] -> DecQ
mkNewtypeForRecords n  records =  newtypeD
        (cxt [])
        n
        []
        Nothing
        (normalC
          n
          [ bangType (bang noSourceUnpackedness noSourceStrictness)
                     (mkRecord records)
          ]
        )
        [ derivClause
            Nothing
            [ conT ''Eq
            , conT ''Show
            , conT ''Generic
            , conT ''ToJSON
            , conT ''FromJSON
            ]
        ]

gqlNameToName :: GQL.Name -> Name
gqlNameToName = mkName . T.unpack . GQL.unName

promotedTypeList :: [Q Type] -> Q Type
promotedTypeList []       = promotedNilT
promotedTypeList (t : ts) = [t| $promotedConsT $t $(promotedTypeList ts) |]

addRecord :: Text -> TypeQ -> TypeQ
addRecord name recordType =
  let recordName = litT (strTyLit $ T.unpack name)
  in  uInfixT recordName (mkName ">:") recordType

mkRecord :: [TypeQ] -> TypeQ
mkRecord = appT (conT ''Record) . promotedTypeList

mkUpperWord :: String -> String
mkUpperWord []       = []
mkUpperWord (x : xs) = toUpper x : xs

mkTypeQFromName :: GQL.Name -> TypeQ
mkTypeQFromName = \case
  (GQL.Name "String" ) -> conT ''Text
  (GQL.Name "Float"  ) -> conT ''Double
  (GQL.Name "Int"    ) -> conT ''Int
  (GQL.Name "Boolean") -> conT ''Bool
  n                    -> conT . mkName . T.unpack . GQL.unName $ n
