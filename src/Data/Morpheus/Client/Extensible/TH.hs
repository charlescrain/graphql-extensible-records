{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Data.Morpheus.Client.Extensible.TH where

import           Data.Either                    ( lefts
                                                , rights
                                                )
import           Control.Applicative            ( (<|>) )
import           Language.Haskell.TH
import           Language.GraphQL.Draft.Parser

-- import           Data.Morpheus.Parsing.Document.TypeSystem
-- import           Data.Morpheus.Types.Internal.Resolving.Core
-- import           Data.Morpheus.Document
-- import           Data.Morpheus.Types.IO  hiding ( operationName )
-- import           Data.Morpheus.Parsing.Request.Parser

import           Data.Char                      ( toUpper )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Extensible                ( Record )
import           Data.List                      ( nub
                                                , find
                                                )
import           Language.GraphQL.Draft.Syntax  ( unVariable
                                                , SchemaDocument(..)
                                                , ExecutableDefinition(..)
                                                , TypedOperationDefinition(..)
                                                , OperationDefinition(..)
                                                , ExecutableDocument(..)
                                                , getExecutableDefinitions
                                                , VariableDefinition(..)
                                                , GType(..)
                                                , Nullability(..)
                                                , isNullable
                                                , isListType
                                                , TypeDefinition(..)
                                                , TypeSystemDefinition(..)
                                                , SchemaDefinition(..)
                                                )

import qualified Language.GraphQL.Draft.Syntax as GQL
import           Control.Monad                  ( void
                                                , forM
                                                )
-- operationTypeToSchemaTypeName :: OperationType -> Text
-- operationTypeToSchemaTypeName = \case
--   Query        -> "Query"
--   Subscription -> "Subscription"
--   Mutation     -> "Mutation"

data Nullable a = Null | NonNull a

buildTypes :: String -> [String] -> Q [Dec]
buildTypes schemaFilePath queryFilePaths = do
  schemaText <- runIO $ T.readFile schemaFilePath
  queryTexts <- runIO $ mapM T.readFile queryFilePaths
  let execDocs = flip map queryTexts $ \queryText ->
        case parseExecutableDoc queryText of
          Left  errs -> error $ show errs
          Right res  -> res
      typeSystemDef = case parseTypeSysDefinition schemaText of
        Left  errs -> error $ show errs
        Right res  -> res

  standaloneTypeDecs <- case mapM (buildEnumDecs typeSystemDef) execDocs of
    Left  err  -> error $ T.unpack err
    Right decs -> nub <$> (sequence . concat $ decs)

  queryTypes <- sequence $ concat $ rights $ sequence $ mapM
    (buildTypesForQuery typeSystemDef)
    execDocs

  pure $ standaloneTypeDecs <> queryTypes
 where
  buildTypesForQuery schemaDoc execDoc@(ExecutableDocument eds) = do
    let typedOps = catMaybes $ flip map eds $ \case
          ExecutableDefinitionOperation (OperationDefinitionTyped tod) ->
            Just tod
          _ -> Nothing
    argTypeDecs    <- mapM (buildArgTypeDecs' schemaDoc) typedOps
    returnTypeDecs <- buildReturnTypeDec' schemaDoc execDoc
    pure (argTypeDecs <> [returnTypeDecs])


buildEnumDecs
  :: [TypeSystemDefinition] -> ExecutableDocument -> Either Text [DecQ]
buildEnumDecs sd (ExecutableDocument eds) = do
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
      td' <- case lookupFieldType sd td fd of
        Nothing ->
          Left
            $  "findEnumNamesFromSelection: didn't find field `"
            <> GQL.unName _fName
            <> "` on typedefinion: \n"
            <> T.pack (show td)
        Just td' -> pure td'
      enums <- case td' of
        TypeDefinitionEnum etd -> pure [GQL._etdName etd]
        _                      -> pure []
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
    enums    <- findEnumNameFromSelectionSet td (_todSelectionSet tod)
    argEnums <- concat <$> mapM getVariableEnum (_todVariableDefinitions tod)
    pure $ enums <> argEnums
  findEnumNameFromFragmentDefinition _ =
    Left "findEnumNameFromFragmentDefinition: Fragments not supported"

  getVariableEnum VariableDefinition {..} = do
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
      TypeDefinitionEnum        etd -> Right [GQL._etdName etd]
      TypeDefinitionInputObject td  -> do
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
  mkEnumDefs = mapM $ \n -> case lookUpTypeInSchema sd n of
    Just (TypeDefinitionEnum GQL.EnumTypeDefinition {..}) -> do
      let constrs =
            flip map _etdValueDefinitions $ \GQL.EnumValueDefinition {..} ->
              normalC
                (mkName . T.unpack . GQL.unName $ GQL.unEnumValue _evdName)
                []
      pure $ dataD (cxt [])
                   (mkName . T.unpack . GQL.unName $ n)
                   []
                   Nothing
                   constrs
                   [derivClause Nothing [equalityT, conT ''Show]]
    Just _ ->
      Left $ "mkEnumDefs: Found non-enum type for name `" <> GQL.unName n <> "`"
    Nothing ->
      Left $ "mkEnumDefs: Type not found in schema `" <> GQL.unName n <> "`"



lookupFieldType
  :: [GQL.TypeSystemDefinition]
  -> GQL.TypeDefinition
  -> GQL.Field
  -> Maybe GQL.TypeDefinition
lookupFieldType tsds td GQL.Field {..} = case td of
  TypeDefinitionObject GQL.ObjectTypeDefinition {..} -> do
    GQL.FieldDefinition {..} <- lookupFieldDefinition _fName
                                                      _otdFieldsDefinition
    lookUpTypeInSchema tsds _fldName
  TypeDefinitionInterface GQL.InterfaceTypeDefinition {..} -> do
    GQL.FieldDefinition {..} <- lookupFieldDefinition _fName
                                                      _itdFieldsDefinition
    lookUpTypeInSchema tsds _fldName
  TypeDefinitionUnion GQL.UnionTypeDefinition {..} -> do
    GQL.NamedType n <- find ((==) _fName . GQL.unNamedType) _utdMemberTypes
    lookUpTypeInSchema tsds n
  TypeDefinitionInputObject GQL.InputObjectTypeDefinition {..} -> do
    GQL.InputValueDefinition {..} <- find ((==) _fName . GQL._ivdName)
                                          _iotdValueDefinitions
    lookUpTypeInSchema tsds (GQL.unNamedType $ GQL.getBaseType _ivdType)
  _ -> Nothing

lookupFieldDefinition
  :: GQL.Name -> [GQL.FieldDefinition] -> Maybe GQL.FieldDefinition
lookupFieldDefinition n = find (\GQL.FieldDefinition {..} -> n == _fldName)


getRootOperationName
  :: [TypeSystemDefinition] -> [ExecutableDefinition] -> Either Text GQL.Name
getRootOperationName sd eds = case GQL.partitionExDefs eds of
  (_, [tod], _) -> do
    let opType = _todType tod
    case lookUpRootOperationTypeDefinition sd opType of
      Nothing ->
        Left
          $  "getRootOperationName: No Root operation found for `"
          <> T.pack (show opType)
          <> "`"
      Just rOpt -> Right . GQL.unNamedType $ GQL._rotdOperationTypeType rOpt
  _ -> Left
    "getRootOperationName: Only single `TypedOperationDefinition` supported."


buildArgTypeDecs'
  :: [TypeSystemDefinition] -> TypedOperationDefinition -> Either Text DecQ
buildArgTypeDecs' sd tod = case _todName tod of
  Nothing           -> Left "buildArgTypeDecs: Unnamed Queries not permitted"
  Just (GQL.Name n) -> do
    let queryName = mkName $ mkUpperWord $ T.unpack (n <> "Args")
        varDefs   = _todVariableDefinitions tod
    records <- mkRecord <$> buildArgRecords' sd varDefs
    Right $ tySynD queryName [] records

buildArgRecords'
  :: [TypeSystemDefinition] -> [VariableDefinition] -> Either Text [TypeQ]
buildArgRecords' sd = mapM buildArgType
 where
  buildArgType vdef = do
    let argName                  = unVariable $ _vdVariable vdef
        recordName = litT (strTyLit $ T.unpack $ GQL.unName argName)
        (GQL.NamedType typeName) = GQL.getBaseType (_vdType vdef)
        isOptional               = case _vdDefaultValue vdef of
          Nothing -> False
          Just _  -> True
        wrapMaybe = if isOptional then appT (conT ''Maybe) else id
    typeQ <- case lookUpPrimitiveTypeQ typeName of
      Nothing -> case lookUpTypeInSchema sd typeName of
        Nothing ->
          Left
            $  "buildArgRecords: Type not found in schema: "
            <> GQL.unName typeName
        Just _ -> Right $ mkTypeQFromName typeName
      Just tq -> Right tq
    case lookUpPrimitiveTypeQ typeName of
      Just tq -> Right $ uInfixT recordName (mkName ">:") tq
      Nothing -> case lookUpTypeInSchema sd typeName of
        Nothing ->
          Left
            $  "buildArgRecords: Type not found in schema: "
            <> GQL.unName typeName
        Just _ ->
          let recordType = wrapMaybe $ wrap (_vdType vdef) typeQ
          in  Right $ uInfixT recordName (mkName ">:") recordType
  wrap gt@(GQL.TypeNamed _ (GQL.NamedType _)) tq = wrapNull gt tq
  wrap gt@(GQL.TypeList _ lt) tq =
    wrapNull gt $ wrapList gt $ wrap (GQL.unListType lt) tq
  wrapNull gt = if isNullable gt then appT (conT ''Nullable) else id
  wrapList gt = if isListType gt then appT listT else id

getTypeName :: TypeDefinition -> GQL.Name
getTypeName = \case
  TypeDefinitionScalar      std  -> GQL._stdName std
  TypeDefinitionObject      otd  -> GQL._otdName otd
  TypeDefinitionInterface   itd  -> GQL._itdName itd
  TypeDefinitionUnion       utd  -> GQL._utdName utd
  TypeDefinitionEnum        etd  -> GQL._etdName etd
  TypeDefinitionInputObject iotd -> GQL._iotdName iotd

lookUpRootOperationTypeDefinition
  :: [TypeSystemDefinition]
  -> GQL.OperationType
  -> Maybe GQL.RootOperationTypeDefinition
lookUpRootOperationTypeDefinition tds opt =
  listToMaybe $ catMaybes $ flip map tds $ \case
    TypeSystemDefinitionType _ -> Nothing
    TypeSystemDefinitionSchema sd ->
      find (\rotd -> GQL._rotdOperationType rotd == opt)
           (_sdRootOperationTypeDefinitions sd)
        <|> do
              let name = case opt of
                    GQL.OperationTypeQuery        -> GQL.Name "Query"
                    GQL.OperationTypeMutation     -> GQL.Name "Mutation"
                    GQL.OperationTypeSubscription -> GQL.Name "Subscription"
              void $ lookUpTypeInSchema tds name
              Just $ GQL.RootOperationTypeDefinition opt (GQL.NamedType name)


lookUpTypeInSchema :: [TypeSystemDefinition] -> GQL.Name -> Maybe TypeDefinition
lookUpTypeInSchema tds n = listToMaybe $ catMaybes $ flip map tds $ \case
  TypeSystemDefinitionSchema _ -> Nothing
  TypeSystemDefinitionType td ->
    if getTypeName td == n then Just td else Nothing

buildReturnTypeDec'
  :: [TypeSystemDefinition] -> ExecutableDocument -> Either Text DecQ
buildReturnTypeDec' = undefined

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

lookUpPrimitiveTypeQ :: GQL.Name -> Maybe TypeQ
lookUpPrimitiveTypeQ = \case
  (GQL.Name "String" ) -> Just $ conT ''Text
  (GQL.Name "Float"  ) -> Just $ conT ''Float
  (GQL.Name "Int"    ) -> Just $ conT ''Int
  (GQL.Name "Boolean") -> Just $ conT ''Bool
  _                    -> Nothing

mkTypeQFromName :: GQL.Name -> TypeQ
mkTypeQFromName = conT . mkName . T.unpack . GQL.unName
