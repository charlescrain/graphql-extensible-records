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
import           Language.Haskell.TH
import           Language.GraphQL.Draft.Parser

-- import           Data.Morpheus.Parsing.Document.TypeSystem
-- import           Data.Morpheus.Types.Internal.Resolving.Core
-- import           Data.Morpheus.Document
-- import           Data.Morpheus.Types.IO  hiding ( operationName )
-- import           Data.Morpheus.Parsing.Request.Parser

import           Data.Char                      ( toUpper )
import           Data.Maybe                     ( catMaybes
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
import           Language.GraphQL.Draft.Syntax  (unVariable,  SchemaDocument(..)
                                                , ExecutableDefinition(..)
                                                , TypedOperationDefinition(..)
                                                , OperationDefinition(..)
                                                , ExecutableDocument(..)
                                                , getExecutableDefinitions
                                                , Name(..)
                                                , VariableDefinition(..)
                                                , GType(..)
                                                , Nullability(..)
                                                )

operationTypeToSchemaTypeName :: OperationType -> Text
operationTypeToSchemaTypeName = \case
  Query        -> "Query"
  Subscription -> "Subscription"
  Mutation     -> "Mutation"

buildTypes :: String -> [String] -> Q [Dec]
buildTypes schemaFilePath queryFilePaths = do
  schemaText <- runIO $ T.readFile schemaFilePath
  queryTexts <- runIO $ mapM T.readFile queryFilePaths
  let execDocs = flip map queryTexts $ \queryText ->
        case parseExecutableDoc queryText of
          Left  errs -> error $ show errs
          Right res  -> res
      schemaDoc = case parseSchemaDoc schemaText of
        Left  errs -> error $ show errs
        Right res  -> res

  standaloneTypeDecs <- case mapM (buildEnumDecs schemaDoc) execDocs of
    Left  err  -> error $ T.unpack err
    Right decs -> concat . nub <$> sequence decs


  queryTypes <- sequence $ concat $ rights $ sequence $ mapM
    (buildTypesForQuery schemaDoc)
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

buildEnumDecs :: SchemaDocument -> ExecutableDocument -> Either Text (Q [Dec])
buildEnumDecs = undefined

buildArgTypeDecs'
  :: SchemaDocument -> TypedOperationDefinition -> Either Text DecQ
buildArgTypeDecs' sd tod = case _todName tod of
  Nothing -> Left "buildArgTypeDecs: Unnamed Queries not permitted"
  Just (Name n) ->
    let queryName = mkName $ mkUpperWord $ T.unpack (n <> "Args")
        varDefs = _todVariableDefinitions tod
        records = mkRecord $ buildArgRecords' sd varDefs
    in  tySynD queryName [] records



buildArgRecords' :: SchemaDocument -> [VariableDefinition] -> Either Text [TypeQ]
buildArgRecords' sd varDefs = sequence . map buildArgType
 where
  buildArgType vdef =
    let name       = unName $ unVariable $ _vdVariable vdef
        isOptional = case _vdDefaultValue vdef of
          Nothing -> False
          Just _  -> True
    in  let wrapMaybe = if isOptional then appT (conT ''Maybe) else id
            wrapNull = case _vdType vdef of
                TypeNamed (Nullability True) _ -> appT (conT ''Nullable) 
                TypeList (Nullability True) _ -> appT (conT ''Nullable) 
                _ -> id
            wrapList = if isList' variableType then appT listT else id
            type' =
                fromMaybe (error $ "Type not found in schema: " <> T.unpack name)
                  $ lookUpTypeInSchema schema name
            recordName = litT (strTyLit $ T.unpack argName)
            recordType = wrapMaybe $ wrapNull $ wrapList type'
        in  uInfixT recordName (mkName ">:") recordType































buildReturnTypeDec' :: SchemaDocument -> ExecutableDocument -> Either Text DecQ
buildReturnTypeDec' = undefined

promotedTypeList :: [Q Type] -> Q Type
promotedTypeList []       = promotedNilT
promotedTypeList (t : ts) = [t| $promotedConsT $t $(promotedTypeList ts) |]

type SchemaTypeDefs = [TypeDefinition]

data Nullable a = Null | NonNull a

lookUpPrimitiveType :: Text -> Maybe TypeQ
lookUpPrimitiveType = \case
  "String"  -> Just $ conT ''Text
  "Float"   -> Just $ conT ''Float
  "Int"     -> Just $ conT ''Int
  "Boolean" -> Just $ conT ''Bool
  _         -> Nothing


buildTypesDecs :: SchemaTypeDefs -> GQLQuery -> Either Text (Q [Dec])
buildTypesDecs sc gq = do
  let
    optype = operationType . operation $ gq
    argTypes =
      map (typeConName . variableType . snd) (operationArguments $ operation gq)
    argTypesNoPrims = flip filter argTypes $ \t ->
      case lookUpPrimitiveType t of
        Nothing -> True
        _       -> False
  selectionTypes <- map typeConName
    <$> getSelectionTypes sc optype (operationSelection $ operation gq)
  let
    allTypeNames         = nub (argTypesNoPrims <> selectionTypes)
    allTypeDefs          = mapMaybe (lookUpTypeDefInSchema sc) allTypeNames
    allTypeDefsNoScalars = flip filter allTypeDefs $ \td ->
      case typeContent td of
        DataScalar _ -> False
        _            -> True
  pure (toTHDefinitions False allTypeDefsNoScalars >>= declareTypes False)

getSelectionTypes
  :: SchemaTypeDefs
  -> OperationType
  -> SelectionSet (stage :: Stage)
  -> Either Text [TypeRef]
getSelectionTypes sc optype set =
  let optypeText      = operationTypeToSchemaTypeName optype
      mqueryFieldDefs = getFieldDefinitionsFromType sc optypeText
  in  case mqueryFieldDefs of
        Nothing ->
          error $ "no " <> T.unpack optypeText <> " TypeDefinition found"
        Just queryFieldDefs ->
          getSelectionTypesFromSelectionSet queryFieldDefs set
 where
  getSelectionTypesFromSelectionSet
    :: [FieldDefinition]
    -> SelectionSet (stage :: Stage)
    -> Either Text [TypeRef]
  getSelectionTypesFromSelectionSet fieldDefs selset =
    let eSelTypes = map (getSelectionTypesFromSelection fieldDefs) selset
        rs        = concat $ rights eSelTypes
        ls        = lefts eSelTypes
    in  case ls of
          [] -> Right rs
          _  -> Left $ T.intercalate "\n" ls
  getSelectionTypesFromSelection
    :: [FieldDefinition]
    -> (Text, Selection (stage :: Stage))
    -> Either Text [TypeRef]
  getSelectionTypesFromSelection fieldDefs (name, sel) =
    case selectionContent sel of
      SelectionField -> maybe (Left ("no TypeRef found for type: " <> name))
                              (Right . (: []))
                              (getSelectionFieldTypeRef fieldDefs name)
      UnionSelection _ -> Left "UnionSelection not supported"
      SelectionSet selSet ->
        case find (\fd -> fieldName fd == name) fieldDefs of
          Nothing -> Left ("no field def found " <> name)
          Just fd ->
            let fieldTypeName = typeConName $ fieldType fd
            in  case getFieldDefinitionsFromType sc fieldTypeName of
                  Nothing ->
                    Left $ "no FieldDefs found for type" <> fieldTypeName
                  Just nextFieldDefs ->
                    getSelectionTypesFromSelectionSet nextFieldDefs selSet

getSelectionFieldTypeRef :: [FieldDefinition] -> Text -> Maybe TypeRef
getSelectionFieldTypeRef fds name =
  fieldType <$> find (\fd -> fieldName fd == name) fds

getFieldDefinitionsFromType :: SchemaTypeDefs -> Text -> Maybe [FieldDefinition]
getFieldDefinitionsFromType sc name =
  unFieldsDefinition
    .   objectFields
    .   typeContent
    <$> find (\td -> typeName td == name) sc

lookUpTypeDefInSchema :: SchemaTypeDefs -> Text -> Maybe TypeDefinition
lookUpTypeDefInSchema sc t = flip find sc $ \td -> typeName td == t

lookUpTypeInSchema :: SchemaTypeDefs -> Text -> Maybe TypeQ
lookUpTypeInSchema sc t = case lookUpPrimitiveType t of
  Just a  -> Just a
  Nothing -> buildTypeFromSchema
 where
  buildTypeFromSchema = do
    td <- lookUpTypeDefInSchema sc t
    pure $ conT (mkName $ T.unpack $ typeName td)

isNullable' :: TypeRef -> Bool
isNullable' td = or $ flip map (typeWrappers td) $ \case
  TypeMaybe -> True
  _         -> False

isList' :: TypeRef -> Bool
isList' td = or $ flip map (typeWrappers td) $ \case
  TypeList -> True
  _        -> False

buildArgRecords :: SchemaTypeDefs -> GQLQuery -> [TypeQ]
buildArgRecords schema GQLQuery {..} =
  let Operation { operationArguments = args } = operation
  in  map buildArgType args
 where
  buildArgType (argName, Variable {..}) =
    let name       = typeConName variableType
        isOptional = case variableValue of
          DefaultValue Nothing -> False
          DefaultValue _       -> True
    in  let wrapMaybe = if isOptional then appT (conT ''Maybe) else id
            wrapNull =
                if isNullable' variableType then appT (conT ''Nullable) else id
            wrapList = if isList' variableType then appT listT else id
            type' =
                fromMaybe (error $ "Type not found in schema: " <> T.unpack name)
                  $ lookUpTypeInSchema schema name
            recordName = litT (strTyLit $ T.unpack argName)
            recordType = wrapMaybe $ wrapNull $ wrapList type'
        in  uInfixT recordName (mkName ">:") recordType


buildArgTypeDecs :: SchemaTypeDefs -> GQLQuery -> DecQ
buildArgTypeDecs stds gqlq =
  let queryName = case operationName $ operation gqlq of
        Nothing   -> error "Unnamed Queries not permitted"
        Just name -> mkName $ mkUpperWord $ T.unpack (name <> "Args")
      records = mkRecord $ buildArgRecords stds gqlq
  in  tySynD queryName [] records

buildReturnTypeDec :: SchemaTypeDefs -> GQLQuery -> DecQ
buildReturnTypeDec stds gqlq =
  let selections = operationSelection $ operation gqlq
      optype     = operationType . operation $ gqlq
      queryName  = case operationName $ operation gqlq of
        Nothing   -> error "Unnamed Queries not permitted"
        Just name -> mkName $ mkUpperWord $ T.unpack (name <> "Response")
      responseRecords = map (buildReturnType optype stds) selections
      records         = mkRecord responseRecords
  in  tySynD queryName [] records

buildReturnType
  :: OperationType
  -> SchemaTypeDefs
  -> (Text, Selection (stage :: Stage))
  -> TypeQ
buildReturnType optype stds sel =
  let optypeText = operationTypeToSchemaTypeName optype
  in  case getFieldDefinitionsFromType stds optypeText of
        Nothing ->
          error $ "No " <> T.unpack optypeText <> " Field found in schema"
        Just fieldDefs -> recLookUp fieldDefs sel
 where
  recLookUp fieldDefs (name, sel') =
    let name'   = fromMaybe name (selectionAlias sel')
        typeRef = fromMaybe
          (error ("Failed to find " <> T.unpack name <> " in field defs"))
          (getSelectionFieldTypeRef fieldDefs name)
    in  case selectionContent sel' of
          UnionSelection _ -> error "UnionSelection unsupported"
          SelectionField ->
            let type' = fromMaybe
                  (conT (mkName $ T.unpack $ typeConName typeRef))
                  (lookUpPrimitiveType (typeConName typeRef))
            in  addRecord name' type'
          SelectionSet nextSels ->
            case getFieldDefinitionsFromType stds (typeConName typeRef) of
              Nothing ->
                error ("Failed to find type " <> T.unpack (typeConName typeRef))
              Just nextFieldDefs -> addRecord
                name'
                (mkRecord (map (recLookUp nextFieldDefs) nextSels))

addRecord :: Text -> TypeQ -> TypeQ
addRecord name recordType =
  let recordName = litT (strTyLit $ T.unpack name)
  in  uInfixT recordName (mkName ">:") recordType

mkRecord :: [TypeQ] -> TypeQ
mkRecord = appT (conT ''Record) . promotedTypeList


mkUpperWord :: String -> String
mkUpperWord []       = []
mkUpperWord (x : xs) = toUpper x : xs
