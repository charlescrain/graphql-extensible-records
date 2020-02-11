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

import           Language.Haskell.TH
import           Data.Morpheus.Types.Internal.AST
import           Data.Morpheus.Parsing.Document.TypeSystem
import           Data.Morpheus.Types.Internal.Resolving.Core
import           Data.Morpheus.Document
import           Data.Morpheus.Types.IO  hiding ( operationName )
import           Data.Char                      ( toUpper )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Morpheus.Parsing.Request.Parser
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Extensible                ( Record )
import           Data.List                      ( nub
                                                , find
                                                )

buildTypes :: String -> [String] -> Q [Dec]
buildTypes schemaFilePath queryFilePaths = do
  schemaText <- runIO $ T.readFile schemaFilePath
  queryTexts  <- runIO $ mapM T.readFile queryFilePaths
  concat <$> mapM (buildTypesForQuery schemaText) queryTexts
 where 
  buildTypesForQuery schemaText queryText = do
    let gqlQuery = case parseGQL (GQLRequest queryText Nothing Nothing) of
          Failure errs    -> error $ show errs
          Success res _ _ -> res
        schemaTypeDefs = case parseSchema schemaText of
          Failure errs    -> error $ show errs
          Success res _ _ -> res
    argTypeDecs        <- sequence [buildArgTypeDecs schemaTypeDefs gqlQuery]
    standaloneTypeDecs <- buildTypesDecs schemaTypeDefs gqlQuery
    returnTypeDecs     <- buildReturnTypeDec schemaTypeDefs gqlQuery
    pure (argTypeDecs <> standaloneTypeDecs <> [returnTypeDecs])

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

buildTypesDecs :: SchemaTypeDefs -> GQLQuery -> Q [Dec]
buildTypesDecs sc gq =
  let
    argTypes =
      map (typeConName . variableType . snd) (operationArguments $ operation gq)
    argTypesNoPrims = flip filter argTypes $ \t ->
      case lookUpPrimitiveType t of
        Nothing -> True
        _       -> False
    selectionTypes =
      map typeConName $ getSelectionTypes sc (operationSelection $ operation gq)
    allTypeNames         = nub (argTypesNoPrims <> selectionTypes)
    allTypeDefs          = mapMaybe (lookUpTypeDefInSchema sc) allTypeNames
    allTypeDefsNoScalars = flip filter allTypeDefs $ \td ->
      case typeContent td of
        DataScalar _ -> False
        _            -> True
  in
    toTHDefinitions False allTypeDefsNoScalars >>= declareTypes False

getSelectionTypes
  :: SchemaTypeDefs -> SelectionSet (stage :: Stage) -> [TypeRef]
getSelectionTypes sc set =
  let mqueryFieldDefs = getFieldDefinitionsFromType sc "Query"
  in  case mqueryFieldDefs of
        Nothing -> error "no query TypeDefinition found"
        Just queryFieldDefs ->
          getSelectionTypesFromSelectionSet queryFieldDefs set
 where
  getSelectionTypesFromSelectionSet
    :: [FieldDefinition] -> SelectionSet (stage :: Stage) -> [TypeRef]
  getSelectionTypesFromSelectionSet fieldDefs =
    concatMap (getSelectionTypesFromSelection fieldDefs)
  getSelectionTypesFromSelection
    :: [FieldDefinition] -> (Text, Selection (stage :: Stage)) -> [TypeRef]
  getSelectionTypesFromSelection fieldDefs (name, sel) =
    case selectionContent sel of
      SelectionField -> maybe
        (error ("no TypeRef found for type: " <> T.unpack name))
        (: [])
        (getSelectionFieldTypeRef fieldDefs name)
      UnionSelection _ -> error "UnionSelection not supported"
      SelectionSet selSet ->
        case find (\fd -> fieldName fd == name) fieldDefs of
          Nothing -> error ("no field def found " <> T.unpack name)
          Just fd ->
            let
              fieldTypeName = typeConName $ fieldType fd
              nextFieldDefs = fromMaybe
                (error ("no FieldDefs found for type" <> T.unpack fieldTypeName)
                )
                (getFieldDefinitionsFromType sc fieldTypeName)
            in
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
      queryName  = case operationName $ operation gqlq of
        Nothing   -> error "Unnamed Queries not permitted"
        Just name -> mkName $ mkUpperWord $ T.unpack (name <> "Response")
      responseRecords = map (buildReturnType stds) selections
      records         = mkRecord responseRecords
  in  tySynD queryName [] records

buildReturnType :: SchemaTypeDefs -> (Text, Selection (stage :: Stage)) -> TypeQ
buildReturnType stds sel = case getFieldDefinitionsFromType stds "Query" of
  Nothing        -> error "No Query Field found in schema"
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
