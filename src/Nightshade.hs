{-# LANGUAGE LambdaCase #-}

module Nightshade where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.List
import qualified Data.HashMap.Strict as HMS
import Language.GLSL
import Text.PrettyPrint.HughesPJClass

import Nightshade.Ugly

type Name = ( String, Bool )

findDeclarations :: Data s => s -> [ Name ]
findDeclarations node = defaultExterns ++ decs ++ funcsAndArgs
  where
    defaultExterns = zip [ "main" ] (repeat True)
    decs = node ^.. template . folding initDecs
    initDecs = \case
      InitDeclaration
        (TypeDeclarator (FullType typeQual _)) decls
          -> [ ( name, isExternal typeQual ) | InitDecl name _ _ <- decls ]
      _ -> []
    isExternal t = case t ^? template of
      Nothing -> False
      Just Language.GLSL.Const -> False
      _ -> True
    parameterDecls = \case
      ParameterDeclaration _ _ _ (Just ( name, _ )) -> Just ( name, False )
      _ -> Nothing
    funcsAndArgs = node ^.. template . folding (\(FuncProt _ name params) ->
      ( name, False ) : (params ^.. folded . folding parameterDecls))

shortNames :: [ String ]
shortNames = map (:[]) alphabet ++ (concat $ transpose $ map (\l -> map (l:) shortNames) alphabet)
  where
    alphabet = ['a'..'z'] ++ ['A'..'Z']

type NameMapping = HMS.HashMap String String

nameMapping :: [ Name ] -> NameMapping
nameMapping names = HMS.fromList $ zip internal shortNames
  where
    internal = [ n | ( n, e ) <- HMS.toList $ HMS.fromListWith (||) names, not e ]

applyMapping :: Data s => NameMapping -> s -> s
applyMapping nm = updateFuncs . updateExprs . updateDecls
  where
    mapName n = HMS.lookupDefault n n nm
    updateDecls = over template (\(InitDecl n ce ae) -> InitDecl (mapName n) ce ae)
    updateExprs = transformOnOf template uniplate $ \case
      Variable n -> Variable (HMS.lookupDefault n n nm)
      FunctionCall (FuncId n) ps -> FunctionCall (FuncId (mapName n)) ps
      e -> e
    updateParam = \case
      ParameterDeclaration a b c (Just ( n, d ))
        -> ParameterDeclaration a b c (Just ( mapName n, d ))
      pd -> pd
    updateFuncs = over template $ \(FuncProt t fn params) ->
      FuncProt t (mapName fn) (map updateParam params)

test = do
  fragS <- readFile "shader.frag"
  case parse fragS of
    Left err -> print err
    Right frag -> do
      let decls = findDeclarations frag
      let nm = nameMapping decls
      putStrLn "Mapping:"
      putStrLn $ unlines $ map (\( orig, new ) -> orig ++ ": " ++ new) (HMS.toList nm)
      putStrLn $ renderStyle style { mode = OneLineMode } $ uglify $ applyMapping nm frag
