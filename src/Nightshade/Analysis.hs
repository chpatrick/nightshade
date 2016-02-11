{-# LANGUAGE LambdaCase #-}

module Nightshade.Analysis where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.Foldable
import Data.List
import Language.GLSL

import Nightshade.Types

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

findUniforms :: Data s => s -> [ Uniform ]
findUniforms = toListOf (template . folding uniforms)
  where
    uniforms = \case
      InitDeclaration
        (TypeDeclarator
          (FullType
            typeQual
            (TypeSpec _
              (TypeSpecNoPrecision
                typeSpec
                Nothing)))) decls | isUniform typeQual
          -> [ ( name, typeSpec ) | InitDecl name _ _ <- decls ]
      _ -> []
    isUniform t = case t ^? template of
      Just Uniform -> True
      _ -> False
