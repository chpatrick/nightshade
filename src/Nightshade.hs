{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Nightshade (process) where

import Text.PrettyPrint.HughesPJ (render)
import Language.GLSL

import Nightshade.Analysis
import Nightshade.Format
import Nightshade.Ugly

process ::
     String -- ^ Namespace to wrap the output in
  -> Bool -- ^ Whether to uglify
  -> String -- ^ Source
  -> Either String String
process namespace uglifySrc src = do
  ast <- either (Left . show) Right (parse src)
  let processed = if uglifySrc then uglify ast else src
  let uniforms = findUniforms ast
  return (render ((formatShader namespace) uniforms processed))
