{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Text.PrettyPrint.HughesPJ (render)
import Language.GLSL
import Options.Applicative

import Nightshade.Analysis
import Nightshade.Format
import Nightshade.Ugly

data Config = Config
  { sourceFile :: String
  , namespace :: String
  , uglifySource :: Bool
  }

parseConfig :: Parser Config
parseConfig = Config
     <$> argument str
         (metavar "SOURCE"
        <> help "The shader source files to process" )
     <*> argument str
         ( metavar "NAMESPACE"
        <> help "The namespace to wrap the output in" )
     <*> switch
         ( long "uglify"
        <> help "Whether the source should be minified" )

main :: IO ()
main = execParser opts >>= process
  where
    opts = info (helper <*> parseConfig)
      ( fullDesc
     <> progDesc "Wrap shaders into TypeScript and optionally obfuscate"
     <> header "nightshade" )
    process cfg = do
      shaderSrc <- readFile (sourceFile cfg)
      case parse shaderSrc of
        Left err -> print err
        Right ast -> do
          let
            srcProcessed
              = if uglifySource cfg then uglify ast else shaderSrc
          let uniforms = findUniforms ast
          putStrLn $ render $ formatShader (namespace cfg) uniforms srcProcessed
