{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}
import Data.Monoid ((<>))
import Options.Applicative

import Nightshade

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
main = do
  Config{..} <- execParser opts
  shaderSrc <- readFile sourceFile
  case process namespace uglifySource shaderSrc of
    Left err -> fail err
    Right processed -> putStrLn processed
  where
    opts = info (helper <*> parseConfig)
      ( fullDesc
     <> progDesc "Wrap shaders into TypeScript and optionally obfuscate"
     <> header "nightshade" )
