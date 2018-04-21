{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}
import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Nightshade

parseConfig :: Parser Bool
parseConfig =
  switch
    ( long "uglify" <> help "Whether the source should be minified" )

main :: IO ()
main = do
  uglifySource <- execParser opts
  shaderSrc <- T.getContents
  case process uglifySource (T.unpack shaderSrc) of
    Left err -> fail err
    Right processed -> putStrLn processed
  where
    opts = info (helper <*> parseConfig)
      ( fullDesc
     <> progDesc "Wrap shaders into TypeScript and optionally obfuscate"
     <> header "nightshade" )
