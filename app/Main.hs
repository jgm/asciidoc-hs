{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import Options.Applicative
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import Text.Show.Pretty (ppShow)
import AsciiDoc.Parse (parseDocument)
import qualified Data.ByteString.Lazy as B
import System.IO (hPutStrLn, stderr)

data OutputFormat
  = AST    -- ^ Show the parsed AST in Haskell notation
  | JSON   -- ^ Show the parsed AST in JSON serialization
  deriving (Show, Eq)

-- | Command line options
data Options = Options
  { optInputFiles :: [FilePath]
  , optOutputFormat :: OutputFormat
  } deriving (Show)

-- | Parser for output format
outputFormatParser :: ReadM OutputFormat
outputFormatParser = eitherReader $ \s ->
  case s of
    "ast"  -> Right AST
    "json" -> Right JSON
    _      -> Left $ "Invalid output format: " ++ s ++ ". Use 'ast' or 'json'."

-- | Command line options parser
optionsParser :: Parser Options
optionsParser = Options
  <$> many (argument str
      ( metavar "FILE"
     <> help "Input files (omit to read from stdin)"
      ))
  <*> option outputFormatParser
      ( long "to"
     <> metavar "FORMAT"
     <> value AST
     <> help "Output format (ast*|json)"
      )

-- | Program description
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Parse AsciiDoc and output an AST"
 <> header "hasciidoc - AsciiDoc parser in Haskell"
  )


main :: IO ()
main = do
  options <- execParser opts

  let raiseError pos msg = do
        hPutStrLn stderr $ "Parse error at position " <> show pos <> ": " <> msg
        exitFailure

  -- Parse the document
  doc <- case optInputFiles options of
          [] -> TIO.getContents >>= parseDocument TIO.readFile raiseError "stdin"
          fs -> mconcat <$> mapM (\fp ->
                  TIO.readFile fp >>= parseDocument TIO.readFile raiseError fp)
                fs
  case optOutputFormat options of
    AST -> putStrLn $ ppShow doc
    JSON -> do
      B.putStr $ encode doc
      B.putStr "\n"
