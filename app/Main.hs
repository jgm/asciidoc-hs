{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import Options.Applicative
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import Text.Show.Pretty (ppShow)
import AsciiDoc.Parse (parseDocument)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy as B

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

  -- Read input
  input <- case optInputFiles options of
    [] -> TIO.getContents
    fs -> mconcat <$> mapM TIO.readFile fs

  -- Parse the document
  res <- runExceptT $ parseDocument (lift . TIO.readFile) input
  case res of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right doc ->
      case optOutputFormat options of
        AST -> putStrLn $ ppShow doc
        JSON -> do
          B.putStr $ encode doc
          B.putStr "\n"
