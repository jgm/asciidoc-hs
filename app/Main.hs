{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import Text.Show.Pretty (ppShow)
import System.Environment (getArgs)
import AsciiDoc.Parse (parseDocument)
import Control.Monad.Except
import Control.Monad.Trans (lift)

main :: IO ()
main = do
  args <- getArgs

  -- Read input
  input <- case args of
    [] -> TIO.getContents
    fs -> mconcat <$> mapM TIO.readFile fs

  -- Parse the document
  res <- runExceptT $ parseDocument (lift . TIO.readFile) input
  case res of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right doc -> putStrLn $ ppShow doc
