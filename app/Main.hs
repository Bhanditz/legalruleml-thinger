{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Main where

import Lib
import Text.XML.HXT.Core
import Options.Applicative as OA
import Data.Monoid((<>))
import Control.Applicative
import Data.List
import Data.Text

data AppOptions = AppOptions {
  fileName :: String,
  outputFile :: String
}

appoptions :: OA.Parser AppOptions
appoptions = AppOptions
        <$> argument str
              ( metavar "FILE"
             <> help "File to parse" )
        <*> argument str
              ( metavar "FILE"
             <> help "Output file name")

real_main :: AppOptions -> IO ()
real_main options =
    do
      stmts <- runX (readDocument [] (fileName options) >>> getChildren >>> isElem >>> hasName "lrml:LegalRuleML" >>> all_statement_er)
      case stmts of
        []  -> putStrLn "No statements found."
        w   -> print w

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Parse LegalRuleML documents into a database"
     <> header "legalruleml-thinger-exe - For ingesting LegalRulML documents into a relational database" )

