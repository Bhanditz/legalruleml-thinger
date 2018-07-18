{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Main where

import Lib
import Text.XML.HXT.Core
import Options.Applicative as OA
import Options.Applicative.Arrows
import Data.Monoid((<>))
import Control.Applicative
import Data.List
import Data.Text

data AppOptions = AppOptions {
  fileName :: Text,
  act      :: Command
}

data Command = PrintOut | PopulateDatabase String

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc 

appoptions :: OA.Parser AppOptions
appoptions = subparser $
        command "print" (printCmd `withInfo` "Print out the structured information extracted from a LegalRuleML file") <>
        command "db"    (dbCmd    `withInfo` "Populate a database with information extracted from a LegalRuleML file")

fileBit :: OA.Parser Text
fileBit = runA $ proc () -> do
            file <- asA (argument str (metavar "FILE" <> help "File to parse")) -< ()
            returnA -< pack file

printCmd :: OA.Parser AppOptions
printCmd =  runA $ proc () -> do
             fileName <- asA fileBit -< ()
             returnA -< AppOptions { fileName = fileName, act = PrintOut }

dbCmd :: OA.Parser AppOptions
dbCmd = AppOptions
          <$> fileBit
          <*> (PopulateDatabase <$> argument str
              ( metavar "DBNAME"
             <> help "Database to connect to" ))


real_main :: AppOptions -> IO ()
real_main options =
    do
      stmts <- runX (readDocument [withRemoveWS yes] (unpack (fileName options)) >>> getChildren >>> isElem >>> hasName "lrml:LegalRuleML" >>> all_statement_er)
      case (act options) of
        PrintOut            -> print stmts
        PopulateDatabase db -> print "..."

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Parse LegalRuleML documents into a database"
     <> header "legalruleml-thinger-exe - For ingesting LegalRulML documents into a relational database" )

