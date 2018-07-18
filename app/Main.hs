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

data Command = PrintOut Text | PopulateDatabase Text Text | InitDatabase Text

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc 

appoptions :: OA.Parser Command
appoptions = subparser $
        command "print" (printCmd `withInfo` "Print out the structured information extracted from a LegalRuleML file") <>
        command "db"    (dbCmd    `withInfo` "Populate a database with information extracted from a LegalRuleML file") <>
        command "init"  (initCmd  `withInfo` "Initialise a database with the tables needed to store LegalRuleML data.")

fileBit :: OA.Parser Text
fileBit = runA $ proc () -> do
            file <- asA (argument str (metavar "FILE" <> help "File to parse")) -< ()
            returnA -< pack file

dbBit :: OA.Parser Text
dbBit = runA $ proc () -> do
            db <- asA (argument str (metavar "DBNAME" <> help "Database to connect to")) -< ()
            returnA -< pack db

printCmd :: OA.Parser Command
printCmd =  runA $ proc () -> do
             fileName <- asA fileBit -< ()
             returnA -< PrintOut fileName

dbCmd :: OA.Parser Command
dbCmd = PopulateDatabase
          <$> fileBit
          <*> dbBit

initCmd :: OA.Parser Command
initCmd = InitDatabase
           <$> dbBit


getData :: Text -> IO [Statement_ed]
getData fileName = do
    runX (readDocument [withRemoveWS yes] (unpack fileName) >>> getChildren >>> isElem >>> hasName "lrml:LegalRuleML" >>> all_statement_er)

populateDb :: [Statement_ed] -> Text -> IO ()
populateDb stuff dbName = do
    print stuff

real_main :: Command -> IO ()
real_main options =
    do
      case options of
        InitDatabase     db      -> print db
        PrintOut         file    -> do
            stuff <- getData file
            print stuff
        PopulateDatabase file db -> do
            stuff <- getData file
            populateDb stuff db

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Parse LegalRuleML documents into a database"
     <> header "legalruleml-thinger-exe - For ingesting LegalRulML documents into a relational database" )

