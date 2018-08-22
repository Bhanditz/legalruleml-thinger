{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase, ScopedTypeVariables #-}

module Main where

import Lib
import Text.XML.HXT.Core
import Options.Applicative as OA
import Options.Applicative.Arrows
import Data.Monoid((<>))
import Control.Applicative
import Data.List
import Data.Text
import qualified Database.PostgreSQL.Simple as PGS

data Command = PrintOut Text | PopulateDatabase Text (Text, Text, Text, Int) | InitDatabase (Text, Text, Text, Int)

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

dbBit :: OA.Parser (Text, Text, Text, Int)
dbBit = runA $ proc () -> do
            db <- asA (argument str (metavar "DBNAME" <> help "Database to connect to")) -< ()
            user <- asA (argument str (metavar "USER" <> help "User name (db role)")) -< ()
            password <- asA (argument str (metavar "PASSWORD" <> help "Password")) -< ()
            port <- asA (argument auto (metavar "PORT" <> help "Port to connect on")) -< ()
            returnA -< (pack db, pack user, pack password, port)

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

populateDb :: [Statement_ed] -> Text -> Text -> Text -> Int -> IO ()
populateDb stuff dbName user password port = do
    conn <- dbConnection dbName user password port
    insertStatements stuff conn

initDb :: Text -> Text -> Text -> Int -> IO ()
initDb dbName user password port = do
    conn <- dbConnection dbName user password port
    PGS.execute_ conn "CREATE TABLE \"Statement\" (id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, category TEXT NOT NULL, strength TEXT, key TEXT, formula integer[]);"
    PGS.execute_ conn "CREATE TABLE \"Formula\" (id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, name TEXT NOT NULL, text TEXT, children integer[]);"
    PGS.execute_ conn "CREATE TABLE \"Metadata\" (id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, text TEXT NOT NULL);"
    return ()


real_main :: Command -> IO ()
real_main options =
    do
      case options of
        InitDatabase (db, user, password, port) -> do
            initDb db user password port
        PrintOut         file    -> do
            stuff <- getData file
            print stuff
        PopulateDatabase file (db, user, password, port) -> do
            stuff <- getData file
            populateDb stuff db user password port

main :: IO ()
main = execParser opts >>= real_main
  where
    opts = info (helper <*> appoptions)
      ( fullDesc
     <> progDesc "Parse LegalRuleML documents into a database"
     <> header "legalruleml-thinger-exe - For ingesting LegalRulML documents into a relational database" )

