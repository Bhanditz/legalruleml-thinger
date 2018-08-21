{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( all_statement_er, Statement_ed, formulas
    ) where

import Prelude hiding (sum)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList (listA)
import Data.Text (Text, pack, unpack)
import Data.Maybe
import Control.Monad (mplus)

-- Imports for db handling

import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table, table, tableColumn, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===), Insert(..), Update(..), Delete(..), rCount, rReturning, updateEasy,
                         (.++), ifThenElse, sqlString, sqlArray, sqlInt4, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery, runInsert_,
                         showSqlForPostgres, Unpackspec,
                         SqlInt4, SqlInt8, SqlText, SqlDate, SqlFloat8, SqlBool, SqlArray)

import           Data.Profunctor.Product (p2, p3, p4, p5)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)

import           Control.Arrow (returnA)

import qualified Database.PostgreSQL.Simple as PGS


data StatementCategory = PrescriptiveStatement | ConstitutiveStatement deriving (Show, Eq)

data Statement_ed = Statement_ed { statement_category :: StatementCategory, strength, key :: Maybe Text, formula :: Maybe [Logic]} deriving (Show, Eq)

data LogicData = LogicText Text | LogicCollection [Logic] | LogicEmpty deriving (Show, Eq)
data Logic = Logic { name :: Text, child :: LogicData } deriving (Show, Eq)

all_statement_er = multi (isElem >>> ((hasName "lrml:PrescriptiveStatement") <+> (hasName "lrml:ConstitutiveStatement"))) >>>
  statement_er

statement_er = proc stmt -> do
                      sc <- getName >>> arr pack >>> arr statement_type -< stmt
                      key <- single (withDefault (getAttrl >>> hasName "key" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) Nothing) -< stmt
                      strength <- single (withDefault (getChildren >>> hasName "ruleml:Rule" >>> getAttrl >>> hasName "strength" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) Nothing) -< stmt
                      formula <- single (withDefault (getChildren >>> hasName "ruleml:Rule" >>> formulas >>> arr (\x -> Just x)) Nothing) -< stmt
                      returnA -< Statement_ed {
                        statement_category = sc,
                        strength = strength,
                        key = key,
                        formula = formula }

formulas = proc parent -> do
                  logics <- listA (getChildren >>> (getName &&& (isElem >>> (proc elem -> do
                          text       <- withDefault (getChildren >>> (getText >>> arr (\x -> Just (LogicText (pack x))))) Nothing -< elem
                          subformula <- withDefault (formulas >>> arr (\x -> if x == [] then Just LogicEmpty else Just (LogicCollection x))) Nothing -< elem
                          returnA -< (fromMaybe LogicEmpty $ mplus text subformula)))) >>>
                      arr (\x -> Logic { name = pack (fst x), child = snd x })) -< parent
                  returnA -< logics


statement_type :: Text -> StatementCategory
statement_type = \case
                           "lrml:PrescriptiveStatement"  -> PrescriptiveStatement
                           "lrml:ConstitutiveStatement"  -> ConstitutiveStatement
                           a                             -> error ("Not a statement:" ++ (unpack a))


-- db housekeeping

dbConnection :: Text -> IO PGS.Connection
dbConnection db = PGS.connect PGS.ConnectInfo {
                     PGS.connectHost="localhost"
                   , PGS.connectPort=5432
                   , PGS.connectDatabase=(unpack db)
                   , PGS.connectPassword="Password"
                   , PGS.connectUser="User"
                   }


-- Define tables
-- We have one for statements, one for formulas, and one for metadata
-- Statements :: Id, Category, Strength, key, formula
-- Formulas :: Id, name, text, children
-- Metadata :: Id, text


statementTable :: Table
      (Maybe (Column SqlInt4), Column SqlText, Maybe (Column SqlText), Maybe (Column SqlText), Maybe (Column (SqlArray SqlInt4)))
      ((Column SqlInt4), Column SqlText, (Column SqlText), (Column SqlText), (Column (SqlArray SqlInt4)))

statementTable = table "Statement" (p5 (tableColumn "id", tableColumn "category", tableColumn "strength", tableColumn "key", tableColumn "formula"))

formulaTable :: Table
      (Maybe (Column SqlInt4), Column SqlText, Maybe (Column SqlText), Maybe (Column (SqlArray SqlInt4)))
      ((Column SqlInt4), Column SqlText, (Column SqlText), (Column (SqlArray SqlInt4)))

formulaTable = table "Formula" (p4 (tableColumn "id", tableColumn "name", tableColumn "text", tableColumn "children"))

metadataTable :: Table
      (Maybe (Column SqlInt4), Column SqlText)
      ((Column SqlInt4), Column SqlText)

metadataTable = table "Metadata" (p2 (tableColumn "id", tableColumn "text"))


--insertStatements :: [Statement_ed] -> IO ()
--insertStatements statements = 

--   child_keys <- mapM logicBuilder (formula s)

returns :: (Column SqlInt4, Column SqlText, Column SqlText, Column SqlText, Column (SqlArray SqlInt4)) -> Column SqlInt4
returns (id_, _, _, _, _) = id_ 

statementBuilder :: Statement_ed -> [Int] -> (Insert [Int])
statementBuilder s c =
   Insert
     { iTable        = statementTable
     , iRows         = [(Nothing, sqlString (show (statement_category s)), fmap (\x -> sqlString (unpack x)) (strength s), fmap (\x -> sqlString (unpack x)) (key s), Just (sqlArray (\n -> sqlInt4 n) c))]
     , iReturning    = rReturning returns
     , iOnConflict   = Nothing
     }

-- Before we can build a statement, we need the formula ids
-- So, from a [Statement_ed] we map formulaBuilder (as an insertReturning), then mapM runQueryReturning, getting back [(Statement_ed, [Int])] - we then map (\(s, c) -> statementBuilder s c) over it, and then mapM_ runQuery

insertStatements :: [Statement_ed] -> PGS.Connection -> IO ()
insertStatements st conn = do
    let formulaQuery = map (\x -> (x, formulaBuilding x)) st
    formulasWithIds <- mapM (\(x, y) -> do
      z <- runInsert_ conn y
      return (x, z)) formulaQuery
    undefined

formulaBuilding = undefined
