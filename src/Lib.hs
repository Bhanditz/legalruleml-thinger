{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( all_statement_er, Statement_ed, all_term_er, ATerm, all_stuff, formulas, insertStatements, insertTerms, dbConnection
    ) where

import Prelude hiding (sum)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList (listA)
import Data.Text (Text, pack, unpack)
import Data.Maybe
import Data.Foldable (concat)
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
data Logic = Logic { name :: Text, child :: LogicData, formula_iri :: Maybe Text} deriving (Show, Eq)

data ATerm = ATerm { term_iri :: Maybe Text, term_atom :: Text, term_description :: Maybe Text } deriving (Show, Eq)

all_stuff = proc in_stuff -> do
                   statements <- listA all_statement_er -< in_stuff
                   terms      <- listA all_term_er      -< in_stuff
                   returnA -< (statements, terms)

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
                  logics <- listA (getChildren >>> (getName &&& (withDefault (getAttrl >>> hasName "iri" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) Nothing) &&& (isElem >>> (proc elem -> do
                          text       <- withDefault (getChildren >>> (getText >>> arr (\x -> Just (LogicText (pack x))))) Nothing -< elem
                          subformula <- withDefault (formulas >>> arr (\x -> if x == [] then Just LogicEmpty else Just (LogicCollection x))) Nothing -< elem
                          returnA -< (fromMaybe LogicEmpty $ mplus text subformula)))) >>>
                      arr (\(name, (iri, child)) -> Logic { name = pack name, child = child, formula_iri = iri })) -< parent
                  returnA -< logics


statement_type :: Text -> StatementCategory
statement_type = \case
                           "lrml:PrescriptiveStatement"  -> PrescriptiveStatement
                           "lrml:ConstitutiveStatement"  -> ConstitutiveStatement
                           a                             -> error ("Not a statement:" ++ (unpack a))


all_term_er = multi (isElem >>> hasName "vocabulary") >>> getChildren >>> isElem >>> hasName "term" >>> term_er

term_er = proc term -> do
                 iri <- single (withDefault (getAttrl >>> hasName "iri" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) Nothing) -< term
                 atom <- single (withDefault (getChildren >>> isElem >>> hasName "atom" >>> getChildren >>> isText >>> getText) "") -< term
                 description <- single (withDefault (getChildren >>> isElem >>> hasName "description" >>> getChildren >>> isText >>> getText >>> arr (\x -> Just (pack x))) Nothing) -< term
                 returnA -< ATerm { term_iri=iri, term_atom=pack atom, term_description=description }

-- db housekeeping

dbConnection :: Text -> Text -> Text -> Int -> IO PGS.Connection
dbConnection db user pwd port= PGS.connect PGS.ConnectInfo {
                     PGS.connectHost="localhost"
                   , PGS.connectPort=fromInteger (toInteger port)
                   , PGS.connectDatabase=(unpack db)
                   , PGS.connectPassword=(unpack pwd)
                   , PGS.connectUser=(unpack user)
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
      (Maybe (Column SqlInt4), Column SqlText, Maybe (Column SqlText), Maybe (Column (SqlArray SqlInt4)), Maybe (Column SqlText))
      ((Column SqlInt4), Column SqlText, (Column SqlText), (Column (SqlArray SqlInt4)), Column SqlText)

formulaTable = table "Formula" (p5 (tableColumn "id", tableColumn "name", tableColumn "text", tableColumn "children", tableColumn "iri"))

metadataTable :: Table
      (Maybe (Column SqlInt4), Column SqlText)
      ((Column SqlInt4), Column SqlText)

metadataTable = table "Metadata" (p2 (tableColumn "id", tableColumn "text"))

termTable :: Table
      (Maybe (Column SqlInt4), Maybe (Column SqlText), Column SqlText, Maybe (Column SqlText))
      ((Column SqlInt4), Column SqlText, Column SqlText, Column SqlText)

termTable = table "Term" (p4 (tableColumn "id", tableColumn "iri", tableColumn "atom", tableColumn "description"))


--insertStatements :: [Statement_ed] -> IO ()
--insertStatements statements = 

--   child_keys <- mapM logicBuilder (formula s)

returns :: (Column SqlInt4, Column SqlText, Column SqlText, Column SqlText, Column (SqlArray SqlInt4)) -> Column SqlInt4
returns (id_, _, _, _, _) = id_ 

returnsF :: (Column SqlInt4, Column SqlText, Column SqlText, Column (SqlArray SqlInt4), Column SqlText) -> Column SqlInt4
returnsF (id_, _, _, _, _) = id_ 

returnsTerm :: (Column SqlInt4, Column SqlText, Column SqlText, Column SqlText) -> Column SqlInt4
returnsTerm (id_, _, _, _) = id_ 

termsBuilder :: [ATerm] -> [(Insert [Int])]
termsBuilder terms = map (\term ->
   Insert
     { iTable        = termTable
     , iRows         = [(Nothing, (fmap (\x -> sqlString (unpack x)) (term_iri term)), (sqlString $ unpack $ term_atom term), fmap (\x -> sqlString (unpack x)) (term_description term))]
     , iReturning    = rReturning returnsTerm
     , iOnConflict   = Nothing
     }) terms


statementBuilder :: Statement_ed -> [Int] -> (Insert [Int])
statementBuilder s c =
   Insert
     { iTable        = statementTable
     , iRows         = [(Nothing, sqlString (show (statement_category s)), fmap (\x -> sqlString (unpack x)) (strength s), fmap (\x -> sqlString (unpack x)) (key s), Just (sqlArray (\n -> sqlInt4 n) c))]
     , iReturning    = rReturning returns
     , iOnConflict   = Nothing
     }


insertTerms :: [ATerm] -> PGS.Connection -> IO ()
insertTerms terms conn = do
    mapM_ (runInsert_ conn) (termsBuilder terms)
    return ()

-- Before we can build a statement, we need the formula ids
-- So, from a [Statement_ed] we need to generate formula ids. Unfortunately we can't just map, as formulas can contain other formulas, and we don't know the ids until after they get inserted.
-- So to form a thing we first check to see if it has subformulas; if so, form them and get the resulting ids. Then make an insert (with any returned ids in place), and runInsert_ it. Then return the resulting ids.
-- ... getting back [(Statement_ed, [Int])] - we then map (\(s, c) -> statementBuilder s c) over it, and then mapM_ runQuery

insertStatements :: [Statement_ed] -> PGS.Connection -> IO ()
insertStatements st conn = do
    let logics = map (\x -> (x, concat $ formula x)) st
    statementsWithFormulaIds <- mapM (\(x, y) -> do
      z <- mapM (\w -> pushThroughLogic w formulaBuilder conn) y
      return (x, concat z)) logics
    let statementQueries = map (\(x, y) -> statementBuilder x y) statementsWithFormulaIds
    mapM_ (runInsert_ conn) statementQueries

formulaBuilder :: Logic -> [Int] -> (Insert [Int])
formulaBuilder formula subformulas =
   Insert
     { iTable        = formulaTable
     , iRows         = [(Nothing
                         , sqlString (show (name formula))
                         , case child formula of
                                        LogicEmpty        -> Nothing
                                        LogicText t       -> Just $ sqlString $ show t
                                        LogicCollection _ -> Nothing
                         , case child formula of
                                        LogicEmpty        -> Nothing
                                        LogicText _       -> Nothing
                                        LogicCollection _ -> Just $ sqlArray sqlInt4 subformulas
                         , fmap (\x -> sqlString (unpack x)) (formula_iri formula))]
     , iReturning    = rReturning returnsF
     , iOnConflict   = Nothing
     }


pushThroughLogic :: Logic -> (Logic -> [Int] -> Insert [Int]) -> PGS.Connection -> IO [Int]
pushThroughLogic logic builder conn = do
    case (child logic) of
      LogicEmpty        -> return []
      LogicText _       -> runInsert_ conn (builder logic [])
      LogicCollection c -> do
        -- we need to pushThrough all of the elements of c, then concat the results
        children <- mapM (\x -> pushThroughLogic x builder conn) c
        let flatChildren = concat children
        runInsert_ conn (builder logic flatChildren)

