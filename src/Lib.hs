{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( parseXML, all_statement_er, Statement_ed, formulas
    ) where

import Text.XML.HXT.Core
import Control.Arrow.ArrowList (listA)
import Data.Text (Text, pack, unpack)
import Data.Maybe
import Control.Monad (mplus)

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

parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
