{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( parseXML, all_statement_er, Statement_ed, formulas
    ) where

import Text.XML.HXT.Core
import Control.Arrow.ArrowList (listA)
import Data.Text (Text, pack, unpack)
import Data.Maybe

data StatementCategory = PrescriptiveStatement | ConstitutiveStatement deriving (Show, Eq)

data Statement_ed = Statement_ed { statement_category :: StatementCategory, strength, key :: Maybe Text, formula :: Maybe [Logic]} deriving (Show, Eq)

data LogicData = LogicText Text | LogicCollection [Logic] | LogicEmpty deriving (Show, Eq)
data Logic = Logic { name :: Text, child :: LogicData } deriving (Show, Eq)

all_statement_er = multi (isElem >>> ((hasName "lrml:PrescriptiveStatement") <+> (hasName "lrml:ConstitutiveStatement"))) >>>
  statement_er

statement_er = proc stmt -> do
                      tagname <- getName -< stmt
                      sc <- arr statement_type -< pack tagname
                      key <- (getAttrl >>> hasName "key" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) <+> (arr (\x -> Nothing)) -< stmt
                      strength <- (getChildren >>> hasName "ruleml:Rule" >>> getAttrl >>> hasName "strength" >>> getChildren >>> getText >>> arr (\x -> Just (pack x))) <+> (arr (\x -> Nothing)) -< stmt
                      formula <- (getChildren >>> hasName "ruleml:Rule" >>> formulas >>> arr (\x -> Just x)) <+> (arr (\x -> Nothing)) -< stmt
                      returnA -< Statement_ed {
                        statement_category = sc,
                        strength = strength,
                        key = key,
                        formula = formula }

formulas = proc parent -> do
                  logics <- listA (getChildren >>> (getName &&&
                      (catA [isElem >>> getChildren >>> (getText >>> arr (\x -> LogicText (pack x))), withDefault (isElem >>> formulas >>> arr (\x -> if x == [] then LogicEmpty else LogicCollection (x))) LogicEmpty])) >>>
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
