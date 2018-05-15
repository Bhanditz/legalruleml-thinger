{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Control.Arrow.ArrowList (listA)
import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data StatementCategory = PrescriptiveStatement | ConstituiveStatement deriving (Show, Eq)

data Statement_ed = Statement_ed { statement_category :: StatementCategory, strength, key, formula_frag_id :: Text}

-- statement_er :: XmlTree -> [Statement_ed]
-- statement_er doc = let statement_s = ((multi (isElem >>> ((hasName "PrescriptiveStatement") <+> (hasName "ConstituiveStatement")))) doc) in
--                        map (\stmt -> Statement_ed (statement_type stmt) "" "" "") statement_s

all_statement_er = multi (isElem >>> ((hasName "PrescriptiveStatement") <+> (hasName "ConstituiveStatement"))) >>>
  proc stmt -> do
    processed <- arr statement_er -< stmt
    returnA -< processed

statement_er stmt = proc stmt -> do
                      tagname <- getName -< stmt
                      sc <- arr statement_type -< pack tagname
                      returnA -< Statement_ed {
                        statement_category = sc,
                        strength = "defeasible",
                        key = "",
                        formula_frag_id = "0" }

statement_type :: Text -> StatementCategory
statement_type = \case
                           "PrescriptiveStatement" -> PrescriptiveStatement
                           "ConstituiveStatement"  -> ConstituiveStatement
                           _                       -> error "Not a statement"

parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
