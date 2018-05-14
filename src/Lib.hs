{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Lib
    ( someFunc
    ) where

import Text.XML.HXT.Core
import Data.Text (Text, pack)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data StatementCategory = PrescriptiveStatement | ConstituiveStatement deriving (Show, Eq)

data Statement_ed = Statement_ed { statement_category :: StatementCategory, strength, key, formula_frag_id :: Text}

-- statement_er :: XmlTree -> [Statement_ed]
-- statement_er doc = let statement_s = ((multi (isElem >>> ((hasName "PrescriptiveStatement") <+> (hasName "ConstituiveStatement")))) doc) in
--                        map (\stmt -> Statement_ed (statement_type stmt) "" "" "") statement_s

statement_er doc = proc doc -> do
                     returnA -< Statement_ed {
                       statement_category = PrescriptiveStatement,
                       strength = "defeasible",
                       key = "",
                       formula_frag_id = "0" }

statement_type :: Text -> StatementCategory
statement_type = \case
                           "PrescriptiveStatement" -> PrescriptiveStatement
                           "ConstituiveStatement"  -> ConstituiveStatement
                           _                       -> error "Not a statement"
