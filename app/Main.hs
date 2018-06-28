{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings, Arrows, NoMonomorphismRestriction, LambdaCase #-}

module Main where

import Lib
import Text.XML.HXT.Core

main :: IO ()
main = do
  -- xml   <- return $ parseXML "test.xml"
  stmts <- runX (readDocument [] "test.xml" >>> getChildren >>> isElem >>> hasName "lrml:LegalRuleML" >>> all_statement_er)
  case stmts of
    []  -> putStrLn "No statements found."
    w -> print w
--  (case stmts of
--    [IOLA stmt] -> print stmt
--    _           -> print "")
