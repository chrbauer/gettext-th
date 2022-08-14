{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import I18N.Gettext.TH
import qualified Data.Text.IO as T

[gettexts|
explain: It it also possible to define multiple messages
  in one quasiquotation. With leading whitespace it is possible to
  define multiline texts.
howtouse:  The quasi quotation will generate for each message a binding with the given name.
|]

main :: IO ()
main = do
  T.putStrLn [__|Hello, Haskell|]
  T.putStrLn explain
  T.putStrLn howtouse
