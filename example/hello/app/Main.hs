{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import I18N.Gettext.TH
import qualified Data.Text.IO as T

main :: IO ()

main = T.putStrLn [__|Hello, Haskell|]
