module I18N.Gettext.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Gettext as G
import Data.Gettext (Catalog, loadCatalog)
import System.FilePath.Posix


{-# NOINLINE firstCall #-}
firstCall :: IORef Bool
firstCall = unsafePerformIO $ newIORef True

potFileName :: FilePath
potFileName = "po/messages.pot"


poFileName :: FilePath
poFileName = replaceExtension potFileName ".po"

moFileName :: FilePath
moFileName = replaceExtension potFileName ".mo"

{-# NOINLINE catalog #-}
catalog :: Catalog
catalog = unsafePerformIO $ loadCatalog moFileName


header :: String
header = unlines [
  "# SOME DESCRIPTIVE TITLE.",
  "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER",
  "# This file is distributed under the same license as the PACKAGE package.",
  "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.",
  "#",
  "#: hello.c:140",
  "#, fuzzy",
  "msgid \"\"",
  "msgstr \"\"",
  "\"Project-Id-Version: PACKAGE VERSION\\n\"",
  "\"Report-Msgid-Bugs-To: \\n\"",
  "\"POT-Creation-Date: 2022-08-03 07:51+0200\\n\"",
  "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"",
  "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"",
  "\"Language-Team: LANGUAGE <LL@li.org>\\n\"",
  "\"Language: \\n\"",
  "\"MIME-Version: 1.0\\n\"",
  "\"Content-Type: text/plain; charset=CHARSET\\n\"",
  "\"Content-Transfer-Encoding: 8bit\\n\""
  ]

createPotFile :: Q ()
createPotFile = do
  runIO $ do
    f <- readIORef firstCall
    when f $ do
      writeIORef firstCall False
      createDirectoryIfMissing True (takeDirectory potFileName)
      potE <- doesFileExist potFileName    
      when potE $
        renameFile potFileName (potFileName ++ ".bak")

      writeFile potFileName header
    --addDependentFile poFileName


packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack


gettextQ  :: String -> Q Exp
gettextQ str = do
  createPotFile
  loc <- location
  runIO $ appendFile potFileName $ unlines [
      "",
      "#: " ++ (loc_filename loc) ++ ":0", -- TODO line nr or char pos
      "msgid \"" ++ str ++ "\"",
      "msgstr \"\""
      ]
  let trans = TL.toStrict $ G.gettext catalog (packStr str)
  runIO $ putStrLn $ "gettext: \"" ++ str  ++ "\" -> \"" ++ (T.unpack trans) ++ "\""
  [| trans |]


gettext :: QuasiQuoter
gettext = QuasiQuoter
  { quoteExp  = gettextQ
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = error "Usage as a decl is not supported"

  }

__ :: QuasiQuoter
__ = gettext
