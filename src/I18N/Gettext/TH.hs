module I18N.Gettext.TH
(gettext, __)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Instances.TH.Lift()
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Control.Monad
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

--import Control.Exception (catch, IOException)

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Gettext as G
import Data.Gettext (Catalog, loadCatalog)
import System.FilePath.Posix
import System.IO

{-# NOINLINE knownMsgs #-}
knownMsgs :: IORef (Set String)
knownMsgs = unsafePerformIO $ newIORef S.empty

potFileName :: Loc -> FilePath
potFileName loc = "po/messages_" <> takeBaseName (loc_filename loc) <> ".pot"


-- poFileName :: FilePath
-- poFileName = replaceExtension potFileName ".po"

moFileName :: FilePath
moFileName = "po/messages.mo"

{-# NOINLINE catalog #-}
catalog :: Maybe Catalog
catalog = unsafePerformIO $ do
  fe <- doesFileExist moFileName
  if fe then Just <$> loadCatalog moFileName
   else return Nothing


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
  "\"Content-Type: text/plain; charset=utf-8\\n\"",
  "\"Content-Transfer-Encoding: 8bit\\n\""
  ]




writeFileUtf8 :: FilePath -> IOMode -> String -> IO ()
writeFileUtf8 f mode txt = withFile f mode (\ hdl -> do
                                               hSetEncoding hdl utf8
                                               hPutStr hdl txt)

createPotFile :: Q ()
createPotFile = do
  loc <- location
  let potFn = potFileName loc
  moFn <- runIO $ do
      createDirectoryIfMissing True (takeDirectory potFn)
      potE <- doesFileExist potFn
      when potE $
        renameFile potFn (potFn ++ ".bak")

      writeFileUtf8 potFn WriteMode header
      moE <- doesFileExist moFileName
      if moE then makeAbsolute moFileName
        else return ""
  unless (null moFn) $ addDependentFile moFn


packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

lookupText :: String -> Text
lookupText str = maybe (T.pack str) (\ c -> TL.toStrict $ G.gettext c $ packStr str) catalog


gettextQ  :: String -> Q Exp
gettextQ str = do
  kmsgs <- runIO $ do
     kmsgs <- readIORef knownMsgs
     writeIORef knownMsgs (S.insert str kmsgs)
     return kmsgs

  when (S.null kmsgs) createPotFile
  when (str `S.notMember` kmsgs) $ do
    loc <- location
    runIO $ writeFileUtf8 (potFileName loc) AppendMode $ unlines $ poEntry loc str
  let trans = lookupText str
  [| trans |]

quote :: String -> String
quote s =  '"':escape s
  where escape [] = "\""
        escape ('"':s') = '\\':'"':escape s'
        escape ('\n':s') = '\\':'n':escape s'
        escape ('\r':s') = escape s'
        escape (c:s') = c:escape s'


poEntry :: Loc -> String -> [String]
poEntry loc msg = [
      "",
      "#: " ++ (loc_filename loc) ++ ":0", -- TODO line nr or char pos
      "msgid " ++ quote msg,
      "msgstr " ++ quote msg
      ]

gettextsDecs  :: String -> Q [Dec]
gettextsDecs str = do
  let msgs = map splitKeyMsg $ parseLines str
  kmsgs <- runIO $ do
     kmsgs <- readIORef knownMsgs
     writeIORef knownMsgs (foldl' (\ acc (_, msg) -> msg `S.insert` acc) kmsgs msgs)
     return kmsgs
  when (S.null kmsgs) createPotFile
  loc <- location

  runIO $ writeFileUtf8 (potFileName loc) AppendMode $ unlines $ concat [ poEntry loc msg | (_, msg) <- msgs, msg `S.notMember` kmsgs ]

  forM msgs $ \ (key, msg) ->
              let trans = lookupText msg in do
                 funD (mkName key) [clause [] (normalB [| trans |]) []]




parseLines :: String -> [String]
parseLines text = go [] (lines text)
   where go acc [] = reverse acc
         go acc (('#':_):lines') = go acc lines'
         go acc (line:lines') =
           if all isSpace line then go acc lines'
             else collect (join acc) [line] lines'
         collect :: ([String] -> [String]) -> [String] -> [String] -> [String]
         collect j cl [] = go (j cl) []
         collect j cl ([]:t) = go (j cl) t
         collect j cl lines'@((c:d):t) =
                   if isSpace c then collect j ((dropWhile isSpace d):cl) t
                     else go (j cl) lines'
         join acc cl = (intercalate "\n" $ reverse cl):acc

splitKeyMsg :: String -> (String, String)
splitKeyMsg line =  bimap trim (trim . tail) $ span (/= ':') line


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


gettext :: QuasiQuoter
gettext = QuasiQuoter
  { quoteExp  = gettextQ
  , quotePat  = error "Usage as a pattern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = gettextsDecs
  }

__ :: QuasiQuoter
__ = gettext

