module I18N.Gettext.TH
(gettext, __, gettexts)
where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe
import System.Directory
import Data.IORef
import Control.Monad
import Data.Bifunctor
import Data.Char (isSpace)
import Data.List

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
      "msgstr \"" ++ str ++ "\""
      ]
  let trans = TL.toStrict $ G.gettext catalog (packStr str)
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


gettextsDecs  :: String -> Q [Dec]
gettextsDecs str = do
  createPotFile
  loc <- location
  let msgs = map splitKeyMsg $ parseLines str  
  runIO $ appendFile potFileName $ unlines $ concat [[
      "",
      "#: " ++ (loc_filename loc) ++ ":0", -- TODO line nr or char pos
      "msgid " ++ show msg,
      "msgstr " ++ show msg 
      ] | (_, msg) <- msgs ]    

  forM msgs $ \ (key, msg) ->
              let trans = TL.toStrict $ G.gettext catalog (packStr msg) in do
                 e <-  [| trans |]
                 return $ FunD (mkName key) [Clause [] (NormalB e) []]
                 

         

parseLines :: String -> [String]
parseLines text = go [] (lines text)
   where go acc [] = reverse acc
         go acc (('#':_):lines') = go acc lines'
         go acc (line:lines') =
           if all isSpace line then go acc lines'
             else collect (join acc) [line] lines'
         collect :: ([String] -> [String]) -> [String] -> [String] -> [String]
         collect j cl [] = go (j cl) [] 
         collect j cl lines'@(h@(c:d):t) =
                   if isSpace c then collect j ((dropWhile isSpace d):cl) t
                     else go (j cl) lines'
         join acc cl = (intercalate "\n" $ reverse cl):acc

splitKeyMsg :: String -> (String, String)        
splitKeyMsg line =  bimap trim (trim . tail)$ span (/= ':') line 
  

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


gettexts :: QuasiQuoter
gettexts = QuasiQuoter
  { quoteExp  = error "Usage as a decl is not supported" 
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec = gettextsDecs
  }
