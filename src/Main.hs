{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import Data.Maybe (listToMaybe)
import Data.ByteString (readFile)
import Data.Time
import Data.Version
import System.Directory
import System.Environment
import System.Exit
import System.Process
import Control.Applicative ((<$>))
import Control.Monad (filterM, void, when, forM_)
import Paths_miv

import qualified Setting as S
import qualified Plugin as P
import Git
import VimScript

expandHomeDirectory :: FilePath -> IO FilePath
expandHomeDirectory ('~':path) = fmap (++path) getHomeDirectory
expandHomeDirectory path = return path

getSettingFile :: IO (Maybe FilePath)
getSettingFile
  = listToMaybe <$> filterM ((=<<) doesFileExist . expandHomeDirectory)
       [ "~/.vimrc.yaml"
       , "~/.vim/.vimrc.yaml"
       , "~/vimrc.yaml"
       , "~/.vim/vimrc.yaml"
       ]

getSetting :: IO (Maybe S.Setting)
getSetting = fmap S.decodeSetting $ getSettingFile >>=
               maybe (return "") ((=<<) readFile . expandHomeDirectory)

getSettingWithError :: IO S.Setting
getSettingWithError =
  getSettingFile >>= \file ->
    case file of
         Nothing -> error "No setting file: ~/.vimrc.yaml"
         Just _ ->
           getSetting >>= \set ->
             case set of
                  Nothing -> error "Parse error"
                  Just setting -> return setting

pluginDirectory :: IO String
pluginDirectory = expandHomeDirectory "~/.vim/miv/"

createPluginDirectory :: IO ()
createPluginDirectory =
  createDirectoryIfMissing True =<< pluginDirectory

printUsage :: IO ()
printUsage = mapM_ putStrLn usage

data Argument = Argument (String, String)
              deriving (Eq, Ord)
instance Show Argument where
  show (Argument (x, y)) = "  "
                        ++ x
                        ++ replicate (10 - length x) ' '
                        ++ y

arguments :: [Argument]
arguments = map Argument
          [ ("install", "Installs all the uninstalled plugins.")
          , ("update", "Updates all the plugins.")
          , ("generate",   "Generate the miv files.")
          , ("helptags",   "Generate the help tags file.")
          , ("list",   "List the plugins.")
          , ("clean", "Clean up unused plugins.")
          , ("edit", "Edit the configuration file.")
          , ("help", "Show this help.")
          ]

usage :: [String]
usage = [ "miv version " ++ showVersion version
        , ""
        , "Usage: miv COMMAND"
        , ""
        , "Commands:"
        ]
     ++ map show arguments

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein a b = last $ foldl f [0..length a] b
  where
    f [] _ = []
    f xs@(x:xs') c = scanl (g c) (x + 1) (zip3 a xs xs')
    g c z (d, x, y) = minimum [y + 1, z + 1, x + fromEnum (c /= d)]

suggestCommand :: String -> IO ()
suggestCommand arg = do
  let commands = [(levenshtein arg x, Argument (x, y)) | (Argument (x, y)) <- arguments]
      mindist = fst (minimum commands)
      mincommands = [y | (x, y) <- commands, x == mindist]
  putStrLn $ "Unknown command: " ++ arg
  putStrLn "Probably:"
  mapM_ print mincommands

data Update = Install | Update deriving Eq

updatePlugin :: Update -> S.Setting -> IO ()
updatePlugin update setting = do
  createPluginDirectory
  createPluginCode setting
  dir <- pluginDirectory
  result <- mapM (updateOnePlugin dir update) $ filter P.sync (S.plugin setting)
  generateHelpTags setting
  let failure = filter ((/=ExitSuccess) . snd) result
  if not (null failure)
     then putStrLn "Error:" >> mapM_ (putStrLn . ("  "++) . P.name . fst) failure
     else putStrLn $ "Success in "
                  ++ (if update == Install then "installing" else "updating")
                  ++ "."

generateHelpTags :: S.Setting -> IO ()
generateHelpTags setting = do
  dir <- pluginDirectory
  let docdir = dir ++ "miv/doc/"
  createDirectoryIfMissing True docdir
  removeDirectoryRecursive docdir
  createDirectoryIfMissing True docdir
  forM_ (map ((++"/doc/") . (dir++) . ('/':) . P.rtpName) (S.plugin setting))
    $ \path ->
        doesDirectoryExist path
          >>= \exists -> when exists (void (system ("cp " ++ path ++ "* " ++ docdir)))
  _ <- system $ "vim -u NONE -i NONE -N -c 'helptags " ++ docdir ++ "' -c 'noa qa!'"
  return ()

updateOnePlugin :: String -> Update -> P.Plugin -> IO (P.Plugin, ExitCode)
updateOnePlugin dir update plugin = do
  let path = dir ++ P.rtpName plugin
      repo = vimScriptRepo (P.name plugin)
  doesDirectoryExist path
    >>= \exists -> if not exists
                      then putStrLn ("Installing: " ++ P.name plugin)
                           >> (,) plugin <$> clone repo path
                      else if update == Install
                              then return (plugin, ExitSuccess)
                              else putStrLn ("Pulling: " ++ P.name plugin)
                                   >> (,) plugin <$> pull path

vimScriptRepo :: String -> String
vimScriptRepo name | '/' `elem` name = name
                   | otherwise = "vim-scripts/" ++ name

cleanDirectory :: S.Setting -> IO ()
cleanDirectory setting = do
  createPluginDirectory
  dir <- pluginDirectory
  createDirectoryIfMissing True dir
  cnt <- getDirectoryContents dir
  let paths = "." : ".." : "miv" : map P.rtpName (S.plugin setting)
      delpath' = [ dir ++ d | d <- cnt, d `notElem` paths ]
  deldir <- filterM doesDirectoryExist delpath'
  delfile <- filterM doesFileExist delpath'
  let delpath = deldir ++ delfile
  if not (null delpath)
     then putStrLn "Remove:"
       >> mapM_ (putStrLn . ("  "++)) delpath
       >> putStrLn "Really? [y/N]"
       >> getChar
       >>= \c -> when (c == 'y' || c == 'Y')
                      (mapM_ removeDirectoryRecursive deldir >> mapM_ removeFile delfile)
     else putStrLn "Clean."

saveScript :: (String, String, [String]) -> IO ()
saveScript (name, relname, code) =
  getCurrentTime >>= \time ->
  writeFile name $ unlines $
            [ "\" " ++ replicate 76 '='
            , "\" Filename: " ++ relname
            , "\" Author: itchyny"
            , "\" Last Change: " ++ show time
            , "\" Generated by miv " ++ showVersion version
            , "\" " ++ replicate 76 '='
            , ""
            , "let s:save_cpo = &cpo"
            , "set cpo&vim"
            , "" ]
         ++ code
         ++ [ ""
            , "let &cpo = s:save_cpo"
            , "unlet s:save_cpo" ]

createPluginCode :: S.Setting -> IO ()
createPluginCode setting = do
  dir <- fmap (++"miv/") pluginDirectory
  createDirectoryIfMissing True dir
  removeDirectoryRecursive dir
  createDirectoryIfMissing True (dir ++ "plugin/")
  createDirectoryIfMissing True (dir ++ "autoload/miv/")
  mapM_ (saveScript . (\(t, s) -> (dir ++ show t, show t, s)))
        (vimScriptToList (gatherScript setting))

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram ['-':arg] = mainProgram [arg]
mainProgram ["help"] = printUsage
mainProgram ["install"] = getSettingWithError >>= updatePlugin Install
mainProgram ["update"] = getSettingWithError >>= updatePlugin Update
mainProgram ["list"] = getSettingWithError >>= mapM_ (putStrLn . P.name) . S.plugin
mainProgram ["clean"] = getSettingWithError >>= cleanDirectory
mainProgram ["edit"] = getSettingFile >>= maybe (return ()) (($) void . system . ("vim "++))
mainProgram ["generate"] = getSettingWithError >>= \s -> createPluginCode s >> generateHelpTags s
mainProgram ["helptags"] = getSettingWithError >>= generateHelpTags
mainProgram [arg] = suggestCommand arg
mainProgram _ = printUsage

main :: IO ()
main = getArgs >>= mainProgram
