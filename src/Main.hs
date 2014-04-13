{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import Data.Functor
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.ByteString (readFile)
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import System.Directory
import System.Environment
import System.Exit
import System.IO (hFlush, stdout)
import System.Process
import Control.Monad
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
       , "~/_vimrc.yaml"
       , "~/.vim/_vimrc.yaml"
       , "~/_vim/_vimrc.yaml"
       , "~/vimfiles/.vimrc.yaml"
       , "~/vimfiles/vimrc.yaml"
       , "~/vimfiles/_vimrc.yaml"
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

commandHelp :: IO ()
commandHelp = mapM_ print arguments

data Argument = Argument (String, String)
              deriving (Eq, Ord)
instance Show Argument where
  show (Argument (x, y)) = x ++ replicate (10 - length x) ' ' ++ y

arguments :: [Argument]
arguments = map Argument
          [ ("install" , "Installs all the uninstalled plugins.")
          , ("update"  , "Updates all the plugins.")
          , ("generate", "Generate the miv files.")
          , ("helptags", "Generate the help tags file.")
          , ("list"    , "List the plugins.")
          , ("clean"   , "Clean up unused plugins.")
          , ("edit"    , "Edit the configuration file.")
          , ("command" , "Show the commands.")
          , ("help"    , "Show this help.")
          ]

usage :: [String]
usage = [ "miv version " ++ showVersion version
        , ""
        , "Usage: miv COMMAND"
        , ""
        , "Commands:"
        ]
     ++ map (("  "++) . show) arguments
     ++ [ ""
        , "You can specify the name of plugins:"
        , "  miv install plugin1 plugin2"
        , "  miv update plugin1 plugin2"
        ]

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
  mapM_ (putStrLn . ("  "++) . show) mincommands

suggestPlugin :: [P.Plugin] -> String -> IO ()
suggestPlugin plugin arg = do
  let plugins = [(levenshtein arg (show p), p) | p <- plugin]
      mindist = fst (minimum plugins)
      minplugins = [y | (x, y) <- plugins, x == mindist]
  putStrLn $ "Unknown plugin: " ++ arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "++) . show) minplugins

data Update = Install | Update deriving Eq
instance Show Update where
  show Install = "installing"
  show Update = "updating"

updatePlugin :: Update -> Maybe [String] -> S.Setting -> IO ()
updatePlugin update plugins setting = do
  let installedPlugins = map P.rtpName (S.plugin setting)
      unknownPlugins = filter (`notElem` installedPlugins) (fromMaybe [] plugins)
  unless (null unknownPlugins)
     $ mapM_ (suggestPlugin (S.plugin setting)) unknownPlugins
  createPluginDirectory
  dir <- pluginDirectory
  let specified p = P.rtpName p `elem` fromMaybe [] plugins
  let filterplugin p = P.sync p && (isNothing plugins || specified p)
  let ps = filter filterplugin (S.plugin setting)
  time <- maximum <$> mapM (lastUpdatePlugin dir) ps
  result <- foldM (\s p -> updateOnePlugin time dir update (specified p) s p) (P.defaultPlugin, ExitSuccess) ps
  if snd result /= ExitSuccess
     then putStrLn "Error:" >> putStrLn ("  " ++ P.name (fst result))
     else putStrLn ("Success in " ++ show update ++ ".")
       >> generatePluginCode setting
       >> generateHelpTags setting

cleanAndCreateDirectory :: String -> IO ()
cleanAndCreateDirectory directory = do
  createDirectoryIfMissing True directory
  removeDirectoryRecursive directory
  createDirectoryIfMissing True directory

generateHelpTags :: S.Setting -> IO ()
generateHelpTags setting = do
  dir <- pluginDirectory
  let docdir = dir ++ "miv/doc/"
  cleanAndCreateDirectory docdir
  forM_ (map (\p -> dir ++ P.rtpName p ++ "/doc/") (S.plugin setting))
    $ \path ->
        doesDirectoryExist path
          >>= \exists -> when exists (void (system ("cd " ++ singleQuote path ++ " && cp * " ++ singleQuote docdir)))
  _ <- system $ "vim -u NONE -i NONE -N -c 'helptags " ++ docdir ++ "' -c 'noa qa!'"
  putStrLn "Success in processing helptags."

lastUpdatePlugin :: String -> P.Plugin -> IO Integer
lastUpdatePlugin dir plugin = do
  let path = dir ++ P.rtpName plugin
  doesDirectoryExist path
    >>= \exists -> if exists then lastUpdate path else return 0

updateOnePlugin :: Integer -> String -> Update -> Bool -> (P.Plugin, ExitCode) -> P.Plugin -> IO (P.Plugin, ExitCode)
updateOnePlugin _ _ _ _ x@(_, ExitFailure 2) _ = return x
updateOnePlugin time dir update specified (_, _) plugin = do
  let path = dir ++ P.rtpName plugin
      repo = vimScriptRepo (P.name plugin)
  doesDirectoryExist path
    >>= \exists -> 
      if not exists
         then putStrLn ("Installing: " ++ P.name plugin)
              >> (,) plugin <$> clone repo path
         else if update == Install
                 then return (plugin, ExitSuccess)
                 else lastUpdate path >>= \lastUpdateTime ->
                      if lastUpdateTime < time - 60 * 60 * 24 * 30 && not specified
                         then putStrLn ("Outdated: " ++ P.name plugin)
                              >> return (plugin, ExitSuccess)
                         else putStrLn ("Pulling: " ++ P.name plugin)
                              >> (,) plugin <$> pull path

vimScriptRepo :: String -> String
vimScriptRepo name | '/' `elem` name = name
                   | otherwise = "vim-scripts/" ++ name

listPlugin :: S.Setting -> IO ()
listPlugin setting = mapM_ (putStrLn . format) $ S.plugin setting
  where format p = space (P.rtpName p, P.name p)
        space (x, y) = x ++ replicate (max 1 (maxlen + 1 - length x)) ' ' ++ y
        maxlen = maximum (map (length . P.rtpName) (S.plugin setting))

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
       >> putStr "Really? [y/N] "
       >> hFlush stdout
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

generatePluginCode :: S.Setting -> IO ()
generatePluginCode setting = do
  dir <- fmap (++"miv/") pluginDirectory
  createDirectoryIfMissing True dir
  cleanAndCreateDirectory (dir ++ "plugin/")
  cleanAndCreateDirectory (dir ++ "autoload/miv/")
  mapM_ (saveScript . (\(t, s) -> (dir ++ show t, show t, s)))
        (vimScriptToList (gatherScript setting))
  putStrLn "Success in generating Vim scripts of miv."

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram ['-':arg] = mainProgram [arg]
mainProgram ["help"] = printUsage
mainProgram ["install"] = getSettingWithError >>= updatePlugin Install Nothing
mainProgram ["update"] = getSettingWithError >>= updatePlugin Update Nothing
mainProgram ["list"] = getSettingWithError >>= listPlugin
mainProgram ["command"] = commandHelp
mainProgram ["clean"] = getSettingWithError >>= cleanDirectory
mainProgram ["edit"] = getSettingFile >>= maybe (return ()) (($) void . system . ("vim "++))
mainProgram ["generate"] = getSettingWithError >>= generatePluginCode
mainProgram ["helptags"] = getSettingWithError >>= generateHelpTags
mainProgram [arg] = suggestCommand arg
mainProgram ("install":args) = getSettingWithError >>= updatePlugin Install (Just args)
mainProgram ("update":args) = getSettingWithError >>= updatePlugin Update (Just args)
mainProgram (('-':arg):args) = mainProgram (arg:args)
mainProgram _ = printUsage

main :: IO ()
main = getArgs >>= mainProgram
