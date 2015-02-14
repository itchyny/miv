{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile)
import Data.Functor ((<$>))
import Data.List (foldl', nub, isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getDirectoryContents, getHomeDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process (system)
import Control.Monad (filterM, foldM, forM_, liftM, unless, void, when)
import Paths_miv (version)

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
getSetting = liftM (fromMaybe "") getSettingFile >>= expandHomeDirectory >>= S.decodeSetting

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
printUsage = mapM_ T.putStrLn usage

commandHelp :: IO ()
commandHelp = mapM_ print arguments

data Argument = Argument (String, String)
              deriving (Eq, Ord)
instance Show Argument where
  show (Argument (x, y)) = x ++ replicate (10 - length x) ' ' ++ y

arguments :: [Argument]
arguments = map Argument
          [ ("install" , "Installs the uninstalled plugins.")
          , ("update"  , "Updates the plugins.")
          , ("generate", "Generate the miv files.")
          , ("helptags", "Generate the help tags file.")
          , ("each"    , "Execute command at each plugin directory.")
          , ("path"    , "Print the paths of each plugins.")
          , ("list"    , "List the plugins.")
          , ("clean"   , "Clean up unused plugins.")
          , ("edit"    , "Edit the configuration file.")
          , ("command" , "Show the sub-commands.")
          , ("version" , "Show the version of miv.")
          , ("help"    , "Show this help.")
          ]

nameversion :: T.Text
nameversion = T.pack $ "miv version " ++ showVersion version

usage :: [T.Text]
usage = nameversion :
        [ ""
        , "Usage: miv COMMAND"
        , ""
        , "Commands:"
        ]
     ++ map (T.append "  " . T.pack . show) arguments
     ++ [ ""
        , "You can update the plugins by the following command:"
        , "  miv update"
        , ""
        , "You can specify the name of plugins:"
        , "  miv install plugin1 plugin2"
        , "  miv update plugin1 plugin2"
        , ""
        , "Normally, outdated plugins are ignored but"
        , "you can update all the plugins with trailing !:"
        , "  miv update!"
        , ""
        , "You can use `miv each' to execute some commands."
        , "  miv each pwd"
        , "  miv each git gc"
        , "  miv each git diff"
        , "  miv each 'echo $(du -sh .) $(basename $(pwd))'"
        , "  miv each 'echo \"\\033[1;32m$(basename $(pwd))\\033[0m\" && git diff'"
        , ""
        , "This software is released under the MIT License."
        , "This software is distributed at https://github.com/itchyny/miv."
        , "Report a bug of this software at https://github.com/itchyny/miv/issues."
        , "The author is itchyny <https://github.com/itchyny>."
        ]

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein a b = last $ foldl' f [0..length a] b
  where
    f [] _ = []
    f xs@(x:xs') c = scanl (g c) (x + 1) (zip3 a xs xs')
    g c z (d, x, y) = minimum [y + 1, z + 1, x + fromEnum (c /= d)]

suggestCommand :: String -> IO ()
suggestCommand arg = do
  let commands = [(levenshtein arg x, Argument (x, y)) | (Argument (x, y)) <- arguments]
      mindist = fst (minimum commands)
      mincommands = [y | (x, y) <- commands, x == mindist]
      prefixcommands = [y | y@(Argument (x, _)) <- arguments, arg `isPrefixOf` x || x `isPrefixOf` arg]
      containedcommands = [y | y@(Argument (x, _)) <- arguments, length arg > 1 && arg `isContainedIn` x]
  putStrLn $ "Unknown command: " ++ arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "++) . show) (if null prefixcommands then nub (containedcommands ++ mincommands) else prefixcommands)

isContainedIn :: Eq a => [a] -> [a] -> Bool
isContainedIn xxs@(x:xs) (y:ys) = x == y && xs `isContainedIn` ys || xxs `isContainedIn` ys
isContainedIn [] _ = True
isContainedIn _ [] = False

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
  let installedPlugins = map show (S.plugin setting)
      unknownPlugins = filter (`notElem` installedPlugins) (fromMaybe [] plugins)
  unless (null unknownPlugins)
     $ mapM_ (suggestPlugin (S.plugin setting)) unknownPlugins
  createPluginDirectory
  dir <- pluginDirectory
  let specified p = show p `elem` fromMaybe [] plugins || plugins == Just []
  let filterplugin p = isNothing plugins || specified p
  let ps = filter filterplugin (S.plugin setting)
  time <- maximum <$> mapM (lastUpdatePlugin dir) ps
  result <- foldM (\s p -> updateOnePlugin time dir update (specified p) s p) (P.defaultPlugin, ExitSuccess) ps
  if snd result /= ExitSuccess
     then putStrLn "Error:" >> T.putStrLn ("  " <> P.name (fst result))
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
  forM_ (map (\p -> dir ++ show p ++ "/doc/") (S.plugin setting))
    $ \path ->
        doesDirectoryExist path
          >>= \exists -> when exists $ void
              $ system $ unwords ["cd", singleQuote' path,
                                  "&& cp *", singleQuote' docdir, "2>/dev/null"]
  _ <- system $ "vim -u NONE -i NONE -N -e -s -c 'helptags " ++ docdir ++ "' -c quit"
  putStrLn "Success in processing helptags."

lastUpdatePlugin :: String -> P.Plugin -> IO Integer
lastUpdatePlugin dir plugin = do
  let path = dir ++ show plugin ++ "/.git"
  doesDirectoryExist path
    >>= \exists -> if exists then lastUpdate path else return 0

updateOnePlugin :: Integer -> String -> Update -> Bool -> (P.Plugin, ExitCode) -> P.Plugin -> IO (P.Plugin, ExitCode)
updateOnePlugin _ _ _ _ x@(_, ExitFailure 2) _ = return x
updateOnePlugin time dir update specified (_, _) plugin = do
  let path = dir ++ show plugin
      repo = vimScriptRepo (T.unpack $ P.name plugin)
      cloneCommand = if P.submodule plugin then cloneSubmodule else clone
      pullCommand = if P.submodule plugin then pullSubmodule else pull
  doesDirectoryExist path
    >>= \exists ->
      if not exists
         then T.putStrLn ("Installing: " <> P.name plugin)
              >> (,) plugin <$> cloneCommand repo path
         else if update == Install || not (P.sync plugin)
                 then return (plugin, ExitSuccess)
                 else lastUpdate path >>= \lastUpdateTime ->
                      if lastUpdateTime < time - 60 * 60 * 24 * 30 && not specified
                         then T.putStrLn ("Outdated: " <> P.name plugin)
                              >> return (plugin, ExitSuccess)
                         else T.putStrLn ("Pulling: " <> P.name plugin)
                              >> (,) plugin <$> pullCommand path

vimScriptRepo :: String -> String
vimScriptRepo name | '/' `elem` name = name
                   | otherwise = "vim-scripts/" ++ name

listPlugin :: S.Setting -> IO ()
listPlugin setting = mapM_ putStrLn $ space $ map format $ S.plugin setting
  where format p = [show p, T.unpack $ P.name p, gitUrl (vimScriptRepo (T.unpack $ P.name p))]
        space xs =
          let max0 = maximum (map (length . (!!0)) xs) + 1
              max1 = maximum (map (length . (!!1)) xs) + 1
              in map (\(as:bs:cs:_) -> as ++ replicate (max0 - length as) ' ' ++ bs ++ replicate (max1 - length bs) ' ' ++ cs) xs

cleanDirectory :: S.Setting -> IO ()
cleanDirectory setting = do
  createPluginDirectory
  dir <- pluginDirectory
  createDirectoryIfMissing True dir
  cnt <- getDirectoryContents dir
  let paths = "." : ".." : "miv" : map show (S.plugin setting)
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

saveScript :: (String, Place, [T.Text]) -> IO ()
saveScript (dir, place, code) =
  let isftplugin = isFtplugin place
      relname = show place
      name = dir ++ relname 
      isallascii = all (T.all (<='~')) code in
  getCurrentTime >>= \time ->
  T.writeFile name $ T.unlines $
            [ "\" Filename: " <> T.pack relname
            , "\" Last Change: " <> T.pack (show time)
            , "\" Generated by miv " <> T.pack (showVersion version)
            , "" ]
         ++ (if isallascii then [] else [ "scriptencoding utf-8", "" ])
         ++ (if isftplugin then [ "if exists('b:did_ftplugin')"
                                , "  finish"
                                , "endif"
                                , "" ]
                           else [])
         ++ [ "let s:save_cpo = &cpo"
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
  cleanAndCreateDirectory (dir ++ "ftplugin/")
  mapM_ (saveScript . (\(t, s) -> (dir, t, s)))
        (vimScriptToList (gatherScript setting))
  putStrLn "Success in generating Vim scripts of miv."

eachPlugin :: String -> S.Setting -> IO ()
eachPlugin command setting = do
  createPluginDirectory
  dir <- pluginDirectory
  result <- foldM (eachOnePlugin command dir) (P.defaultPlugin, ExitSuccess) (S.plugin setting)
  when (snd result /= ExitSuccess)
     $ putStrLn "Error:" >> T.putStrLn ("  " <> P.name (fst result))

eachOnePlugin :: String -> String -> (P.Plugin, ExitCode) -> P.Plugin -> IO (P.Plugin, ExitCode)
eachOnePlugin _ _ x@(_, ExitFailure 2) _ = return x
eachOnePlugin command dir (_, _) plugin = do
  let path = dir ++ show plugin
  doesDirectoryExist path
    >>= \exists ->
      if not exists
         then return (plugin, ExitSuccess)
         else (,) plugin <$> system (unwords ["cd", singleQuote' path, "&&", command])

eachHelp :: IO ()
eachHelp = mapM_ putStrLn [ "Specify command:", "  miv each [command]" ]

pathPlugin :: [String] -> S.Setting -> IO ()
pathPlugin plugins setting = do
  let ps = filter (\p -> show p `elem` plugins || null plugins) (S.plugin setting)
  dir <- pluginDirectory
  forM_ ps (\plugin -> putStrLn $ dir ++ show plugin)

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram ['-':arg] = mainProgram [arg]
mainProgram ["help"] = printUsage
mainProgram ["version"] = T.putStrLn nameversion
mainProgram ["install"] = getSettingWithError >>= updatePlugin Install Nothing
mainProgram ["update"] = getSettingWithError >>= updatePlugin Update Nothing
mainProgram ["update!"] = getSettingWithError >>= updatePlugin Update (Just [])
mainProgram ["update", "!"] = getSettingWithError >>= updatePlugin Update (Just [])
mainProgram ["each"] = eachHelp
mainProgram ["list"] = getSettingWithError >>= listPlugin
mainProgram ["command"] = commandHelp
mainProgram ["clean"] = getSettingWithError >>= cleanDirectory
mainProgram ["edit"] = getSettingFile >>= maybe (return ()) (($) void . system . ("vim "++))
mainProgram ["generate"] = getSettingWithError >>= generatePluginCode
mainProgram ["helptags"] = getSettingWithError >>= generateHelpTags
mainProgram ["path"] = getSettingWithError >>= pathPlugin []
mainProgram [arg] = suggestCommand arg
mainProgram ("install":args) = getSettingWithError >>= updatePlugin Install (Just args)
mainProgram ("update":args) = getSettingWithError >>= updatePlugin Update (Just args)
mainProgram ("each":args) = getSettingWithError >>= eachPlugin (unwords args)
mainProgram ("path":args) = getSettingWithError >>= pathPlugin args
mainProgram (('-':arg):args) = mainProgram (arg:args)
mainProgram _ = printUsage

main :: IO ()
main = getArgs >>= mainProgram
