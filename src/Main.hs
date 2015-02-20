{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Prelude hiding (readFile, writeFile, unwords, unlines, putStrLn, putStr, show)
import qualified Prelude as P
import Data.Functor ((<$>))
import Data.List (foldl', nub)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text, unwords, unlines, pack, unpack)
import Data.Text.IO (putStrLn, putStr, writeFile)
import Data.Time (getZonedTime)
import Data.Version (showVersion)
import qualified System.Directory as SD
import qualified System.Environment as SE
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO (hFlush, stdout)
import Control.Monad (filterM, foldM, forM_, liftM, unless, void, when)
import Paths_miv (version)

import qualified Setting as S
import qualified Plugin as P
import Git
import VimScript
import ShowText

nameversion :: Text
nameversion = "miv " <> pack (showVersion version)

createDirectoryIfMissing :: Text -> IO ()
createDirectoryIfMissing = SD.createDirectoryIfMissing True . unpack

doesDirectoryExist :: Text -> IO Bool
doesDirectoryExist = SD.doesDirectoryExist . unpack

doesFileExist :: Text -> IO Bool
doesFileExist = SD.doesFileExist . unpack

getDirectoryContents :: Text -> IO [Text]
getDirectoryContents = fmap (map pack) . SD.getDirectoryContents . unpack

removeDirectoryRecursive :: Text -> IO ()
removeDirectoryRecursive = SD.removeDirectoryRecursive . unpack

removeFile :: Text -> IO ()
removeFile = SD.removeFile . unpack

getHomeDirectory :: IO Text
getHomeDirectory = fmap pack SD.getHomeDirectory

getArgs :: IO [Text]
getArgs = fmap (map pack) SE.getArgs

expandHomeDirectory :: Text -> IO Text
expandHomeDirectory path | T.take 1 path == "~" = fmap (<>T.tail path) getHomeDirectory
                         | otherwise = return path

getSettingFile :: IO (Maybe Text)
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
getSetting = liftM (fromMaybe "") getSettingFile >>= expandHomeDirectory >>= S.decodeSetting . unpack

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

pluginDirectory :: IO Text
pluginDirectory = do
  dir <- expandHomeDirectory "~/.vim/miv/"
  windir <- expandHomeDirectory "~/vimfiles/miv/"
  exists <- doesDirectoryExist dir
  return (if exists then dir else (if os `elem` ["windows", "mingw"] then windir else dir))

createPluginDirectory :: IO ()
createPluginDirectory =
  createDirectoryIfMissing =<< pluginDirectory

printUsage :: IO ()
printUsage = mapM_ putStrLn usage

commandHelp :: IO ()
commandHelp = mapM_ (putStrLn . (show :: Argument -> Text)) arguments

data Argument = Argument (Text, Text)
              deriving (Eq, Ord)
instance ShowText Argument where
  show (Argument (x, y)) = x <> T.replicate (10 - T.length x) " " <> y

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

usage :: [Text]
usage = [ nameversion
        , ""
        , "Usage: miv COMMAND"
        , ""
        , "Commands:"
        ]
     <> map (T.append "  " . show) arguments
     <> [ ""
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

suggestCommand :: Text -> IO ()
suggestCommand arg = do
  let commands = [(levenshtein (unpack arg) (unpack x), Argument (x, y)) | (Argument (x, y)) <- arguments]
      mindist = fst (minimum commands)
      mincommands = [y | (x, y) <- commands, x == mindist]
      prefixcommands = [y | y@(Argument (x, _)) <- arguments, arg `T.isPrefixOf` x || x `T.isPrefixOf` arg]
      containedcommands = [y | y@(Argument (x, _)) <- arguments, T.length arg > 1 && unpack arg `isContainedIn` unpack x]
  putStrLn $ "Unknown command: " <> arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "<>) . show) (if null prefixcommands then nub (containedcommands <> mincommands) else prefixcommands)

isContainedIn :: Eq a => [a] -> [a] -> Bool
isContainedIn xxs@(x:xs) (y:ys) = x == y && xs `isContainedIn` ys || xxs `isContainedIn` ys
isContainedIn [] _ = True
isContainedIn _ [] = False

suggestPlugin :: [P.Plugin] -> Text -> IO ()
suggestPlugin plugin arg = do
  let plugins = [(levenshtein (unpack arg) (P.show p), p) | p <- plugin]
      mindist = fst (minimum plugins)
      minplugins = [y | (x, y) <- plugins, x == mindist]
  putStrLn $ "Unknown plugin: " <> arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "<>) . show) minplugins

data Update = Install | Update deriving Eq
instance ShowText Update where
  show Install = "installing"
  show Update = "updating"

data UpdateStatus
   = UpdateStatus {
       installed :: [P.Plugin],
       updated :: [P.Plugin],
       nosync :: [P.Plugin],
       outdated :: [P.Plugin],
       failed :: [P.Plugin]
   }

defaultUpdateStatus :: UpdateStatus
defaultUpdateStatus = UpdateStatus [] [] [] [] []

updatePlugin :: Update -> Maybe [Text] -> S.Setting -> IO ()
updatePlugin update plugins setting = do
  let unknownPlugins = filter (`notElem` map show (S.plugin setting)) (fromMaybe [] plugins)
  unless (null unknownPlugins)
     $ mapM_ (suggestPlugin (S.plugin setting)) unknownPlugins
  createPluginDirectory
  dir <- pluginDirectory
  let specified p = show p `elem` fromMaybe [] plugins || plugins == Just []
  let filterplugin p = isNothing plugins || specified p
  let ps = filter filterplugin (S.plugin setting)
  let count xs = if length xs > 1 then "s (" <> pack (P.show (length xs)) <> ")" else ""
  time <- maximum <$> mapM (lastUpdatePlugin dir) ps
  status <- foldM (\s p -> updateOnePlugin time dir update (specified p) s p) defaultUpdateStatus ps
  putStrLn $ (if null (failed status) then "Success" else "Error occured") <> " in " <> show update <> "."
  unless (null (installed status))
    (putStrLn ("Installed plugin" <> count (installed status) <> ": ")
     >> mapM_ (putStrLn . ("  "<>) . P.name) (reverse (installed status)))
  unless (null (updated status))
    (putStrLn ("Updated plugin" <> count (updated status) <> ": ")
     >> mapM_ (putStrLn . ("  "<>) . P.name) (reverse (updated status)))
  unless (null (failed status))
    (putStrLn ("Failed plugin" <> count (failed status) <> ": ")
     >> mapM_ (putStrLn . ("  "<>) . P.name) (reverse (failed status)))
  generatePluginCode setting
  generateHelpTags setting

cleanAndCreateDirectory :: Text -> IO ()
cleanAndCreateDirectory directory = do
  let dir = directory
  createDirectoryIfMissing dir
  removeDirectoryRecursive dir
  createDirectoryIfMissing dir

generateHelpTags :: S.Setting -> IO ()
generateHelpTags setting = do
  dir <- pluginDirectory
  let docdir = dir <> "miv/doc/"
  cleanAndCreateDirectory docdir
  forM_ (map (\p -> dir <> show p <> "/doc/") (S.plugin setting))
    $ \path ->
        doesDirectoryExist path
          >>= \exists -> when exists $ void
              $ system $ unwords ["cd", "'" <> path <> "'",
                                  "&& cp *", "'" <> docdir <> "'", "2>/dev/null"]
  _ <- system $ "vim -u NONE -i NONE -N -e -s -c 'helptags " <> docdir <> "' -c quit"
  putStrLn "Success in processing helptags."

lastUpdatePlugin :: Text -> P.Plugin -> IO Integer
lastUpdatePlugin dir plugin = do
  let path = dir <> show plugin <> "/.git"
  exists <- doesDirectoryExist path
  status <- gitStatus $ dir <> show plugin
  if exists && status == ExitSuccess then lastUpdate path else return 0

updateOnePlugin :: Integer -> Text -> Update -> Bool -> UpdateStatus -> P.Plugin -> IO UpdateStatus
updateOnePlugin time dir update specified status plugin = do
  let path = dir <> show plugin
      repo = vimScriptRepo (P.name plugin)
      cloneCommand = if P.submodule plugin then cloneSubmodule else clone
      pullCommand = if P.submodule plugin then pullSubmodule else pull
  exists <- doesDirectoryExist path
  gitstatus <- gitStatus path
  if not exists || (gitstatus /= ExitSuccess && not (P.sync plugin))
     then do putStrLn $ "Installing: " <> P.name plugin
             when exists $ removeDirectoryRecursive path
             cloneStatus <- cloneCommand repo path
             created <- doesDirectoryExist path
             if cloneStatus /= ExitSuccess || not created
                then return status { failed = plugin : failed status }
                else return status { installed = plugin : installed status }
     else if update == Install || not (P.sync plugin)
             then return status { nosync = plugin : nosync status }
             else lastUpdate path >>= \lastUpdateTime ->
                  if lastUpdateTime < time - 60 * 60 * 24 * 30 && not specified
                     then do putStrLn $ "Outdated: " <> P.name plugin
                             return status { outdated = plugin : outdated status }
                     else do putStrLn $ "Pulling: " <> P.name plugin
                             pullStatus <- pullCommand path
                             newUpdateTime <- lastUpdate path
                             return $ if pullStatus /= ExitSuccess
                                         then status { failed = plugin : failed status }
                                         else if newUpdateTime <= lastUpdateTime
                                                 then status
                                                 else status { updated = plugin : updated status }

vimScriptRepo :: Text -> Text
vimScriptRepo name | T.any (=='/') name = name
                   | otherwise = "vim-scripts/" <> name

listPlugin :: S.Setting -> IO ()
listPlugin setting = mapM_ putStrLn $ space $ map format $ S.plugin setting
  where format p = [show p, P.name p, gitUrl (vimScriptRepo (P.name p))]
        space xs =
          let max0 = maximum (map (T.length . (!!0)) xs) + 1
              max1 = maximum (map (T.length . (!!1)) xs) + 1
              in map (\(as:bs:cs:_) -> as <> T.replicate (max0 - T.length as) " " <> bs <> T.replicate (max1 - T.length bs) " " <> cs) xs

cleanDirectory :: S.Setting -> IO ()
cleanDirectory setting = do
  createPluginDirectory
  dir <- pluginDirectory
  createDirectoryIfMissing dir
  cnt <- getDirectoryContents dir
  let paths = "." : ".." : "miv" : map show (S.plugin setting)
      delpath' = [ dir <> d | d <- cnt, d `notElem` paths ]
  deldir <- filterM doesDirectoryExist delpath'
  delfile <- filterM doesFileExist delpath'
  let delpath = deldir <> delfile
  if not (null delpath)
     then do putStrLn "Remove:"
             mapM_ (putStrLn . ("  "<>)) delpath
             putStr "Really? [y/N] "
             hFlush stdout
             c <- getChar
             when (c == 'y' || c == 'Y')
                  (mapM_ removeDirectoryRecursive deldir >> mapM_ removeFile delfile)
     else putStrLn "Clean."

saveScript :: (Text, Place, [Text]) -> IO ()
saveScript (dir, place, code) =
  let isftplugin = isFtplugin place
      relname = show place
      name = unpack (dir <> relname)
      isallascii = all (T.all (<='~')) code in
  getZonedTime >>= \time ->
  writeFile name $ unlines $
            [ "\" Filename: " <> relname
            , "\" Last Change: " <> pack (P.show time)
            , "\" Generated by " <> nameversion
            , "" ]
         <> (if isallascii then [] else [ "scriptencoding utf-8", "" ])
         <> (if isftplugin then [ "if exists('b:did_ftplugin')"
                                , "  finish"
                                , "endif"
                                , "" ]
                           else [])
         <> [ "let s:save_cpo = &cpo"
            , "set cpo&vim"
            , "" ]
         <> code
         <> [ ""
            , "let &cpo = s:save_cpo"
            , "unlet s:save_cpo" ]

generatePluginCode :: S.Setting -> IO ()
generatePluginCode setting = do
  dir <- fmap (<>"miv/") pluginDirectory
  createDirectoryIfMissing dir
  cleanAndCreateDirectory (dir <> "plugin/")
  cleanAndCreateDirectory (dir <> "autoload/miv/")
  cleanAndCreateDirectory (dir <> "ftplugin/")
  mapM_ (saveScript . (\(t, s) -> (dir, t, s)))
        (vimScriptToList (gatherScript setting))
  putStrLn "Success in generating Vim scripts of miv."

eachPlugin :: Text -> S.Setting -> IO ()
eachPlugin command setting = do
  createPluginDirectory
  dir <- pluginDirectory
  result <- foldM (eachOnePlugin command dir) (P.defaultPlugin, ExitSuccess) (S.plugin setting)
  when (snd result /= ExitSuccess)
     $ putStrLn "Error:" >> putStrLn ("  " <> P.name (fst result))

eachOnePlugin :: Text -> Text -> (P.Plugin, ExitCode) -> P.Plugin -> IO (P.Plugin, ExitCode)
eachOnePlugin _ _ x@(_, ExitFailure 2) _ = return x
eachOnePlugin command dir (_, _) plugin = do
  let path = dir <> show plugin
  doesDirectoryExist path
    >>= \exists ->
      if not exists
         then return (plugin, ExitSuccess)
         else (,) plugin <$> system (unwords ["cd", "'" <> path <> "'", "&&", command])

eachHelp :: IO ()
eachHelp = mapM_ putStrLn [ "Specify command:", "  miv each [command]" ]

pathPlugin :: [Text] -> S.Setting -> IO ()
pathPlugin plugins setting = do
  let ps = filter (\p -> show p `elem` plugins || null plugins) (S.plugin setting)
  dir <- pluginDirectory
  forM_ ps (\plugin -> putStrLn $ dir <> show plugin)

mainProgram :: [Text] -> IO ()
mainProgram [] = printUsage
mainProgram [arg] | T.take 1 arg == "-" = mainProgram [T.tail arg]
mainProgram ["help"] = printUsage
mainProgram ["version"] = putStrLn nameversion
mainProgram ["install"] = getSettingWithError >>= updatePlugin Install Nothing
mainProgram ["update"] = getSettingWithError >>= updatePlugin Update Nothing
mainProgram ["update!"] = getSettingWithError >>= updatePlugin Update (Just [])
mainProgram ["update", "!"] = getSettingWithError >>= updatePlugin Update (Just [])
mainProgram ["each"] = eachHelp
mainProgram ["list"] = getSettingWithError >>= listPlugin
mainProgram ["command"] = commandHelp
mainProgram ["clean"] = getSettingWithError >>= cleanDirectory
mainProgram ["edit"] = getSettingFile >>= maybe (return ()) (($) void . system . ("vim "<>))
mainProgram ["generate"] = getSettingWithError >>= generatePluginCode
mainProgram ["helptags"] = getSettingWithError >>= generateHelpTags
mainProgram ["path"] = getSettingWithError >>= pathPlugin []
mainProgram [arg] = suggestCommand arg
mainProgram ("install":args) = getSettingWithError >>= updatePlugin Install (Just args)
mainProgram ("update":args) = getSettingWithError >>= updatePlugin Update (Just args)
mainProgram ("each":args) = getSettingWithError >>= eachPlugin (unwords args)
mainProgram ("path":args) = getSettingWithError >>= pathPlugin args
mainProgram (arg:args) | T.take 1 arg == "-" = mainProgram (T.tail arg:args)
mainProgram _ = printUsage

main :: IO ()
main = getArgs >>= mainProgram
