{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Monad (filterM, forM_, liftM, unless, void, when)
import qualified Control.Monad.Parallel as P
import Data.List ((\\), foldl', isPrefixOf, nub, transpose, unfoldr)
import Data.Maybe (listToMaybe, fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Text (Text, unlines, pack, unpack)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, putStr, writeFile)
import Data.Time (getZonedTime)
import Data.Version (showVersion)
import Data.Yaml (decodeFile)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Prelude hiding (readFile, writeFile, unlines, putStrLn, putStr, show)
import System.Directory
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO (hFlush, stdout)
import System.Process (system)

import Git
import Paths_miv (version)
import Plugin
import Setting
import ShowText
import VimScript

nameversion :: Text
nameversion = "miv " <> pack (showVersion version)

expandHomeDirectory :: FilePath -> IO FilePath
expandHomeDirectory ('~':path) = (<>path) <$> getHomeDirectory
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

getSetting :: IO (Maybe Setting)
getSetting = liftM (fromMaybe "") getSettingFile >>= expandHomeDirectory >>= decodeFile

getSettingWithError :: IO Setting
getSettingWithError =
  getSettingFile >>= \file ->
    case file of
         Nothing -> error "No setting file: ~/.vimrc.yaml"
         Just _ ->
           getSetting >>= \set ->
             case set of
                  Nothing -> error "Parse error"
                  Just setting -> return setting

pluginDirectory :: IO FilePath
pluginDirectory = do
  dir <- expandHomeDirectory "~/.vim/miv/"
  windir <- expandHomeDirectory "~/vimfiles/miv/"
  exists <- doesDirectoryExist dir
  return (if exists then dir else (if os `elem` ["windows", "mingw"] then windir else dir))

createPluginDirectory :: IO ()
createPluginDirectory =
  createDirectoryIfMissing True =<< pluginDirectory

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
          , ("ftdetect", "Gather ftdetect scripts.")
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
        , "You can install the plugins by the following command:"
        , "  miv install"
        , ""
        , "You can update the plugins by the following command:"
        , "  miv update"
        , ""
        , "You can specify the name of plugins:"
        , "  miv update plugin1 plugin2"
        , ""
        , "Normally, outdated plugins are ignored but"
        , "you can update all the plugins with trailing !:"
        , "  miv update!"
        , ""
        , "You can use `miv each' to execute some commands at each plugin directory."
        , "  miv each pwd"
        , "  miv each git gc"
        , "  miv each git diff"
        , "  miv each 'echo $(du -sh .) $(basename $(pwd))'"
        , "  miv each 'git diff --quiet || echo ${PWD##*/}'"
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
  let distcommands = [(levenshtein arg (unpack x), Argument (x, y)) | (Argument (x, y)) <- arguments]
      mindist = fst (minimum distcommands)
      mincommands = [y | (x, y) <- distcommands, x == mindist]
      prefixcommands = [y | y@(Argument (x, _)) <- arguments, arg `isPrefixOf` unpack x || unpack x `isPrefixOf` arg]
      containedcommands = [y | y@(Argument (x, _)) <- arguments, length arg > 1 && arg `isContainedIn` unpack x]
  putStrLn $ "Unknown command: " <> pack arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "<>) . show) (if null prefixcommands then nub (containedcommands <> mincommands) else prefixcommands)

isContainedIn :: Eq a => [a] -> [a] -> Bool
isContainedIn xxs@(x:xs) (y:ys) = x == y && xs `isContainedIn` ys || xxs `isContainedIn` ys
isContainedIn [] _ = True
isContainedIn _ [] = False

suggestPlugin :: [Plugin] -> String -> IO ()
suggestPlugin plugin arg = do
  let distplugins = [(levenshtein arg (rtpName p), p) | p <- plugin]
      mindist = fst (minimum distplugins)
      minplugins = [y | (x, y) <- distplugins, x == mindist]
  putStrLn $ "Unknown plugin: " <> pack arg
  putStrLn "Probably:"
  mapM_ (putStrLn . ("  "<>) . show) minplugins

data Update = Install | Update deriving Eq
instance ShowText Update where
  show Install = "installing"
  show Update = "updating"

data UpdateStatus
   = UpdateStatus {
       installed :: [Plugin],
       updated :: [Plugin],
       nosync :: [Plugin],
       outdated :: [Plugin],
       failed :: [Plugin]
   }

instance Monoid UpdateStatus where
  mempty = UpdateStatus [] [] [] [] []
  mappend (UpdateStatus i u n o f) (UpdateStatus i' u' n' o' f')
    = UpdateStatus (i <> i') (u <> u') (n <> n') (o <> o') (f <> f')

updatePlugin :: Update -> Maybe [String] -> Setting -> IO ()
updatePlugin update maybePlugins setting = do
  setNumCapabilities =<< getNumProcessors
  let unknownPlugins = filter (`notElem` map rtpName (plugins setting)) (fromMaybe [] maybePlugins)
  unless (null unknownPlugins) $
    mapM_ (suggestPlugin (plugins setting)) unknownPlugins
  createPluginDirectory
  dir <- pluginDirectory
  let specified p = rtpName p `elem` fromMaybe [] maybePlugins || maybePlugins == Just []
  let filterplugin p = isNothing maybePlugins || specified p
  let ps = filter filterplugin (plugins setting)
  let count xs = if length xs > 1 then "s (" <> show (length xs) <> ")" else ""
  time <- maximum <$> mapM' (lastUpdatePlugin dir) ps
  status <- mconcat <$> mapM' (\p -> updateOnePlugin time dir update (specified p) p) ps
  putStrLn $ (if null (failed status) then "Success" else "Error occured") <> " in " <> show update <> "."
  unless (null (installed status)) $ do
    putStrLn $ "Installed plugin" <> count (installed status) <> ": "
    mapM_ (putStrLn . ("  "<>) . name) (reverse (installed status))
  unless (null (updated status)) $ do
    putStrLn $ "Updated plugin" <> count (updated status) <> ": "
    mapM_ (putStrLn . ("  "<>) . name) (reverse (updated status))
  unless (null (failed status)) $ do
    putStrLn $ "Failed plugin" <> count (failed status) <> ": "
    mapM_ (putStrLn . ("  "<>) . name) (reverse (failed status))
  generatePluginCode setting
  gatherFtdetectScript setting
  generateHelpTags setting

mapM' :: P.MonadParallel m => (a -> m b) -> [a] -> m [b]
mapM' f = fmap mconcat . P.mapM (mapM f) . transpose . takeWhile (not . null) . unfoldr (Just . splitAt 8)

cleanAndCreateDirectory :: FilePath -> IO ()
cleanAndCreateDirectory dir = do
  createDirectoryIfMissing True dir
  removeDirectoryRecursive dir
  createDirectoryIfMissing True dir

generateHelpTags :: Setting -> IO ()
generateHelpTags setting = do
  dir <- pluginDirectory
  let docdir = dir <> "miv/doc/"
  cleanAndCreateDirectory docdir
  P.forM_ (map (\p -> dir <> rtpName p <> "/doc/") (plugins setting)) $ \path -> do
    exists <- doesDirectoryExist path
    when exists $
      void $ system $ unwords ["cd", "'" <> path <> "'", "&& cp *", "'" <> docdir <> "'", "2>/dev/null"]
  _ <- system $ "vim -u NONE -i NONE -N -e -s -c 'helptags " <> docdir <> "' -c quit"
  putStrLn "Success in processing helptags."

lastUpdatePlugin :: FilePath -> Plugin -> IO Integer
lastUpdatePlugin dir plugin = do
  let path = dir <> rtpName plugin <> "/.git"
  exists <- doesDirectoryExist path
  if exists then lastUpdate path else return 0

updateOnePlugin :: Integer -> FilePath -> Update -> Bool -> Plugin -> IO UpdateStatus
updateOnePlugin time dir update specified plugin = do
  let path = dir <> rtpName plugin
      repo = vimScriptRepo (name plugin)
      cloneCommand = if submodule plugin then cloneSubmodule else clone
      pullCommand = if submodule plugin then pullSubmodule else pull
  exists <- doesDirectoryExist path
  gitstatus <- gitStatus path
  if not exists || (gitstatus /= ExitSuccess && not (sync plugin))
     then do putStrLn $ "Installing: " <> name plugin
             when exists $ removeDirectoryRecursive path
             cloneStatus <- cloneCommand repo path
             created <- doesDirectoryExist path
             if cloneStatus /= ExitSuccess || not created
                then return mempty { failed = [plugin] }
                else return mempty { installed = [plugin] }
     else if update == Install || not (sync plugin)
             then return mempty { nosync = [plugin] }
             else lastUpdate path >>= \lastUpdateTime ->
                  if lastUpdateTime < time - 60 * 60 * 24 * 30 && not specified
                     then do putStrLn $ "Outdated: " <> name plugin
                             return mempty { outdated = [plugin] }
                     else do putStrLn $ "Pulling: " <> name plugin
                             pullStatus <- pullCommand path
                             newUpdateTime <- lastUpdate path
                             return $ if pullStatus /= ExitSuccess
                                         then mempty { failed = [plugin] }
                                         else if newUpdateTime <= lastUpdateTime
                                                 then mempty
                                                 else mempty { updated = [plugin] }

vimScriptRepo :: Text -> FilePath
vimScriptRepo pluginname | T.any (=='/') pluginname = unpack pluginname
                         | otherwise = unpack $ "vim-scripts/" <> pluginname

listPlugin :: Setting -> IO ()
listPlugin setting = mapM_ putStrLn $ space $ map format $ plugins setting
  where format p = [show p, name p, pack $ gitUrl (vimScriptRepo (name p))]
        space xs =
          let max0 = maximum (map (T.length . (!!0)) xs) + 1
              max1 = maximum (map (T.length . (!!1)) xs) + 1
              in map (\(as:bs:cs:_) -> as <> T.replicate (max0 - T.length as) " " <> bs <> T.replicate (max1 - T.length bs) " " <> cs) xs

cleanDirectory :: Setting -> IO ()
cleanDirectory setting = do
  createPluginDirectory
  dir <- pluginDirectory
  createDirectoryIfMissing True dir
  cnt <- getDirectoryContents dir
  let paths = "." : ".." : "miv" : map (unpack . show) (plugins setting)
      delpath' = [ dir <> d | d <- cnt, d `notElem` paths ]
  deldir <- filterM doesDirectoryExist delpath'
  delfile <- filterM doesFileExist delpath'
  let delpath = deldir <> delfile
  if not (null delpath)
     then do putStrLn "Remove:"
             mapM_ (putStrLn . pack . ("  "<>)) delpath
             putStr "Really? [y/N] "
             hFlush stdout
             c <- getChar
             when (c == 'y' || c == 'Y') $ do
               mapM_ removeDirectoryRecursive deldir
               mapM_ removeFile delfile
     else putStrLn "Clean."

saveScript :: (FilePath, Place, [Text]) -> IO ()
saveScript (dir, place, code) = do
  let relname = show place
      isallascii = all (T.all (<='~')) code
  time <- getZonedTime
  writeFile (dir <> unpack relname) $ unlines $
            [ "\" Filename: " <> relname
            , "\" Last Change: " <> show time
            , "\" Generated by " <> nameversion
            , "" ]
         <> (if isallascii then [] else [ "scriptencoding utf-8", "" ])
         <> [ "let s:save_cpo = &cpo"
            , "set cpo&vim"
            , "" ]
         <> code
         <> [ ""
            , "let &cpo = s:save_cpo"
            , "unlet s:save_cpo" ]

generatePluginCode :: Setting -> IO ()
generatePluginCode setting = do
  dir <- fmap (<>"miv/") pluginDirectory
  createDirectoryIfMissing True dir
  cleanAndCreateDirectory (dir <> "plugin/")
  cleanAndCreateDirectory (dir <> "autoload/miv/")
  cleanAndCreateDirectory (dir <> "ftplugin/")
  P.mapM_ (saveScript . (\(t, s) -> (dir, t, s)))
          (vimScriptToList (gatherScript setting))
  putStrLn "Success in generating Vim scripts of miv."

gatherFtdetectScript :: Setting -> IO ()
gatherFtdetectScript setting = do
  dir <- pluginDirectory
  cleanAndCreateDirectory (dir <> "miv/ftdetect/")
  forM_ (plugins setting) $ \plugin -> do
    let path = rtpName plugin
    exists <- doesDirectoryExist (dir <> path <> "/ftdetect")
    when exists $ do
      files <- (\\ [".", ".."]) <$> getDirectoryContents (dir <> path <> "/ftdetect")
      forM_ files $ \file ->
        copyFile (dir <> path <> "/ftdetect/" <> file) (dir <> "miv/ftdetect/" <> file)
  putStrLn "Success in gathering ftdetect scripts."

data EachStatus = EachStatus { failed' :: [Plugin] }

instance Monoid EachStatus where
  mempty = EachStatus []
  mappend (EachStatus f) (EachStatus f') = EachStatus (f <> f')

eachPlugin :: String -> Setting -> IO ()
eachPlugin command setting = do
  createPluginDirectory
  dir <- pluginDirectory
  status <- mconcat <$> mapM (eachOnePlugin command dir) (plugins setting)
  unless (null (failed' status)) $ do
    putStrLn "Error:"
    mapM_ (putStrLn . ("  "<>) . name) (failed' status)

eachOnePlugin :: String -> FilePath -> Plugin -> IO EachStatus
eachOnePlugin command dir plugin = do
  let path = dir <> rtpName plugin
  exists <- doesDirectoryExist path
  if not exists
     then return mempty
     else do exitCode <- system (unwords ["cd", "'" <> path <> "'", "&&", command])
             case exitCode of
                  ExitSuccess -> return mempty
                  _ -> return $ mempty { failed' = [plugin] }

eachHelp :: IO ()
eachHelp = mapM_ putStrLn [ "Specify command:", "  miv each [command]" ]

pathPlugin :: [String] -> Setting -> IO ()
pathPlugin plugins' setting = do
  let ps = filter (\p -> rtpName p `elem` plugins' || null plugins') (plugins setting)
  dir <- pluginDirectory
  forM_ ps (\plugin -> putStrLn (pack dir <> show plugin))

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram ['-':arg] = mainProgram [arg]
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
mainProgram ["ftdetect"] = getSettingWithError >>= gatherFtdetectScript
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
