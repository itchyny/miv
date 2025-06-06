module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MSem qualified as MSem
import Control.Exception (tryJust)
import Control.Monad (filterM, forM_, guard, unless, void, when)
import Control.Monad.Parallel qualified as P
import Data.ByteString.Lazy qualified as BS
import Data.Functor ((<&>))
import Data.List (foldl', isPrefixOf, nub, sort, (\\))
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text, pack, unlines, unpack)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Data.Text.IO (hGetContents, putStr, putStrLn, writeFile)
import Data.Time (getZonedTime)
import Data.Tuple.Utils (fst3, snd3)
import Data.Version (showVersion)
import Data.YAML qualified as YAML
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Console.Regions
import System.Directory
import System.Environment (getArgs)
import System.Environment.XDG.BaseDir (getUserConfigFile, getUserDataDir)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)
import System.IO (IOMode(..), hClose, hFlush, hGetLine, hPutStrLn, openFile,
                  stderr, stdout)
import System.IO.Error (isDoesNotExistError, isEOFError, tryIOError)
import System.PosixCompat.Files (setFileTimes)
import System.Process
import Prelude hiding (putStr, putStrLn, readFile, unlines, writeFile)

import Git
import Paths_miv (version)
import Plugin
import Setting
import VimScript hiding (singleQuote)

nameversion :: Text
nameversion = "miv " <> pack (showVersion version)

expandHomeDirectory :: FilePath -> IO FilePath
expandHomeDirectory ('~':'/':path) = getHomeDirectory <&> (</> path)
expandHomeDirectory path           = return path

getSettingFileCandidates :: IO [FilePath]
getSettingFileCandidates = do
  xdgConfig <- getUserConfigFile "miv" "config.yaml"
  mapM expandHomeDirectory
    [ xdgConfig
    , "~/.vimrc.yaml"
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

getFirstExistingFile :: [FilePath] -> IO (Maybe FilePath)
getFirstExistingFile = fmap listToMaybe . filterM doesFileExist

getSettingFile :: IO (Maybe FilePath)
getSettingFile = getFirstExistingFile =<< getSettingFileCandidates

getSetting :: IO Setting
getSetting = do
  candidates <- getSettingFileCandidates
  getFirstExistingFile candidates >>= \case
    Just file -> do
      cnt <- BS.readFile file
      case YAML.decode1 cnt of
           Right setting   -> return setting
           Left (pos, err) -> error $ YAML.prettyPosWithSource pos cnt err
    Nothing -> error $ unpack $ unlines $
      "No setting file! Tried following locations:" :
      map (\x -> "  - " <> pack x) candidates

pluginDirectory :: IO FilePath
pluginDirectory = do
  candidates <- sequence [
    getUserDataDir "miv",
    expandHomeDirectory "~/.vim/miv",
    expandHomeDirectory "~/vimfiles/miv" ]
  filterM doesDirectoryExist candidates >>= \case
    (path:_) -> return path
    [] -> expandHomeDirectory "~/.vim/miv"

createPluginDirectory :: IO ()
createPluginDirectory =
  createDirectoryIfMissing True =<< pluginDirectory

printUsage :: IO ()
printUsage = mapM_ putStrLn usage

commandHelp :: IO ()
commandHelp = mapM_ (putStrLn . display) arguments

newtype Argument = Argument (Text, Text)
                 deriving (Eq, Ord)

instance Display Argument where
  displayBuilder (Argument (x, y)) = Builder.fromText $
    x <> T.replicate (10 - T.length x) " " <> y

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
     <> map (T.append "  " . display) arguments
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
    f [] _         = []
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
  mapM_ (putStrLn . ("  "<>) . display) (if null prefixcommands then nub (containedcommands <> mincommands) else prefixcommands)

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
  mapM_ (putStrLn . ("  "<>) . display) minplugins

data Update = Install | Update deriving Eq

instance Display Update where
  displayBuilder = Builder.fromText . \case
    Install -> "install"
    Update  -> "update"

data UpdateStatus =
  UpdateStatus {
    installed :: [Plugin],
    updated   :: [Plugin],
    nosync    :: [Plugin],
    outdated  :: [Plugin],
    failed    :: [Plugin]
  }

instance Semigroup UpdateStatus where
  UpdateStatus i u n o f <> UpdateStatus i' u' n' o' f' =
    UpdateStatus (i <> i') (u <> u') (n <> n') (o <> o') (f <> f')

instance Monoid UpdateStatus where
  mempty = UpdateStatus [] [] [] [] []

updatePlugin :: Update -> Maybe [String] -> Setting -> IO ()
updatePlugin update maybePlugins setting = do
  setNumCapabilities =<< getNumProcessors
  let unknownPlugins = filter (`notElem` map rtpName setting.plugins) (fromMaybe [] maybePlugins)
  unless (null unknownPlugins) $
    mapM_ (suggestPlugin setting.plugins) unknownPlugins
  createPluginDirectory
  dir <- pluginDirectory
  let specified p = rtpName p `elem` fromMaybe [] maybePlugins || maybePlugins == Just []
  let filterplugin p = isNothing maybePlugins || specified p
  let ps = filter filterplugin setting.plugins
  let count xs = if length xs > 1 then "s (" <> display (length xs) <> ")" else ""
  time <- maximum <$> mapM' (lastUpdatePlugin dir) ps
  status <- fmap mconcat <$> displayConsoleRegions $
    mapM' (\p -> updateOnePlugin time dir update (specified p) p) ps
  putStrLn $ (if null status.failed then "Success" else "Error occurred") <> " in " <> display update <> "."
  unless (null status.installed) do
    putStrLn $ "Installed plugin" <> count status.installed <> ": "
    mapM_ (\p -> putStrLn ("  " <> p.name)) (sort status.installed)
  unless (null status.updated) do
    putStrLn $ "Updated plugin" <> count status.updated <> ": "
    mapM_ (\p -> putStrLn ("  " <> p.name)) (sort status.updated)
  unless (null status.failed) do
    putStrLn $ "Failed plugin" <> count status.failed <> ": "
    mapM_ (\p -> putStrLn ("  " <> p.name)) (sort status.failed)
  generatePluginCode setting
  gatherFtdetectScript setting
  generateHelpTags setting

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f xs = MSem.new (16 :: Int) >>= \sem -> mapConcurrently (MSem.with sem . f) xs

cleanAndCreateDirectory :: FilePath -> IO ()
cleanAndCreateDirectory dir = do
  removeDirectoryIfExists dir
  createDirectoryIfMissing True dir

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists dir = do
  exists <- doesDirectoryExist dir
  when exists $ removeDirectoryRecursive dir

generateHelpTags :: Setting -> IO ()
generateHelpTags setting = do
  dir <- pluginDirectory
  let docdir = dir </> "miv" </> "doc"
  cleanAndCreateDirectory docdir
  P.forM_ (map (\p -> dir </> rtpName p </> "doc") setting.plugins) \path -> do
    exists <- doesDirectoryExist path
    when exists $
      void do (_, _, _, ph) <- createProcess (proc "sh" ["-c", "cp -R * " <> singleQuote docdir]) {
                cwd = Just path
              }
              waitForProcess ph
  _ <- system $ "vim -u NONE -i NONE -N -e -s -c 'helptags " <> docdir <> "' -c quit"
  putStrLn "Success in processing helptags."

lastUpdatePlugin :: FilePath -> Plugin -> IO Integer
lastUpdatePlugin dir plugin = do
  let path = dir </> rtpName plugin </> ".git"
  exists <- doesDirectoryExist path
  if exists then lastUpdate path else return 0

updateOnePlugin :: Integer -> FilePath -> Update -> Bool -> Plugin -> IO UpdateStatus
updateOnePlugin time dir update specified plugin = do
  let path = dir </> rtpName plugin
      repo = vimScriptRepo plugin.name
      cloneCommand = if plugin.submodule then cloneSubmodule else clone
      pullCommand = if plugin.submodule then pullSubmodule else pull
      putStrLn' region = setConsoleRegion region . (plugin.name<>) . (": "<>)
      finish' region = finishConsoleRegion region . (plugin.name<>) . (": "<>)
  exists <- doesDirectoryExist path
  gitstatus <- gitStatus path
  if not exists || (gitstatus /= ExitSuccess && not plugin.sync)
     then withConsoleRegion Linear \region -> do
       putStrLn' region "Installing"
       when exists $ removeDirectoryRecursive path
       cloneStatus <- execCommand (unpack plugin.name) region dir $ cloneCommand repo path
       created <- doesDirectoryExist path
       when created do
         buildPlugin path region
         changeModifiedTime path =<< lastUpdate path
       if cloneStatus /= ExitSuccess || not created
          then return mempty { failed = [plugin] }
          else return mempty { installed = [plugin] }
     else if update == Install || not plugin.sync
             then return mempty { nosync = [plugin] }
             else withConsoleRegion Linear \region -> do
               lastUpdateTime <- lastUpdate path
               if lastUpdateTime < time - 60 * 60 * 24 * 30 && not specified
                  then do
                    finish' region "Outdated"
                    return mempty { outdated = [plugin] }
                  else do
                    putStrLn' region "Pulling"
                    pullStatus <- execCommand (unpack plugin.name) region dir $ pullCommand path
                    newUpdateTime <- lastUpdate path
                    if pullStatus /= ExitSuccess
                       then return mempty { failed = [plugin] }
                       else if newUpdateTime <= lastUpdateTime
                               then return mempty
                               else do
                                 buildPlugin path region
                                 changeModifiedTime path newUpdateTime
                                 return mempty { updated = [plugin] }
    where changeModifiedTime path mtime =
            when (mtime > 0) let ctime = fromInteger mtime
                                 in setFileTimes path ctime ctime
          buildPlugin path region =
            unless (T.null plugin.build) $
              void $ execCommand (unpack plugin.name) region path (unpack plugin.build)

singleQuote :: String -> String
singleQuote str = "'" <> str <> "'"

execCommand :: String -> ConsoleRegion -> String -> String -> IO ExitCode
execCommand pluginName region dir command = do
  (_, Just hout, Just herr, ph) <- createProcess (proc "sh" ["-c", command]) {
    cwd     = Just dir,
    std_out = CreatePipe,
    std_err = CreatePipe
  }
  outmvar <- newEmptyMVar
  errmvar <- newEmptyMVar
  _ <- forkIO $ go herr errmvar ""
  _ <- forkIO $ go hout outmvar ""
  errline <- takeMVar errmvar
  outline <- takeMVar outmvar
  code <- waitForProcess ph
  finishConsoleRegion region (pluginName ++ ": " ++ if null outline then errline else outline)
  return code
  where go h mvar lastLine =
          tryIOError (hGetLine h) >>= \case
            Left err -> when (isEOFError err) $ putMVar mvar lastLine
            Right line -> do
              setConsoleRegion region (pluginName ++ ": " ++ line)
              threadDelay 100000
              go h mvar line

vimScriptRepo :: Text -> FilePath
vimScriptRepo pluginname | T.any (=='/') pluginname = unpack pluginname
                         | otherwise = unpack $ "vim-scripts/" <> pluginname

listPlugin :: Setting -> IO ()
listPlugin setting = mapM_ putStrLn $ space $ map format $ setting.plugins
  where format (p :: Plugin) = (display p, p.name, pack $ gitUrl (vimScriptRepo p.name))
        space xs =
          let max0 = maximum (map (T.length . fst3) xs) + 1
              max1 = maximum (map (T.length . snd3) xs) + 1
              in map (\(a, b, c) -> a <> T.replicate (max0 - T.length a) " " <> b <> T.replicate (max1 - T.length b) " " <> c) xs

cleanDirectory :: Setting -> IO ()
cleanDirectory setting = do
  createPluginDirectory
  dir <- pluginDirectory
  createDirectoryIfMissing True dir
  cnt <- listDirectory dir
  let paths = "miv" : map (unpack . display) setting.plugins
      delpath' = [ dir </> d | d <- cnt, d `notElem` paths ]
  deldirs <- filterM doesDirectoryExist delpath'
  files <- getDirectoryFiles (dir </> "miv") $ unpack . display . ($ "*") <$> [ Autoload, Ftplugin, Syntax ]
  let delfiles = (delpath' \\ deldirs)
        <> (((dir </> "miv") </>) <$> (files \\ [ unpack (display place) | (place, _) <- vimScriptToList (gatherScript setting) ]))
  let delpaths = deldirs <> delfiles
  if not (null delpaths)
     then do putStrLn "Remove:"
             mapM_ (putStrLn . pack . ("  "<>)) delpaths
             putStr "Really? [y/N] "
             hFlush stdout
             c <- getChar
             when (c == 'y' || c == 'Y') do
               mapM_ removeDirectoryRecursive deldirs
               mapM_ removeFile delfiles
     else putStrLn "Clean."

saveScript :: (FilePath, Place, [Text]) -> IO ()
saveScript (dir, place, code) = do
  let relname = display place
      path = dir </> unpack relname
      isAllAscii = all (T.all (<='~')) code
      body = "" : (if isAllAscii then [] else [ "scriptencoding utf-8", "" ])
         <> [ "let s:save_cpo = &cpo"
            , "set cpo&vim"
            , "" ]
         <> code
         <> [ ""
            , "let &cpo = s:save_cpo"
            , "unlet s:save_cpo" ]
  time <- getZonedTime
  contents <- fileContents path
  when (contents /= Just body) $
    writeFile path $ unlines $
              [ "\" Filename: " <> relname
              , "\" Last Change: " <> pack (show time)
              , "\" Generated by " <> nameversion ] ++ body

fileContents :: String -> IO (Maybe [Text])
fileContents path =
  tryJust (guard . isDoesNotExistError) (openFile path ReadMode) >>= \case
    Right file -> do
      contentLines <- T.lines <$> hGetContents file
      hClose file
      return $ Just $ dropWhile ("\" " `T.isPrefixOf`) contentLines
    Left _ -> return Nothing

generatePluginCode :: Setting -> IO ()
generatePluginCode setting = do
  dir <- fmap (</>"miv") pluginDirectory
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True (dir </> "plugin")
  createDirectoryIfMissing True (dir </> "autoload" </> "miv")
  createDirectoryIfMissing True (dir </> "ftplugin")
  createDirectoryIfMissing True (dir </> "syntax")
  unless (all (null . (.dependedby)) setting.plugins) $
    hPutStrLn stderr "`dependedby` is deprecated in favor of `loadafter`"
  P.mapM_ (saveScript . (\(t, s) -> (dir, t, s)))
          (vimScriptToList (gatherScript setting))
  putStrLn "Success in generating Vim scripts of miv."

gatherFtdetectScript :: Setting -> IO ()
gatherFtdetectScript setting = do
  dir <- pluginDirectory
  cleanAndCreateDirectory (dir </> "miv" </> "ftdetect")
  forM_ setting.plugins \plugin -> do
    let path = rtpName plugin
    exists <- doesDirectoryExist (dir </> path </> "ftdetect")
    when exists do
      files <- listDirectory (dir </> path </> "ftdetect")
      forM_ files \file ->
        copyFile (dir </> path </> "ftdetect" </> file) (dir </> "miv" </> "ftdetect" </> file)
  putStrLn "Success in gathering ftdetect scripts."

newtype EachStatus = EachStatus { failed' :: [Plugin] }

instance Semigroup EachStatus where
  EachStatus f <> EachStatus f' = EachStatus (f <> f')

instance Monoid EachStatus where
  mempty = EachStatus []

eachPlugin :: String -> Setting -> IO ()
eachPlugin command setting = do
  createPluginDirectory
  dir <- pluginDirectory
  status <- mconcat <$> mapM (eachOnePlugin command dir) setting.plugins
  unless (null status.failed') do
    putStrLn "Error:"
    mapM_ (\p -> putStrLn ("  " <> p.name)) status.failed'

eachOnePlugin :: String -> FilePath -> Plugin -> IO EachStatus
eachOnePlugin command dir plugin = do
  let path = dir </> rtpName plugin
  exists <- doesDirectoryExist path
  if not exists
     then return mempty
     else do (_, _, _, ph) <- createProcess (proc "sh" ["-c", command]) {
               cwd = Just path
             }
             waitForProcess ph >>= \case
               ExitSuccess -> return mempty
               _ -> return mempty { failed' = [plugin] }

eachHelp :: IO ()
eachHelp = mapM_ putStrLn [ "Specify command:", "  miv each [command]" ]

pathPlugin :: [String] -> Setting -> IO ()
pathPlugin plugins' setting = do
  let ps = filter (\p -> rtpName p `elem` plugins' || null plugins') setting.plugins
  dir <- pluginDirectory
  forM_ ps (\plugin -> putStrLn $ pack (dir </> unpack (display plugin)))

mainProgram :: [String] -> IO ()
mainProgram [] = printUsage
mainProgram ['-':arg] = mainProgram [arg]
mainProgram ["help"] = printUsage
mainProgram ["version"] = putStrLn nameversion
mainProgram ["install"] = getSetting >>= updatePlugin Install Nothing
mainProgram ["update"] = getSetting >>= updatePlugin Update Nothing
mainProgram ["update!"] = getSetting >>= updatePlugin Update (Just [])
mainProgram ["update", "!"] = getSetting >>= updatePlugin Update (Just [])
mainProgram ["each"] = eachHelp
mainProgram ["list"] = getSetting >>= listPlugin
mainProgram ["command"] = commandHelp
mainProgram ["clean"] = getSetting >>= cleanDirectory
mainProgram ["edit"] = getSettingFile >>= maybe (return ()) (($) void . system . ("vim "<>))
mainProgram ["generate"] = getSetting >>= generatePluginCode
mainProgram ["ftdetect"] = getSetting >>= gatherFtdetectScript
mainProgram ["helptags"] = getSetting >>= generateHelpTags
mainProgram ["path"] = getSetting >>= pathPlugin []
mainProgram [arg] = suggestCommand arg
mainProgram ("install":args) = getSetting >>= updatePlugin Install (Just args)
mainProgram ("update":args) = getSetting >>= updatePlugin Update (Just args)
mainProgram ("each":args) = getSetting >>= eachPlugin (unwords args)
mainProgram ("path":args) = getSetting >>= pathPlugin args
mainProgram (('-':arg):args) = mainProgram (arg:args)
mainProgram _ = printUsage

main :: IO ()
main = getArgs >>= mainProgram
