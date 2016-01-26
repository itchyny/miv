{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module VimScript where

import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.Functor ((<$>))
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid (..), (<>))
import Data.Text (Text, unpack, unwords, singleton)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (show, unwords)

import qualified Command as C
import qualified Mapping as M
import Mode
import qualified Plugin as P
import qualified Setting as S
import ShowText

data VimScript = VimScript (HM.HashMap Place [Text])
               deriving (Eq)

data Place = Plugin
           | Autoload Text
           | Ftplugin Text
           deriving (Eq, Generic)

instance Hashable Place where
  hashWithSalt a Plugin       = a `hashWithSalt` (0 :: Int) `hashWithSalt` ("" :: Text)
  hashWithSalt a (Autoload s) = a `hashWithSalt` (1 :: Int) `hashWithSalt` s
  hashWithSalt a (Ftplugin s) = a `hashWithSalt` (2 :: Int) `hashWithSalt` s

instance ShowText Place where
  show Plugin        = "plugin/miv.vim"
  show (Autoload "") = "autoload/miv.vim"
  show (Autoload s)  = "autoload/miv/" <> s <> ".vim"
  show (Ftplugin s)  = "ftplugin/" <> s <> ".vim"

autoloadSubdirName :: Place -> Maybe Text
autoloadSubdirName (Autoload "") = Nothing
autoloadSubdirName (Autoload s) = Just s
autoloadSubdirName _ = Nothing

isFtplugin :: Place -> Bool
isFtplugin (Ftplugin _) = True
isFtplugin _ = False

vimScriptToList :: VimScript -> [(Place, [Text])]
vimScriptToList (VimScript x) = HM.toList x

instance Monoid VimScript where
  mempty = VimScript HM.empty
  mappend (VimScript x) (VimScript y)
    | HM.null x = VimScript y
    | HM.null y = VimScript x
    | otherwise = VimScript (HM.unionWith concat' x y)
    where
      concat' a [] = a
      concat' [] b = b
      concat' a b = a <> [""] <> b

gatherScript :: S.Setting -> VimScript
gatherScript setting = addAutoloadNames
                     $ beforeScript setting
                    <> gatherBeforeAfterScript plugins
                    <> gather "dependon" P.dependon plugins
                    <> gather "dependedby" P.dependedby plugins
                    <> gather "mappings" P.mappings plugins
                    <> gather "mapmodes" P.mapmodes plugins
                    <> gatherFuncUndefined setting
                    <> pluginLoader
                    <> mappingLoader
                    <> commandLoader
                    <> foldl' (<>) mempty (map pluginConfig plugins)
                    <> filetypeLoader setting
                    <> gatherInsertEnter setting
                    <> filetypeScript (S.filetypeScript setting)
                    <> filetypeDetect (S.filetypeDetect setting)
                    <> afterScript setting
  where plugins = S.plugin setting

gatherBeforeAfterScript :: [P.Plugin] -> VimScript
gatherBeforeAfterScript x = insertAuNameMap $ gatherScripts x (mempty, HM.empty)
  where
    insertAuNameMap :: (VimScript, HM.HashMap Text Text) -> VimScript
    insertAuNameMap (vs, hm) = VimScript (HM.singleton (Autoload "") $
          [ "let s:autoload = {" ]
       <> [ "      \\ " <> singleQuote k <> ": " <> singleQuote a <> "," | (k, a) <- HM.toList hm ]
       <> [ "      \\ }" ]) <> vs
    gatherScripts :: [P.Plugin] -> (VimScript, HM.HashMap Text Text) -> (VimScript, HM.HashMap Text Text)
    gatherScripts (p:ps) (vs, hm)
            | null (P.beforeScript p) && null (P.afterScript p) = gatherScripts ps (vs, hm)
            | otherwise = gatherScripts ps (vs <> vs', HM.insert name hchar hm)
      where
        name = T.filter isAlphaNum (T.toLower (P.rtpName p))
        hchar | null (loadScript p) = maybe "_" singleton $ getHeadChar $ P.rtpName p
              | otherwise = "_"
        funcname str = "miv#" <> hchar <> "#" <> str <> "_" <> name
        au = Autoload hchar
        vs' = VimScript $ HM.singleton au $ wrapFunction (funcname "before") (P.beforeScript p)
                                         <> wrapFunction (funcname "after") (P.afterScript p)
    gatherScripts [] (vs, hm) = (vs, hm)

addAutoloadNames :: VimScript -> VimScript
addAutoloadNames h@(VimScript hm)
  = VimScript (HM.singleton (Autoload "")
      [ "let s:autoloads = { " <> T.intercalate ", " (((<>": 1") . singleQuote)
                               <$> mapMaybe autoloadSubdirName (HM.keys hm)) <> " }"])
   <> h

gather :: Text -> (P.Plugin -> [Text]) -> [P.Plugin] -> VimScript
gather name f plg
  = VimScript (HM.singleton (Autoload "") $
      [ "let s:" <> name <> " = {" ]
   <> [ "      \\ " <> singleQuote (P.rtpName p)
                    <> ": [ " <> T.intercalate ", " (map singleQuote (f p))  <> " ],"
                                                               | p <- plg, not (null (f p)) ]
   <> [ "      \\ }" ])

pluginConfig :: P.Plugin -> VimScript
pluginConfig plg
    = VimScript (HM.singleton Plugin $ wrapInfo $
        wrapEnable plg $ mapleader <> gatherCommand plg <> gatherMapping plg <> P.script plg <> loadScript plg)
  where
    wrapInfo [] = []
    wrapInfo str = ("\" " <> P.name plg) : str
    mapleader = (\s -> if T.null s then [] else ["let g:mapleader = " <> singleQuote s]) (P.mapleader plg)

loadScript :: P.Plugin -> [Text]
loadScript plg
  | all null [ P.commands plg, P.mappings plg, P.functions plg, P.filetypes plg
             , P.loadafter plg, P.loadbefore plg ] && not (P.insert plg)
  = ["call miv#load(" <> singleQuote (P.rtpName plg) <> ")"]
  | otherwise = []

gatherCommand :: P.Plugin -> [Text]
gatherCommand plg
  | not (null (P.commands plg))
    = [show (C.defaultCommand { C.cmdName = c
        , C.cmdRepText = unwords ["call miv#command(" <> singleQuote (P.rtpName plg) <> ","
                               , singleQuote c <> ","
                               , singleQuote "<bang>" <> ","
                               , "<q-args>,"
                               , "expand('<line1>'),"
                               , "expand('<line2>'))" ] }) | c <- P.commands plg]
  | otherwise = []

gatherMapping :: P.Plugin -> [Text]
gatherMapping plg
  | not (null (P.mappings plg))
    = let genMapping
            = [\mode ->
               M.defaultMapping
                  { M.mapName    = c
                  , M.mapRepText = escape mode <> ":<C-u>call miv#mapping("
                        <> singleQuote (P.rtpName plg) <> ", "
                        <> singleQuote c <> ", "
                        <> singleQuote (show mode) <> ")<CR>"
                  , M.mapMode    = mode } | c <- P.mappings plg]
          escape m = if m `elem` [ InsertMode, OperatorPendingMode ] then "<ESC>" else ""
          modes = if null (P.mapmodes plg) then [NormalMode, VisualMode] else map (read . unpack) (P.mapmodes plg)
          in concat [map (show . f) modes | f <- genMapping]
  | otherwise = []

beforeScript :: S.Setting -> VimScript
beforeScript setting = VimScript (HM.singleton Plugin (S.beforeScript setting))

afterScript :: S.Setting -> VimScript
afterScript setting = VimScript (HM.singleton Plugin (S.afterScript setting))

filetypeLoader :: S.Setting -> VimScript
filetypeLoader setting
  = HM.foldrWithKey f mempty (filetypeLoadPlugins (S.plugin setting) HM.empty)
  where
    f ft plg val =
      case getHeadChar ft of
           Nothing -> val
           Just c ->
             let funcname = "miv#" <> singleton c <> "#load_" <> T.filter isAlphaNum (T.toLower ft)
                 in val
                  <> VimScript (HM.singleton (Autoload (singleton c))
                       (("function! " <> funcname <> "() abort")
                       : "  setl ft="
                       :  concat [wrapEnable b
                       [ "  call miv#load(" <> singleQuote (P.rtpName b) <> ")"] | b <- plg]
                    <> [ "  autocmd! MivFileTypeLoad" <> ft
                       , "  augroup! MivFileTypeLoad" <> ft
                       , "  setl ft=" <> ft
                       , "  silent! doautocmd FileType " <> ft
                       , "endfunction"
                       ]))
                  <> VimScript (HM.singleton Plugin
                       [ "augroup MivFileTypeLoad" <> ft
                       , "  autocmd!"
                       , "  autocmd FileType " <> ft <> " call " <> funcname <> "()"
                       , "augroup END"
                       ])

filetypeLoadPlugins :: [P.Plugin] -> HM.HashMap Text [P.Plugin] -> HM.HashMap Text [P.Plugin]
filetypeLoadPlugins (b:plugins) fts
  | not (null (P.filetypes b))
  = filetypeLoadPlugins plugins (foldr (flip (HM.insertWith (<>)) [b]) fts (P.filetypes b))
  | otherwise = filetypeLoadPlugins plugins fts
filetypeLoadPlugins [] fts = fts

gatherInsertEnter :: S.Setting -> VimScript
gatherInsertEnter setting
  = VimScript (HM.singleton Plugin (f [ p | p <- S.plugin setting, P.insert p ]))
  where f [] = []
        f plgs = "\" InsertEnter"
               : "function! s:insertEnter() abort"
             : [ "  call miv#load(" <> singleQuote (P.rtpName p) <> ")" | p <- plgs ]
            <> [ "  autocmd! MivInsertEnter"
               , "  augroup! MivInsertEnter"
               , "  silent! doautocmd InsertEnter"
               , "endfunction"
               , ""
               , "augroup MivInsertEnter"
               , "  autocmd!"
               , "  autocmd InsertEnter * call s:insertEnter()"
               , "augroup END"
               , "" ]

gatherFuncUndefined :: S.Setting -> VimScript
gatherFuncUndefined setting
  = VimScript (HM.singleton Plugin (f [ p | p <- S.plugin setting, not (null (P.functions p))]))
  where f [] = []
        f plgs = "\" FuncUndefined"
               : "function! s:funcUndefined() abort"
               : "  let f = expand('<amatch>')"
               : concat [
               [ "  if f =~# " <> singleQuote q
               , "    call miv#load(" <> singleQuote (P.rtpName p) <> ")"
               , "  endif" ] | (p, q) <- concatMap (\q -> map ((,) q) (P.functions q)) plgs]
            <> [ "endfunction"
               , ""
               , "augroup MivFuncUndefined"
               , "  autocmd!"
               , "  autocmd FuncUndefined * call s:funcUndefined()"
               , "augroup END"
               , "" ]

wrapEnable :: P.Plugin -> [Text] -> [Text]
wrapEnable plg str
  | null str = []
  | T.null (P.enable plg) = str
  | P.enable plg == "0" = []
  | otherwise = (indent <> "if " <> P.enable plg)
                           : map ("  "<>) str
             <> [indent <> "endif"]
  where indent = T.takeWhile (==' ') (head str)

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

filetypeScript :: HM.HashMap Text [Text] -> VimScript
filetypeScript =
  HM.foldrWithKey (\ft scr val -> val <> VimScript (HM.singleton (Ftplugin ft) scr)) mempty

filetypeDetect :: HM.HashMap Text Text -> VimScript
filetypeDetect =
  mkVimScript . HM.foldrWithKey (\exts ft val -> val <> [f exts ft]) []
    where f exts ft = "  autocmd BufNewFile,BufReadPost " <> T.intercalate "," (map ("*."<>) (T.split (==',') exts)) <> " setlocal filetype=" <> ft
          mkVimScript [] = mempty
          mkVimScript xs = VimScript (HM.singleton Plugin (p <> xs <> a))
          p = ["\" Filetype detection", "augroup MivFileTypeDetect", "  autocmd!"]
          a = ["augroup END"]

wrapFunction :: Text -> [Text] -> [Text]
wrapFunction funcname script =
     ["function! " <> funcname <> "() abort"]
  <> map ("  "<>) script
  <> ["endfunction"]

getHeadChar :: Text -> Maybe Char
getHeadChar xs
  | T.null xs = Nothing
  | otherwise = let x = T.head xs in if isAlpha x then Just (toLower x) else getHeadChar (T.tail xs)

mappingLoader :: VimScript
mappingLoader = VimScript (HM.singleton (Autoload "")
  [ "function! miv#mapping(name, mapping, mode) abort"
  , "  call miv#load(a:name)"
  , "  if a:mode ==# 'v' || a:mode ==# 'x'"
  , "    call feedkeys('gv', 'n')"
  , "  elseif a:mode ==# 'o'"
  , "    call feedkeys(\"\\<Esc>\", 'n')"
  , "    call feedkeys(v:operator, 'm')"
  , "  endif"
  , "  call feedkeys((v:count ? v:count : '') . substitute(a:mapping, '<Plug>', \"\\<Plug>\", 'g'), 'm')"
  , "  return ''"
  , "endfunction"
  ])

commandLoader :: VimScript
commandLoader = VimScript (HM.singleton (Autoload "")
  [ "function! miv#command(name, command, bang, args, line1, line2) abort"
  , "  silent! execute 'delcommand' a:command"
  , "  call miv#load(a:name)"
  , "  let range = a:line1 != a:line2 ? a:line1.','.a:line2 : ''"
  , "  try"
  , "    exec range.a:command.a:bang a:args"
  , "  catch /^Vim\\%((\\a\\+)\\)\\=:E481/"
  , "    exec a:command.a:bang a:args"
  , "  endtry"
  , "endfunction"
  ])

pluginLoader :: VimScript
pluginLoader = VimScript (HM.singleton (Autoload "")
  [ "let s:loaded = {}"
  , "let s:path = expand('<sfile>:p:h:h')"
  , "let s:mivpath = expand('<sfile>:p:h:h:h') . '/'"
  , "function! miv#load(name) abort"
  , "  if has_key(s:loaded, a:name)"
  , "    return"
  , "  endif"
  , "  let s:loaded[a:name] = 1"
  , "  for n in get(s:dependon, a:name, [])"
  , "    call miv#load(n)"
  , "  endfor"
  , "  for mode in get(s:mapmodes, a:name, [ 'n', 'v' ])"
  , "    for mapping in get(s:mappings, a:name, [])"
  , "      silent! execute mode . 'unmap' mapping"
  , "    endfor"
  , "  endfor"
  , "  let name = substitute(tolower(a:name), '[^a-z0-9]', '', 'g')"
  , "  let au = has_key(s:autoload, name) && get(s:autoloads, s:autoload[name])"
  , "  if au"
  , "    call miv#{s:autoload[name]}#before_{name}()"
  , "  endif"
  , "  let newrtp = s:mivpath . a:name . '/'"
  , "  if !isdirectory(newrtp)"
  , "    return"
  , "  endif"
  , "  let rtps = split(&rtp, ',')"
  , "  let i = index(map(deepcopy(rtps), 'resolve(v:val)'), s:path)"
  , "  if i < 0"
  , "    let mivpath = get(filter(deepcopy(rtps), 'v:val =~# \"miv.\\\\?$\"'), 0, '')"
  , "    let i = max([0, index(deepcopy(rtps), mivpath)])"
  , "  endif"
  , "  let &rtp = join(insert(rtps, newrtp, i), ',')"
  , "  if isdirectory(newrtp . 'after/')"
  , "    exec 'set rtp+=' . newrtp . 'after/'"
  , "  endif"
  , "  for dir in filter(['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'], 'isdirectory(newrtp . v:val)')"
  , "    for file in split(glob(newrtp . dir . '/**/*.vim'), '\\n')"
  , "      silent! source `=file`"
  , "    endfor"
  , "  endfor"
  , "  if au"
  , "    call miv#{s:autoload[name]}#after_{name}()"
  , "  endif"
  , "  for n in get(s:dependedby, a:name, [])"
  , "    call miv#load(n)"
  , "  endfor"
  , "endfunction"
  ])
