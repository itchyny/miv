{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module VimScript where

import Data.Char (isAlpha, isAlphaNum, ord, toLower)
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.List (foldl', groupBy, sort, sortBy, nub)
import Data.Text (Text, singleton, unpack, unwords)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude hiding (show, unwords, read)

import Cmdline
import qualified Command as C
import qualified Mapping as M
import Mode
import qualified Plugin as P
import ReadText
import qualified Setting as S
import ShowText

data VimScript = VimScript (HM.HashMap Place [Text])
               deriving (Eq)

data Place = Plugin
           | Autoload Text
           | Ftplugin Text
           | Syntax   Text
           deriving (Eq, Generic)

instance Hashable Place where
  hashWithSalt a Plugin       = a `hashWithSalt` (0 :: Int) `hashWithSalt` ("" :: Text)
  hashWithSalt a (Autoload s) = a `hashWithSalt` (1 :: Int) `hashWithSalt` s
  hashWithSalt a (Ftplugin s) = a `hashWithSalt` (2 :: Int) `hashWithSalt` s
  hashWithSalt a (Syntax s)   = a `hashWithSalt` (3 :: Int) `hashWithSalt` s

instance ShowText Place where
  show Plugin        = "plugin/miv.vim"
  show (Autoload "") = "autoload/miv.vim"
  show (Autoload s)  = "autoload/miv/" <> s <> ".vim"
  show (Ftplugin s)  = "ftplugin/" <> s <> ".vim"
  show (Syntax s)    = "syntax/" <> s <> ".vim"

vimScriptToList :: VimScript -> [(Place, [Text])]
vimScriptToList (VimScript x) = HM.toList x

instance Semigroup VimScript where
  VimScript x <> VimScript y
    | HM.null x = VimScript y
    | HM.null y = VimScript x
    | otherwise = VimScript (HM.unionWith concat' x y)
    where
      concat' a [] = a
      concat' [] b = b
      concat' a b = a <> [""] <> b

instance Monoid VimScript where
  mempty = VimScript HM.empty

gatherScript :: S.Setting -> VimScript
gatherScript setting = beforeScript setting
                    <> gatherBeforeAfterScript plugins
                    <> gather' "dependon" P.dependon P.loadbefore plugins
                    <> gather' "dependedby" P.dependedby P.loadafter plugins
                    <> gather "mappings" P.mappings plugins
                    <> gather "mapmodes" P.mapmodes plugins
                    <> gather "functions" P.functions plugins
                    <> pluginLoader
                    <> mappingLoader
                    <> commandLoader
                    <> foldl' (<>) mempty (map pluginConfig plugins)
                    <> filetypeLoader setting
                    <> funcUndefinedLoader setting
                    <> cmdlineEnterLoader setting
                    <> insertEnterLoader setting
                    <> filetypeScript setting
                    <> syntaxScript setting
                    <> afterScript setting
  where plugins = S.plugins setting

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
            | null (P.before p) && null (P.after p) = gatherScripts ps (vs, hm)
            | otherwise = gatherScripts ps (vs <> vs', HM.insert name hchar hm)
      where
        name = T.filter isAlphaNum (T.toLower (show p))
        hchar | null (loadScript p) = maybe "_" singleton $ getHeadChar $ show p
              | otherwise = "_"
        funcname str = "miv#" <> hchar <> "#" <> str <> "_" <> name
        vs' = VimScript $ HM.singleton (Autoload hchar) $
          wrapFunction (funcname "before") (P.before p) <>
          wrapFunction (funcname "after") (P.after p)
    gatherScripts [] (vs, hm) = (vs, hm)

gather :: Text -> (P.Plugin -> [Text]) -> [P.Plugin] -> VimScript
gather name f plg
  = VimScript (HM.singleton (Autoload "") $
      [ "let s:" <> name <> " = {" ]
   <> [ "      \\ " <> singleQuote (show p)
                    <> ": [ " <> T.intercalate ", " (map singleQuote (f p))  <> " ],"
        | p <- plg, enabled p, not (null (f p)) ]
   <> [ "      \\ }" ])
  where enabled p = P.enable p /= "0"

gather' :: Text -> (P.Plugin -> [Text]) -> (P.Plugin -> [Text]) -> [P.Plugin] -> VimScript
gather' name f g plg
  = VimScript (HM.singleton (Autoload "") $
      [ "let s:" <> name <> " = {" ]
   <> [ "      \\ " <> singleQuote p
                    <> ": [ " <> T.intercalate ", " (map singleQuote $ sort $ nub q)  <> " ],"
        | (p, q) <- collectSndByFst $ [ (show p, q) | p <- plg, enabled p, q <- f p ]
                                   <> [ (q, show p) | p <- plg, enabled p, q <- g p ] ]
   <> [ "      \\ }" ])
  where enabled p = P.enable p /= "0"

collectSndByFst :: Ord a => [(a,b)] -> [(a,[b])]
collectSndByFst xs = [ (fst (ys !! 0), map snd ys)
                       | ys <- groupBy ((==) `on` fst) $ sortBy (compare `on'` fst) xs ]
  where on' f g x y = g x `f` g y

pluginConfig :: P.Plugin -> VimScript
pluginConfig plg
    = VimScript (HM.singleton Plugin $ wrapInfo $
        wrapEnable (P.enable plg) $ mapleader <> gatherCommand plg <> gatherMapping plg <> P.script plg <> loadScript plg)
  where
    wrapInfo [] = []
    wrapInfo str = ("\" " <> P.name plg) : str
    mapleader = (\s -> if T.null s then [] else ["let g:mapleader = " <> singleQuote s]) (P.mapleader plg)

loadScript :: P.Plugin -> [Text]
loadScript plg
  | all null [ P.commands plg, P.mappings plg, P.functions plg, P.filetypes plg
             , P.loadafter plg, P.loadbefore plg ] && null (P.cmdlines plg) && not (P.insert plg)
  = ["call miv#load(" <> singleQuote (show plg) <> ")"]
  | otherwise = []

gatherCommand :: P.Plugin -> [Text]
gatherCommand plg
  | not (null (P.commands plg))
    = [show (C.defaultCommand { C.cmdName = c
        , C.cmdRepText = unwords ["call miv#command(" <> singleQuote (show plg) <> ","
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
                        <> singleQuote (show plg) <> ", "
                        <> singleQuote c <> ", "
                        <> singleQuote (show mode) <> ")<CR>"
                  , M.mapMode    = mode } | c <- P.mappings plg]
          escape m = if m `elem` [ InsertMode, OperatorPendingMode ] then "<ESC>" else ""
          modes = if null (P.mapmodes plg) then [NormalMode] else map read (P.mapmodes plg)
          in concat [map (show . f) modes | f <- genMapping]
  | otherwise = []

beforeScript :: S.Setting -> VimScript
beforeScript setting = VimScript (HM.singleton Plugin (S.before setting))

afterScript :: S.Setting -> VimScript
afterScript setting = VimScript (HM.singleton Plugin (S.after setting))

filetypeLoader :: S.Setting -> VimScript
filetypeLoader setting
  = mconcat $ map (uncurry f) $
      collectSndByFst [(ft, p) | p <- S.plugins setting, ft <- P.filetypes p]
  where
    f :: Text -> [P.Plugin] -> VimScript
    f ft plugins = flip foldMap (getHeadChar ft) $
      \c -> let funcname = "miv#" <> singleton c <> "#load_" <> T.filter isAlphaNum (T.toLower ft)
                in VimScript (HM.singleton Plugin
                     [ "augroup miv-file-type-" <> ft
                     , "  autocmd!"
                     , "  autocmd FileType " <> ft <> " call " <> funcname <> "()"
                     , "augroup END" ]) <>
                   VimScript (HM.singleton (Autoload (singleton c)) $
                     ("function! " <> funcname <> "() abort")
                     : "  setlocal filetype="
                     : loadPlugins plugins
                  <> [ "  autocmd! miv-file-type-" <> ft
                     , "  augroup! miv-file-type-" <> ft
                     , "  setlocal filetype=" <> ft
                     , "  silent! doautocmd FileType " <> ft
                     , "endfunction" ])

wrapEnable :: Text -> [Text] -> [Text]
wrapEnable enable str
  | null str = []
  | T.null enable = str
  | enable == "0" = []
  | otherwise = (indent <> "if " <> enable)
                           : map ("  "<>) str
             <> [indent <> "endif"]
  where indent = T.takeWhile (==' ') (head str)

loadPlugins :: [P.Plugin] -> [Text]
loadPlugins plugins = concat
  [wrapEnable enable
    ["  call miv#load(" <> singleQuote (show p) <> ")" | p <- plugins']
      | (enable, plugins') <- collectSndByFst [(P.enable p, p) | p <- plugins]]

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

filetypeScript :: S.Setting -> VimScript
filetypeScript = foldMap (\(ft, src) -> VimScript (HM.singleton (Ftplugin ft) src)) . HM.toList . S.filetype

syntaxScript :: S.Setting -> VimScript
syntaxScript = foldMap (\(ft, src) -> VimScript (HM.singleton (Syntax ft) src)) . HM.toList . S.syntax

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
  , "endfunction" ])

commandLoader :: VimScript
commandLoader = VimScript (HM.singleton (Autoload "")
  [ "function! miv#command(name, command, bang, args, line1, line2) abort"
  , "  silent! execute 'delcommand' a:command"
  , "  call miv#load(a:name)"
  , "  let range = a:line1 != a:line2 ? a:line1.','.a:line2 : ''"
  , "  try"
  , "    exec range.a:command.a:bang a:args"
  , "  catch /^Vim\\%((\\a\\+)\\)\\=:E481:/"
  , "    exec a:command.a:bang a:args"
  , "  endtry"
  , "endfunction" ])

funcUndefinedLoader :: S.Setting -> VimScript
funcUndefinedLoader setting = if null functions then mempty else
  VimScript (HM.singleton Plugin
  [ "\" FuncUndefined"
  , "augroup miv-func-undefined"
  , "  autocmd!"
  , "  autocmd FuncUndefined * call miv#func_undefined(expand('<amatch>'))"
  , "augroup END" ])
  <> VimScript (HM.singleton (Autoload "")
  [ "function! miv#func_undefined(pattern) abort"
  , "  if a:pattern !~# " <> singleQuote (T.intercalate "\\|" functions)
  , "    return"
  , "  endif"
  , "  if empty(s:functions)"
  , "    autocmd! miv-func-undefined"
  , "    augroup! miv-func-undefined"
  , "    return"
  , "  endif"
  , "  for [name, fs] in items(s:functions)"
  , "    for f in fs"
  , "      if a:pattern =~# f"
  , "        call miv#load(name)"
  , "        return"
  , "      endif"
  , "    endfor"
  , "  endfor"
  , "endfunction" ])
  where functions = [ f | p <- S.plugins setting, f <- P.functions p ]

cmdlineEnterLoader :: S.Setting -> VimScript
cmdlineEnterLoader setting
  = mconcat $ map (uncurry f) $
      collectSndByFst [(cmdline, p) | p <- S.plugins setting, cmdline <- P.cmdlines p]
  where
    f :: Cmdline -> [P.Plugin] -> VimScript
    f cmdline plugins = VimScript (HM.singleton Plugin
      [ "\" CmdlineEnter " <> (show cmdline)
      , "if exists('#CmdlineEnter')"
      , "  augroup " <> group
      , "    autocmd!"
      , "    autocmd CmdlineEnter " <> (cmdlinePattern cmdline) <> " call miv#cmdline_enter_" <> c <> "()"
      , "  augroup END"
      , "else"
      , "  call miv#cmdline_enter_" <> c <> "()"
      , "endif" ])
      <> VimScript (HM.singleton (Autoload "") $
       ("function! miv#cmdline_enter_" <> c <> "() abort")
      : loadPlugins plugins
      <> [ "  if exists('#CmdlineEnter')"
      , "    autocmd! " <> group
      , "    augroup! " <> group
      , "  endif"
      , "endfunction" ])
      where c = T.concat $ map (show . ord) (unpack (show cmdline))
            group = "miv-cmdline-enter-" <> c

insertEnterLoader :: S.Setting -> VimScript
insertEnterLoader setting = if null plugins then mempty else
  VimScript (HM.singleton Plugin
  [ "\" InsertEnter"
  , "augroup miv-insert-enter"
  , "  autocmd!"
  , "  autocmd InsertEnter * call miv#insert_enter()"
  , "augroup END" ])
  <> VimScript (HM.singleton (Autoload "")
  $ "function! miv#insert_enter() abort"
  : loadPlugins plugins
  <> [ "  autocmd! miv-insert-enter"
  , "  augroup! miv-insert-enter"
  , "  silent! doautocmd InsertEnter"
  , "endfunction" ])
  where plugins = filter P.insert (S.plugins setting)

pluginLoader :: VimScript
pluginLoader = VimScript (HM.singleton (Autoload "")
  [ "let s:loaded = {}"
  , "let s:path = expand('<sfile>:p:h:h')"
  , "let s:mivpath = expand('<sfile>:p:h:h:h') . '/'"
  , "let s:rtpidx = 0"
  , "function! miv#load(name) abort"
  , "  if has_key(s:loaded, a:name)"
  , "    return"
  , "  endif"
  , "  let s:loaded[a:name] = 1"
  , "  if has_key(s:functions, a:name)"
  , "    call remove(s:functions, a:name)"
  , "  endif"
  , "  for n in get(s:dependon, a:name, [])"
  , "    call miv#load(n)"
  , "  endfor"
  , "  for mode in get(s:mapmodes, a:name, ['n'])"
  , "    for mapping in get(s:mappings, a:name, [])"
  , "      silent! execute mode . 'unmap' mapping"
  , "    endfor"
  , "  endfor"
  , "  let name = substitute(tolower(a:name), '[^a-z0-9]', '', 'g')"
  , "  let au = has_key(s:autoload, name)"
  , "  if au"
  , "    call miv#{s:autoload[name]}#before_{name}()"
  , "  endif"
  , "  let newrtp = s:mivpath . a:name"
  , "  if !isdirectory(newrtp)"
  , "    return"
  , "  endif"
  , "  let rtps = split(&rtp, ',')"
  , "  let s:rtpidx = max([index(rtps, s:path, s:rtpidx), 0])"
  , "  let &rtp = join(insert(rtps, newrtp, s:rtpidx), ',')"
  , "  if isdirectory(newrtp . '/after')"
  , "    exec 'set rtp+=' . newrtp . '/after'"
  , "  endif"
  , "  for dir in filter(['/plugin', '/after/plugin'], 'isdirectory(newrtp . v:val)')"
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
  , "endfunction" ])
