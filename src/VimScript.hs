module VimScript where

import Data.Char (isAlpha, isAlphaNum, isSpace, ord, toLower)
import Data.Default (def)
import Data.List (foldl', nub, sort)
import Data.List.Extra (groupSort)
import Data.Map.Strict qualified as M
import Data.Text (Text, singleton, unpack, unwords)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as Builder
import Data.Text.Display (Display(..), display)
import Prelude hiding (unwords)

import Cmdline
import Command
import Mapping
import Mode
import Plugin
import Setting

newtype VimScript = VimScript (M.Map Place [Text])
                  deriving Eq

data Place = Plugin'
           | Autoload Text
           | Ftplugin Text
           | Syntax   Text
           deriving (Eq, Ord)

instance Display Place where
  displayBuilder = Builder.fromText . \case
    Plugin'     -> "plugin/miv.vim"
    Autoload "" -> "autoload/miv.vim"
    Autoload s  -> "autoload/miv/" <> s <> ".vim"
    Ftplugin s  -> "ftplugin/" <> s <> ".vim"
    Syntax s    -> "syntax/" <> s <> ".vim"

vimScriptToList :: VimScript -> [(Place, [Text])]
vimScriptToList (VimScript x) = M.toList x

instance Semigroup VimScript where
  VimScript x <> VimScript y
    | M.null x = VimScript y
    | M.null y = VimScript x
    | otherwise = VimScript (M.unionWith concat' x y)
    where
      concat' a [] = a
      concat' [] b = b
      concat' a b  = a <> [""] <> b

instance Monoid VimScript where
  mempty = VimScript M.empty

gatherScript :: Setting -> VimScript
gatherScript setting = beforeScript setting
                    <> gatherBeforeAfterScript setting.plugins
                    <> gather' "dependon" (.dependon) (.loadbefore) setting.plugins
                    <> gather' "dependedby" (.dependedby) (.loadafter) setting.plugins
                    <> gather "mappings" (.mappings) setting.plugins
                    <> gather "mapmodes" (map display . (.mapmodes)) setting.plugins
                    <> gather "functions" (.functions) setting.plugins
                    <> pluginLoader
                    <> mappingLoader
                    <> commandLoader
                    <> foldl' (<>) mempty (map pluginConfig setting.plugins)
                    <> filetypeLoader setting
                    <> funcUndefinedLoader setting
                    <> cmdlineEnterLoader setting
                    <> insertEnterLoader setting
                    <> filetypeScript setting
                    <> syntaxScript setting
                    <> afterScript setting

gatherBeforeAfterScript :: [Plugin] -> VimScript
gatherBeforeAfterScript x = insertAuNameMap $ gatherScripts x (mempty, M.empty)
  where
    insertAuNameMap :: (VimScript, M.Map Text Text) -> VimScript
    insertAuNameMap (vs, m) = VimScript (M.singleton (Autoload "") $
          [ "let s:autoload = {" ]
       <> [ "      \\ " <> singleQuote k <> ": " <> singleQuote a <> "," | (k, a) <- M.toList m ]
       <> [ "      \\ }" ]) <> vs
    gatherScripts :: [Plugin] -> (VimScript, M.Map Text Text) -> (VimScript, M.Map Text Text)
    gatherScripts (p:ps) (vs, m)
            | null p.before && null p.after = gatherScripts ps (vs, m)
            | otherwise = gatherScripts ps (vs <> vs', M.insert name hchar m)
      where
        name = T.filter isAlphaNum (T.toLower (display p))
        hchar | null (loadScript p) = maybe "_" singleton $ getHeadChar $ display p
              | otherwise = "_"
        funcname str = "miv#" <> hchar <> "#" <> str <> "_" <> name
        vs' = VimScript $ M.singleton (Autoload hchar) $
          wrapFunction (funcname "before") p.before <>
          wrapFunction (funcname "after") p.after
    gatherScripts [] (vs, m) = (vs, m)

gather :: Text -> (Plugin -> [Text]) -> [Plugin] -> VimScript
gather name f plugin
  = VimScript (M.singleton (Autoload "") $
      [ "let s:" <> name <> " = {" ]
   <> [ "      \\ " <> singleQuote (display p)
                    <> ": [ " <> T.intercalate ", " (map singleQuote (f p))  <> " ],"
        | p <- plugin, p.enable /= "0", not (null (f p)) ]
   <> [ "      \\ }" ])

gather' :: Text -> (Plugin -> [Text]) -> (Plugin -> [Text]) -> [Plugin] -> VimScript
gather' name f g plugin
  = VimScript (M.singleton (Autoload "") $
      [ "let s:" <> name <> " = {" ]
   <> [ "      \\ " <> singleQuote p
                    <> ": [ " <> T.intercalate ", " (map singleQuote $ sort $ nub q)  <> " ],"
        | (p, q) <- groupSort $ [ (display p, q) | p <- plugin, p.enable /= "0", q <- f p ]
                             <> [ (q, display p) | p <- plugin, p.enable /= "0", q <- g p ] ]
   <> [ "      \\ }" ])

pluginConfig :: Plugin -> VimScript
pluginConfig plugin
  = VimScript (M.singleton Plugin' $ wrapInfo $
      wrapEnable plugin.enable $ mapleader <> gatherCommand plugin <> gatherMapping plugin <> plugin.script <> loadScript plugin)
  where
    wrapInfo []  = []
    wrapInfo str = ("\" " <> plugin.name) : str
    mapleader = let s = plugin.mapleader in ["let g:mapleader = " <> singleQuote s | not (T.null s)]

loadScript :: Plugin -> [Text]
loadScript plugin
  | all null [ plugin.commands, plugin.mappings, plugin.functions, plugin.filetypes
             , plugin.loadafter, plugin.loadbefore ] && null plugin.cmdlines && not plugin.insert
  = ["call miv#load(" <> singleQuote (display plugin) <> ")"]
  | otherwise = []

gatherCommand :: Plugin -> [Text]
gatherCommand plugin
  | not (null plugin.commands)
    = [display (def :: Command)
          { name = c,
            repl = unwords [
              "call miv#command(" <> singleQuote (display plugin) <> ",",
              singleQuote c <> ",",
              singleQuote "<bang>" <> ",",
              "<q-args>,",
              "expand('<line1>'),",
              "expand('<line2>'))"
          ] } | c <- plugin.commands]
  | otherwise = []

gatherMapping :: Plugin -> [Text]
gatherMapping plugin
  | not (null plugin.mappings)
    = [display (def :: Mapping)
          { name = mapping,
            repl = escape mode <> ":<C-u>call miv#mapping("
                <> singleQuote (display plugin) <> ", "
                <> singleQuote mapping <> ", "
                <> singleQuote (display mode) <> ")<CR>",
            mode = mode
          } | mapping <- plugin.mappings, mode <- modes]
  | otherwise = []
    where modes = if null plugin.mapmodes then [NormalMode] else plugin.mapmodes
          escape mode = if mode `elem` [InsertMode, OperatorPendingMode] then "<ESC>" else ""

beforeScript :: Setting -> VimScript
beforeScript setting = VimScript (M.singleton Plugin' setting.before)

afterScript :: Setting -> VimScript
afterScript setting = VimScript (M.singleton Plugin' setting.after)

filetypeLoader :: Setting -> VimScript
filetypeLoader setting =
  mconcat $ map (uncurry f) $ groupSort [(ft, p) | p <- setting.plugins, ft <- p.filetypes]
  where
    f :: Text -> [Plugin] -> VimScript
    f ft plugins = flip foldMap (getHeadChar ft) $
      \c -> let funcname = "miv#" <> singleton c <> "#load_" <> T.filter isAlphaNum (T.toLower ft)
                in VimScript (M.singleton Plugin'
                     [ "augroup miv-file-type-" <> ft
                     , "  autocmd!"
                     , "  autocmd FileType " <> ft <> " call " <> funcname <> "()"
                     , "augroup END" ]) <>
                   VimScript (M.singleton (Autoload (singleton c)) $
                     ("function! " <> funcname <> "() abort")
                     : "  setlocal filetype="
                     : loadPlugins plugins
                  <> [ "  autocmd! miv-file-type-" <> ft
                     , "  augroup! miv-file-type-" <> ft
                     , "  setlocal filetype=" <> ft
                     , "  silent! doautocmd FileType " <> ft
                     , "endfunction" ])

wrapEnable :: Text -> [Text] -> [Text]
wrapEnable _ [] = []
wrapEnable "" strs = strs
wrapEnable "0" _ = []
wrapEnable enable strs@(s:_) =
  [indent <> "if " <> enable] <>
        map ("  "<>) strs <>
  [indent <> "endif"]
  where indent = T.takeWhile isSpace s

loadPlugins :: [Plugin] -> [Text]
loadPlugins plugins = concat
  [wrapEnable enable
    ["  call miv#load(" <> singleQuote (display p) <> ")" | p <- plugins']
      | (enable, plugins') <- groupSort [(p.enable, p) | p <- plugins]]

singleQuote :: Text -> Text
singleQuote str = "'" <> str <> "'"

filetypeScript :: Setting -> VimScript
filetypeScript setting = foldMap (\(ft, src) -> VimScript (M.singleton (Ftplugin ft) src)) $ M.toList setting.filetype

syntaxScript :: Setting -> VimScript
syntaxScript setting = foldMap (\(ft, src) -> VimScript (M.singleton (Syntax ft) src)) $ M.toList setting.syntax

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
mappingLoader = VimScript (M.singleton (Autoload "")
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
commandLoader = VimScript (M.singleton (Autoload "")
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

funcUndefinedLoader :: Setting -> VimScript
funcUndefinedLoader setting = if null functions then mempty else
  VimScript (M.singleton Plugin'
  [ "\" FuncUndefined"
  , "augroup miv-func-undefined"
  , "  autocmd!"
  , "  autocmd FuncUndefined * call miv#func_undefined(expand('<amatch>'))"
  , "augroup END" ])
  <> VimScript (M.singleton (Autoload "")
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
  where functions = [f | p <- setting.plugins, f <- p.functions]

cmdlineEnterLoader :: Setting -> VimScript
cmdlineEnterLoader setting =
  mconcat $ map (uncurry f) $ groupSort [(cmdline, p) | p <- setting.plugins, cmdline <- p.cmdlines]
  where
    f :: Cmdline -> [Plugin] -> VimScript
    f cmdline plugins = VimScript (M.singleton Plugin'
      [ "\" CmdlineEnter " <> display cmdline
      , "if exists('#CmdlineEnter')"
      , "  augroup " <> group
      , "    autocmd!"
      , "    autocmd CmdlineEnter " <> cmdlinePattern cmdline <> " call miv#cmdline_enter_" <> c <> "()"
      , "  augroup END"
      , "else"
      , "  call miv#cmdline_enter_" <> c <> "()"
      , "endif" ])
      <> VimScript (M.singleton (Autoload "") $
       ("function! miv#cmdline_enter_" <> c <> "() abort")
      : loadPlugins plugins
      <> [ "  if exists('#CmdlineEnter')"
      , "    autocmd! " <> group
      , "    augroup! " <> group
      , "  endif"
      , "endfunction" ])
      where c = T.concat $ map (display . ord) (unpack (display cmdline))
            group = "miv-cmdline-enter-" <> c

insertEnterLoader :: Setting -> VimScript
insertEnterLoader setting = if null plugins then mempty else
  VimScript (M.singleton Plugin'
  [ "\" InsertEnter"
  , "augroup miv-insert-enter"
  , "  autocmd!"
  , "  autocmd InsertEnter * call miv#insert_enter()"
  , "augroup END" ])
  <> VimScript (M.singleton (Autoload "")
  $ "function! miv#insert_enter() abort"
  : loadPlugins plugins
  <> [ "  autocmd! miv-insert-enter"
  , "  augroup! miv-insert-enter"
  , "  silent! doautocmd InsertEnter"
  , "endfunction" ])
  where plugins = filter (.insert) setting.plugins

pluginLoader :: VimScript
pluginLoader = VimScript (M.singleton (Autoload "")
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
