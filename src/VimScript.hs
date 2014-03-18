{-# LANGUAGE DeriveGeneric #-}
module VimScript where

import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.Char (toLower, isAlphaNum)
import Data.List (intercalate)
import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import GHC.Generics

import qualified Setting as S
import qualified Plugin as P
import qualified Command as C
import qualified Mapping as M
import Mode

data VimScript = VimScript (HM.HashMap Place [String])
               deriving (Eq, Show)

data Place = Plugin
           | Autoload
           | AutoloadSubdir String
           deriving (Eq, Generic)

instance Hashable Place where
  hash Plugin = 0
  hash Autoload = 1
  hash (AutoloadSubdir s) = 2 + hash s

instance Show Place where
  show Plugin = "plugin/miv.vim"
  show Autoload = "autoload/miv.vim"
  show (AutoloadSubdir s) = "autoload/miv/" ++ s ++ ".vim"

autoloadSubdirName :: Place -> Maybe String
autoloadSubdirName (AutoloadSubdir s) = Just s
autoloadSubdirName _ = Nothing

vimScriptToList :: VimScript -> [(Place, [String])]
vimScriptToList (VimScript x) = HM.toList x

(+++) :: VimScript -> VimScript -> VimScript
(+++) (VimScript x) (VimScript y)
  | HM.null x = VimScript y
  | HM.null y = VimScript x
  | otherwise = VimScript (HM.unionWith concat' x y)
  where
    concat' a [] = a
    concat' [] b = b
    concat' a b = a ++ [""] ++ b

gatherScript :: S.Setting -> VimScript
gatherScript setting = addAutoloadNames
                     $ beforeScript setting
                   +++ dependOnScript (S.plugin setting)
                   +++ dependedByScript (S.plugin setting)
                   +++ pluginLoader
                   +++ mappingLoader
                   +++ commandLoader
                   +++ foldl (+++) (VimScript HM.empty) (map pluginConfig (S.plugin setting))
                   +++ filetypeLoader setting
                   +++ gatherInsertEnter setting
                   +++ gatherFuncUndefined setting
                   +++ filetypeScript (S.filetypeScript setting)
                   +++ afterScript setting

addAutoloadNames :: VimScript -> VimScript
addAutoloadNames h@(VimScript hm)
  = VimScript (HM.singleton Autoload
      ["let s:au = { " ++ intercalate ", " ((\x -> singleQuote x ++ ": 1")
                                    <$> mapMaybe autoloadSubdirName (HM.keys hm)) ++ " }"])
  +++ h

dependOnScript :: [P.Plugin] -> VimScript
dependOnScript plg
  = VimScript (HM.singleton Autoload $
      [ "let s:dependon = {" ]
   ++ [ "      \\ " ++ singleQuote (P.rtpName p) ++ ": [ " ++
                     intercalate ", " [ singleQuote q | q <- P.dependon p ]
                  ++ " ]," | p <- plg, not (null (P.dependon p)) ]
   ++ [ "      \\ }" ])

dependedByScript :: [P.Plugin] -> VimScript
dependedByScript plg
  = VimScript (HM.singleton Autoload $
      [ "let s:dependedby = {" ]
   ++ [ "      \\ " ++ singleQuote (P.rtpName p) ++ ": [ " ++
                     intercalate ", " [ singleQuote q | q <- P.dependedby p ]
                  ++ " ]," | p <- plg, not (null (P.dependedby p)) ]
   ++ [ "      \\ }" ])

pluginConfig :: P.Plugin -> VimScript
pluginConfig plg
    = VimScript (HM.singleton Plugin $ wrapInfo $
        wrapEnable plg $ mapleader
                      ++ P.script plg
                      ++ gatherCommand plg
                      ++ gatherMapping plg
                      ++ loadScript plg)
  +++ VimScript (HM.singleton aufile (wrapFunction (snd (funcname "before")) (P.beforeScript plg)))
  +++ VimScript (HM.singleton aufile (wrapFunction (snd (funcname "after")) (P.afterScript plg)))
  where
    wrapInfo [] = []
    wrapInfo str = ("\" " ++ P.name plg) : str
    mapleader = (\s -> if null s then [] else ["let g:mapleader = " ++ singleQuote s]) (P.mapleader plg)
    aufile = case funcname "before" of ("", _) -> Plugin; (c, _) -> AutoloadSubdir c
    name = filter isAlphaNum (map toLower (P.rtpName plg))
    funcname str =
      case getHeadChar (P.rtpName plg) of
           Nothing -> ("", "s:" ++ str ++ "_" ++ name)
           Just c -> ([c], "miv#" ++ [c] ++ "#" ++ str ++ "_" ++ name)

loadScript :: P.Plugin -> [String]
loadScript plg
  | all null [ P.commands plg, P.mappings plg, P.functions plg, P.filetypes plg
             , P.loadafter plg, P.loadbefore plg ] && not (P.insert plg)
  = ["call miv#load(" ++ singleQuote (P.rtpName plg) ++ ")"]
  | otherwise = []

gatherCommand :: P.Plugin -> [String]
gatherCommand plg
  | not (null (P.commands plg))
    = [show (C.defaultCommand { C.cmdName = c
        , C.cmdRepText = unwords ["call miv#command(" ++ singleQuote (P.rtpName plg) ++ ","
                               , singleQuote c ++ ","
                               , singleQuote "<bang>" ++ ","
                               , "<q-args>,"
                               , "expand('<line1>'),"
                               , "expand('<line2>'))" ] }) | c <- P.commands plg]
  | otherwise = []

gatherMapping :: P.Plugin -> [String]
gatherMapping plg
  | not (null (P.mappings plg))
    = let genMapping
            = [\mode ->
               M.defaultMapping
                  { M.mapName    = c
                  , M.mapRepText = escape mode ++ ":<C-u>call miv#mapping("
                        ++ singleQuote (P.rtpName plg) ++ ", "
                        ++ singleQuote c ++ ", "
                        ++ singleQuote (show mode) ++ ")<CR>"
                  , M.mapMode    = mode } | c <- P.mappings plg]
          escape m = if m == InsertMode then "<ESC>" else ""
          modes = [NormalMode, VisualMode, InsertMode]
          in concat [map (show . f) modes | f <- genMapping]
  | otherwise = []

beforeScript :: S.Setting -> VimScript
beforeScript setting = VimScript (HM.singleton Plugin (S.beforeScript setting))

afterScript :: S.Setting -> VimScript
afterScript setting = VimScript (HM.singleton Plugin (S.afterScript setting))

filetypeLoader :: S.Setting -> VimScript
filetypeLoader setting
  = HM.foldrWithKey f (VimScript HM.empty) (filetypeLoadPlugins (S.plugin setting) HM.empty)
  where
    f ft plg val =
      case getHeadChar ft of
           Nothing -> val
           Just c ->
             let funcname = "miv#" ++ [c] ++ "#load_" ++ filter isAlphaNum (map toLower ft)
                 in val
                  +++ VimScript (HM.singleton (AutoloadSubdir [c])
                       (("function! " ++ funcname ++ "()")
                       : "  setl ft="
                       :  concat [wrapEnable b
                       [ "  call miv#load(" ++ singleQuote (P.rtpName b) ++ ")"] | b <- plg]
                    ++ [ "  autocmd! MivFileTypeLoad" ++ ft
                       , "  augroup! MivFileTypeLoad" ++ ft
                       , "  setl ft=" ++ ft
                       , "  silent! doautocmd FileType " ++ ft
                       , "endfunction"
                       ]))
                  +++ VimScript (HM.singleton Plugin
                       [ "augroup MivFileTypeLoad" ++ ft
                       , "  autocmd!"
                       , "  autocmd FileType " ++ ft ++ " call " ++ funcname ++ "()"
                       , "augroup END"
                       ])

filetypeLoadPlugins :: [P.Plugin] -> HM.HashMap String [P.Plugin] -> HM.HashMap String [P.Plugin]
filetypeLoadPlugins (b:plugins) fts
  | not (null (P.filetypes b))
  = filetypeLoadPlugins plugins (foldr (flip (HM.insertWith (++)) [b]) fts (P.filetypes b))
  | otherwise = filetypeLoadPlugins plugins fts
filetypeLoadPlugins [] fts = fts

gatherInsertEnter :: S.Setting -> VimScript
gatherInsertEnter setting
  = VimScript (HM.singleton Plugin (f [ p | p <- S.plugin setting, P.insert p ]))
  where f [] = []
        f plgs = "\" InsertEnter"
               : "function! s:insertEnter()"
             : [ "  call miv#load(" ++ singleQuote (P.rtpName p) ++ ")" | p <- plgs ]
            ++ [ "  autocmd! MivInsertEnter"
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
               : "function! s:funcUndefined()"
               : "  let f = expand('<amatch>')"
               : concat [
               [ "  if f =~# " ++ singleQuote q
               , "    call miv#load(" ++ singleQuote (P.rtpName p) ++ ")"
               , "  endif" ] | (p, q) <- concatMap (\q -> map ((,) q) (P.functions q)) plgs]
            ++ [ "endfunction"
               , ""
               , "augroup MivFuncUndefined"
               , "  autocmd!"
               , "  autocmd FuncUndefined * call s:funcUndefined()"
               , "augroup END"
               , "" ]

wrapEnable :: P.Plugin -> [String] -> [String]
wrapEnable plg str
  | null str = []
  | null (P.enable plg) = str
  | otherwise = (indent ++ "if " ++ P.enable plg)
                           : map ("  "++) str
                           ++ [indent ++ "endif"]
  where indent = takeWhile (==' ') (head str)

singleQuote :: String -> String
singleQuote str = "'" ++ str ++ "'"

filetypeScript :: HM.HashMap String [String] -> VimScript
filetypeScript =
  HM.foldrWithKey f
    (VimScript (HM.singleton Autoload
    [ "augroup MivFileType"
    , "  autocmd!"
    , "  autocmd FileType * call miv#filetype(expand('<amatch>'))"
    , "augroup END"
    , ""
    , "function! miv#filetype(ft)"
    , "  let c = substitute(tolower(a:ft), '[^a-z0-9]', '', 'g')"
    , "  if len(c) && get(s:au, c[0])"
    , "    silent! call miv#{c[0]}#filetype_{c}()"
    , "  endif"
    , "endfunction"
    ]))
  where
    f ft scr val =
      case getHeadChar ft of
           Nothing -> val
           Just c ->
             let funcname = "miv#" ++ [c] ++ "#filetype_" ++ filter isAlphaNum (map toLower ft)
                 in val +++ VimScript (HM.singleton (AutoloadSubdir [c]) (wrapFunction funcname scr))

wrapFunction :: String -> [String] -> [String]
wrapFunction funcname script =
     ["function! " ++ funcname ++ "()"]
  ++ map ("  "++) script
  ++ ["endfunction"]

getHeadChar :: String -> Maybe Char
getHeadChar (x:xs) | 'a' <= x && x <= 'z' = Just x
                   | 'A' <= x && x <= 'Z' = Just (toLower x)
                   | otherwise = getHeadChar xs
getHeadChar [] = Nothing

mappingLoader :: VimScript
mappingLoader = VimScript (HM.singleton Autoload
  [ "function! miv#mapping(name, mapping, mode)"
  , "  silent! execute a:mode . 'unmap' a:mapping"
  , "  call miv#load(a:name)"
  , "  call feedkeys(substitute(a:mapping, '<Plug>', \"\\<Plug>\", 'g'), 'm')"
  , "  return ''"
  , "endfunction"
  ])

commandLoader :: VimScript
commandLoader = VimScript (HM.singleton Autoload
  [ "function! miv#command(name, command, bang, args, line1, line2)"
  , "  silent! execute 'delcommand' a:command"
  , "  call miv#load(a:name)"
  , "  let range = a:line1 != a:line2 ? \"'<,'>\" : ''"
  , "  try"
  , "    exec range.a:command.a:bang a:args"
  , "  catch /^Vim\\%((\\a\\+)\\)\\=:E481/"
  , "    exec a:command.a:bang a:args"
  , "  endtry"
  , "endfunction"
  ])

pluginLoader :: VimScript
pluginLoader = VimScript (HM.singleton Autoload
  [ "let s:loaded = {}"
  , "function! miv#load(name)"
  , "  if has_key(s:loaded, a:name)"
  , "    return"
  , "  endif"
  , "  let s:loaded[a:name] = 1"
  , "  for n in get(s:dependon, a:name, [])"
  , "    call miv#load(n)"
  , "  endfor"
  , "  let c = substitute(tolower(a:name), '[^a-z0-9]', '', 'g')"
  , "  if len(c) && get(s:au, c[0])"
  , "    call miv#{c[0]}#before_{c}()"
  , "  endif"
  , "  let newrtp = expand('~/.vim/miv/' . a:name . '/')"
  , "  if !isdirectory(newrtp)"
  , "    return"
  , "  endif"
  , "  let rtps = split(&rtp, ',')"
  , "  let &rtp = join(insert(rtps, newrtp, index(rtps, expand('~/.vim'))), ',')"
  , "  if isdirectory(newrtp . 'after/')"
  , "    exec 'set rtp+=' . newrtp . 'after/'"
  , "  endif"
  , "  for dir in filter(['ftdetect', 'after/ftdetect', 'plugin', 'after/plugin'], 'isdirectory(newrtp . v:val)')"
  , "    for file in split(glob(newrtp . dir . '/**/*.vim'), '\\n')"
  , "      silent! source `=file`"
  , "    endfor"
  , "  endfor"
  , "  if len(c) && get(s:au, c[0])"
  , "    call miv#{c[0]}#after_{c}()"
  , "  endif"
  , "  for n in get(s:dependedby, a:name, [])"
  , "    call miv#load(n)"
  , "  endfor"
  , "endfunction"
  ])
