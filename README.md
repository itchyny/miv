# miv [![Build Status](https://travis-ci.org/itchyny/miv.png?branch=master)](https://travis-ci.org/itchyny/miv)

## Installation

```
 $ git clone https://github.com/itchyny/miv
 $ cd miv
 $ sudo cabal install
```

## User guide
### Add miv path to runtimepath in your vimrc.
Example vimrc:
```vim
filetype off
if has('vim_starting')
  set rtp^=~/.vim/miv/miv/
endif
filetype plugin indent on
```

### Create ~/.vimrc.yaml
Example `~/.vimrc.yaml`:
```yaml
plugin:

  Shougo/neocomplete.vim:
    enable: has('lua') && v:version > 703
    beforeScript: |
      let g:neocomplete#enable_at_startup = 1
      let g:neocomplete#enable_smart_case = 1
      let g:neocomplete#max_list = 1000

  thinca/vim-quickrun:
    command: QuickRun
    mapping: <Plug>(quickrun)
    mapleader: \
    dependon:
      - vimproc

  Shougo/vimfiler:
    mapleader: \
    commands:
      - VimFiler
      - VimFilerDouble
      - VimFilerCurrentDir
      - VimFilerBufferDir
      - VimFilerCreate
      - VimFilerSimple
      - VimFilerSplit
      - VimFilerTab
      - VimFilerExplorer
    dependon:
      - vimproc
      - unite
      - vinarise
    beforeScript: |
      let g:vimfiler_as_default_explorer = 1
      let g:vimfiler_sort_type = 'TIME'
    script: |
      nnoremap <silent> <Leader>f :<C-u>VimFilerBufferDir -status -buffer-name=vimfiler -auto-cd<CR>

  Shougo/vinarise:
    loadbefore:
      - vimfiler

  Shougo/unite.vim:
    commands:
      - Unite
      - UniteWithCurrentDir
      - UniteWithBufferDir
      - UniteWithProjectDir
      - UniteWithCursorWord
      - UniteWithInput
      - UniteWithInputDirectory
      - UniteResume
      - UniteClose
    mapleader: ","
    beforeScript: |
      let g:unite_enable_start_insert = 1
      let g:unite_cursor_line_highlight = 'CursorLine'
      let g:unite_force_overwrite_statusline = 0
    script: |
      nnoremap <silent><C-n> :<C-u>Unite file/new directory/new -buffer-name=file/new,directory/new<CR>
      nnoremap <silent><C-o> :<C-u>Unite file file/new -buffer-name=file<CR>
      nnoremap <silent><S-l> :<C-u>Unite line -buffer-name=line<CR>
    dependedby:
      - unite-build
      - unite-colorscheme
      - unite-highlight

  Shougo/unite-build:
    loadafter:
      - unite

  ujihisa/unite-colorscheme:
    loadafter:
      - unite

  osyo-manga/unite-highlight:
    loadafter:
      - unite

  Shougo/vimshell.vim:
    commands:
      - VimShell
      - VimShellCreate
      - VimShellPop
      - VimShellTab
      - VimShellCurrentDir
      - VimShellBufferDir
      - VimShellExecute
      - VimShellInteractive
      - VimShellTerminal
      - VimShellSendString
      - VimShellSendBuffer
    mapleader: ;
    dependon:
      - vimproc
    beforeScript: |
      let g:vimshell_popup_command = 'top new'
      let g:vimshell_split_command = 'vsplit'
    script: |
      nnoremap <silent> <Leader>s :<C-u>VimShellBufferDir<CR>
      nnoremap <silent> H :<C-u>VimShellBufferDir -popup<CR>

  Shougo/vimproc.vim:
    build: make
    function: vimproc
    loadbefore:
      - vimfiler
      - vimshell
      - quickrun

  Align:
    command: Align

  tyru/capture.vim:
    command: Capture

  itchyny/calendar.vim:
    mapleader: ","
    command: Calendar
    mapping: <Plug>(calendar)
    script: |
      map <silent> <Leader>z <Plug>(calendar)
    beforeScript: |
      let g:calendar_views = [ 'year', 'month', 'day_3', 'clock' ]
      let g:calendar_google_calendar = 1
      let g:calendar_google_task = 1

  elzr/vim-json:
    filetype: json

  mattn/emmet-vim:
    filetype: html
    beforeScript: |
      let g:user_emmet_settings = { 'indentation' : '  ' }
    afterScript: |
      imap <buffer><silent> <tab> <plug>(emmet-expand-abbr)

  wavded/vim-stylus:
    filetype: stylus

  groenewege/vim-less:
    filetype: less

  tpope/vim-haml:
    filetype: haml

  jade.vim:
    filetype: jade

  kchmck/vim-coffee-script:
    filetype: coffee
```

### `miv install`

## `miv` subcommands
### `miv install`
Installs all the plugins.

### `miv update`
Update all the plugins.

### `miv install [plugins]`
Installs the specified plugins.

### `miv update [plugins]`
Update the specified plugins.

### `miv clean`
Remove the unused directories.

### `miv generate`
Generate the miv plugin files. (`miv install` and `miv update` automatically do this task)

### `miv helptags`
Generate the helptags file. (`miv install` and `miv update` automatically do this task)

### `miv list`
List all the plugins.

### `miv edit`
Edit the `~/.vimrc.yaml` file.

### `miv command`
List the subcommands of `miv`.

### `miv each [commands]`
Execute the commands each directory of the plugins.

### `miv help`
Show the help of `miv`.

### `miv version`
Show the version of `miv`.

