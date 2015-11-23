# miv [![Build Status](https://travis-ci.org/itchyny/miv.png?branch=master)](https://travis-ci.org/itchyny/miv)

## Installation

```
 $ git clone https://github.com/itchyny/miv
 $ cd miv
 $ stack install
```

### AUR

miv is available in the [AUR](https://aur.archlinux.org/packages/miv-git/):

```
 $ pacaur -S miv-git
```

## User guide
1. Add the miv plugin path to runtimepath in your vimrc.
2. Create ~/.vimrc.yaml.
3. Execute `miv install`.

Example vimrc:
```vim
filetype off
if has('vim_starting')
  set rtp^=~/.vim/miv/miv/
endif
filetype plugin indent on
```

Example `~/.vimrc.yaml`:
```
plugin:

  Valloric/YouCompleteMe: 
    submodule: true
    commands: 
      - YcmCompleter
      - YcmRestartServer
      - YcmDiags
      - YcmShowDetailedDiagnostic
      - YcmDebugInfo

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
      - VimFilerTab
      - VimFilerBufferDir
      - VimFilerExplorer
    dependon:
      - vimproc
      - unite
    beforeScript: |
      let g:vimfiler_as_default_explorer = 1
      let g:vimfiler_sort_type = 'TIME'
    script: |
      nnoremap <silent> <Leader>f :<C-u>VimFilerBufferDir -auto-cd<CR>

  Shougo/vinarise:
    commands:
      - Vinarise

  Shougo/unite.vim:
    commands:
      - Unite
    mapleader: ","
    function: unite
    beforeScript: |
      let g:unite_force_overwrite_statusline = 0
    script: |
      nnoremap <silent><C-n> :<C-u>Unite file/new directory/new<CR>
      nnoremap <silent><C-o> :<C-u>Unite file file/new<CR>
      nnoremap <silent><S-l> :<C-u>Unite line<CR>
    afterScript: |
      call unite#custom#profile('default', 'context', {
            \ 'start_insert' : 1,
            \ 'prompt_direction': 'top',
            \ 'hide_icon': 0 })
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
      - VimShellPop
      - VimShellTab
      - VimShellInteractive
    function: vimshell
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
    filetypes: 
      - html
      - css
    beforeScript: |
      let g:user_emmet_settings = { 'indentation' : '  ' }
    afterScript: |
      autocmd FileType html,css imap <buffer> <tab> <plug>(emmet-expand-abbr)

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

  kana/vim-textobj-user: {}

  kana/vim-textobj-entire:
    dependon:
      - textobj-user
    mapmodes:
      - o
      - v
    mappings:
      - <Plug>(textobj-entire-a)
      - <Plug>(textobj-entire-i)
      - ie
      - ae

  kana/vim-textobj-line:
    dependon:
      - textobj-user
    mapmodes:
      - o
      - v
    mappings:
      - <Plug>(textobj-line-a)
      - <Plug>(textobj-line-i)
      - il
      - al
```

## `miv` subcommands
### `miv install`
Installs all the plugins.

### `miv update`
Updates the plugins (outdated plugins are skipped).

### `miv update!`
Updates all the plugins.

### `miv install [plugins]`
Installs the specified plugins.

### `miv update [plugins]`
Updates the specified plugins.

### `miv clean`
Removes the unused directories.

### `miv generate`
Generates the miv plugin files. (`miv install` and `miv update` automatically do this task)

### `miv helptags`
Generates the helptags file. (`miv install` and `miv update` automatically do this task)

### `miv list`
Lists all the plugins.

### `miv edit`
Edits the `~/.vimrc.yaml` file.

### `miv command`
Lists the subcommands of `miv`.

### `miv path [plugins]`
Prints the paths of the plugins.

### `miv each [commands]`
Executes the commands each directory of the plugins. For example, you can execute `miv each pwd` or `miv each git gc`.

### `miv help`
Shows the help of `miv`.

### `miv version`
Shows the version of `miv`.

