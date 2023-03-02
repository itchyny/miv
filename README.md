# miv
[![CI Status](https://github.com/itchyny/miv/workflows/CI/badge.svg)](https://github.com/itchyny/miv/actions)
[![Hackage](https://img.shields.io/hackage/v/miv.svg)](https://hackage.haskell.org/package/miv)
[![Release](https://img.shields.io/github/release/itchyny/miv/all.svg)](https://github.com/itchyny/miv/releases)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/itchyny/miv/blob/main/LICENSE)

### Vim plugin manager written in Haskell
The `miv` is a command line tool for managing Vim plugins with a single YAML
configuration file. The motivation of this tool is

- to generate a Vim plugin loader files in Vim script
  - A plugin manager written in Vim script build script code and evaluates it
    on editor startup. But the executed commands do not change unless the
    user's configurations do not change. Instead of building the commands on
    editor startup, `miv` generates static plugin loader scripts after plugin
    installation.
- to provide a declarative way to manage Vim plugins
  - Various loading triggers, script configurations and loading dependency can
    be defined.
- to provide a command line tool which is friendly to interact with other tools
  - You can easily update the Vim plugins in cron schedule or from shell script.

## Installation
### Homebrew
```sh
brew install itchyny/tap/miv
```

### Build with stack
```sh
git clone https://github.com/itchyny/miv.git && cd miv && stack install
```

## User guide
1. Add the miv plugin path to `runtimepath` in your `.vimrc`.
2. Create miv configuration file at `~/.vimrc.yaml`. (or `~/.vim/.vimrc.yaml`, `$XDG_CONFIG_HOME/miv/config.yaml`)
3. Execute `miv install`.

Example vimrc:
```vim
filetype off
if has('vim_starting')
  set rtp^=~/.vim/miv/miv
  " or when you set $XDG_DATA_HOME,
  " set rtp^=$XDG_DATA_HOME/miv/miv
endif
filetype plugin indent on
```

Example miv configuration file (refer to [.vimrc.yaml](https://github.com/itchyny/dotfiles/blob/main/.vimrc.yaml) for how the author configures):
```yaml
plugin:

  Align:
    command: Align

  itchyny/lightline.vim:
    before: |
      let g:lightline = {
            \   'colorscheme': 'wombat',
            \ }

  itchyny/calendar.vim:
    mapleader: ","
    command: Calendar
    mapping: <Plug>(calendar)
    script: |
      nmap <Leader>z <Plug>(calendar)
    before: |
      let g:calendar_views = [ 'year', 'month', 'day_3', 'clock' ]

  prabirshrestha/vim-lsp:
    before: |
      let g:lsp_async_completion = 1
      let g:lsp_text_edit_enabled = 0
      let g:lsp_signs_enabled = 0
      augroup lsp_install
        autocmd!
        autocmd User lsp_buffer_enabled setlocal omnifunc=lsp#complete
      augroup END

  prabirshrestha/asyncomplete.vim: {}

  prabirshrestha/asyncomplete-lsp.vim:
    dependon: asyncomplete

  prabirshrestha/asyncomplete-buffer.vim:
    dependon: asyncomplete
    after: |
      call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
          \ 'name': 'buffer',
          \ 'whitelist': ['*'],
          \ 'completor': function('asyncomplete#sources#buffer#completor'),
          \ 'config': {
          \    'max_buffer_size': 100000,
          \  },
          \ }))

  prabirshrestha/asyncomplete-file.vim:
    dependon: asyncomplete
    after: |
      call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
          \ 'name': 'file',
          \ 'whitelist': ['*'],
          \ 'completor': function('asyncomplete#sources#file#completor'),
          \ }))

  mattn/vim-lsp-settings:
    dependon: lsp

  mattn/emmet-vim:
    filetype:
      - html
      - css
    before: |
      let g:user_emmet_settings = { 'indentation' : '  ' }
    after: |
      autocmd FileType html,css imap <buffer> <tab> <plug>(emmet-expand-abbr)

  elzr/vim-json:
    filetype: json

  cespare/vim-toml:
    filetype: toml

  groenewege/vim-less:
    filetype: less

  tpope/vim-haml:
    filetype: haml

  jade.vim:
    filetype: jade

  kana/vim-textobj-user: {}

  kana/vim-textobj-entire:
    dependon: textobj-user
    mapmode:
      - o
      - v
    mapping:
      - <Plug>(textobj-entire-a)
      - <Plug>(textobj-entire-i)
      - ie
      - ae

  kana/vim-textobj-line:
    dependon: textobj-user
    mapmode:
      - o
      - v
    mapping:
      - <Plug>(textobj-line-a)
      - <Plug>(textobj-line-i)
      - il
      - al

before: |
  let g:is_bash = 1
  let g:loaded_2html_plugin = 1
  let g:loaded_rrhelper = 1

after: |
  let g:mapleader = ','

filetype:
  vim: |
    setlocal foldmethod=marker
  css: |
    setlocal iskeyword=37,45,48-57,95,a-z,A-Z,192-255
  make: |
    setlocal noexpandtab
  sh: |
    setlocal iskeyword=36,45,48-57,64,95,a-z,A-Z,192-255
```

## `miv` subcommands
The `miv` command has the following subcommands.

|command|description|
|:--|:--|
|`install`|Installs all the plugins.|
|`update`|Updates the plugins (outdated plugins are skipped).|
|`update!`|Updates all the plugins.|
|`update [plugins]`|Updates the specified plugins.|
|`clean`|Removes unused directories and files.|
|`generate`|Generates the miv plugin files. (`miv install` and `miv update` automatically do this task)|
|`ftdetect`|Gather ftdetect scripts. (`miv install` and `miv update` automatically do this task)|
|`helptags`|Generates the helptags file. (`miv install` and `miv update` automatically do this task)|
|`list`|Lists all the plugins.|
|`edit`|Edits the miv config file.|
|`command`|Lists the subcommands of `miv`.|
|`path [plugins]`|Prints the paths of the plugins.|
|`each [commands]`|Executes the commands each directory of the plugins. For example, you can execute `miv each pwd` or `miv each git gc`.|
|`help`|Shows the help of `miv`.|
|`version`|Shows the version of `miv`.|

Commands to execute when you want to

|do what|command|
|:--|:--|
|install a new plugin|`miv edit`, update the configuration file, save, exit the editor and `miv install`|
|update the installed plugins but skip outdated plugins|`miv update`|
|update all the installed plugins|`miv update!`|
|update specific plugins|`miv update [plugin1] [plugin2]..`|
|uninstall a plugin|`miv edit`, remove the related configurations, `miv generate` (and `miv clean` if you want)|
|list all the plugins|`miv list`|
|count the number of plugins|<code>miv list &vert; wc -l</code>|
|change the current working directory to a plugin directory|`cd "$(miv path [plugin])"`|
|want a help|`miv help`|

## Plugin configuration
### Loading triggers for the plugin
|key|type|description|
|:--|:--|:--|
|`filetype`|<code>string &vert; string[]</code>|load the plugin on setting the filetype|
|`command`|<code>string &vert; string[]</code>|load the plugin on invoking the command|
|`function`|<code>string &vert; string[]</code>|load the plugin on calling a function matching the value in regex|
|`mapping`|<code>string &vert; string[]</code>|load the plugin on the mapping|
|`mapmode`|<code>'n' &vert; 'v' &vert; 'x' &vert; 's' &vert; 'i' &vert; 'c' &vert; 'l' &vert; 'o' &vert; 't'</code>|specify the `map-modes` for the `mapping` configuration|
|`cmdline`|<code>':' &vert; '/' &vert; '?' &vert; '@'</code>|the command-line character to load the plugin|
|`insert`|`boolean`|load the plugin on entering the insert mode for the first time|

### Configurations for the plugin
|key|type|description|
|:--|:--|:--|
|`script`|`string`|script run on startup, specify some configurations or mappings to load the plugin|
|`after`|`string`|script run after the plugin is loaded|
|`before`|`string`|script run just before the plugin is loaded|
|`mapleader`|`string`|the `mapleader` (`<Leader>`) for the `script`|

### Dependency configurations
|key|type|description|
|:--|:--|:--|
|`dependon`|<code>string &vert; string[]</code>|plugins on which the plugin depends; they are loaded just before the plugin is loaded|
|`dependedby`|<code>string &vert; string[]</code>|(deprecated in favor of `loadafter`) plugins loaded just after the plugin is loaded|
|`loadbefore`|<code>string &vert; string[]</code>|indicates lazy loading, the plugin is loaded just before any of the configured plugins|
|`loadafter`|<code>string &vert; string[]</code>|indicates lazy loading, the plugin is loaded just after any of the configured plugins|

### Other miscellaneous configurations
|key|type|description|
|:--|:--|:--|
|`enable`|`string`|enable the plugin when the expression (in Vim script) is truthy|
|`submodule`|`boolean`|pull the submodules of the repository|
|`build`|`string`|build shell script to execute after installing and updating|
|`sync`|`boolean`|skip pulling the repository if the value is `false`|

## Author
itchyny (https://github.com/itchyny)

## License
This software is released under the MIT License, see LICENSE.
