# miv [![Build Status](https://travis-ci.org/itchyny/miv.png?branch=master)](https://travis-ci.org/itchyny/miv)

## Mac OS X installation

```
brew install haskell-platform
git clone git://github.com/itchyny/miv
cd miv
cabal install miv.cabal
```

## User guide

1. create $HOME/.vimrc.yaml for miv
2. $HOME/.cabal/bin/miv install
3. $HOME/.cabal/bin/miv generate
4. append ```set runtimepath+=$HOME/.vim/miv/miv``` to your vimrc

