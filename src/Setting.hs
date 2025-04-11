module Setting where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text, lines)
import Data.YAML
import Prelude hiding (lines)

import Plugin

data Setting =
  Setting {
    plugins  :: [Plugin],
    filetype :: M.Map Text [Text],
    syntax   :: M.Map Text [Text],
    before   :: [Text],
    after    :: [Text]
  } deriving Eq

instance FromYAML Setting where
  parseYAML = withMap "!!map" \o -> do
    plugins <- pluginMapToList <$> o .:? "plugin" .!= M.empty
    filetype <- M.map lines <$> o .:? "filetype" .!= M.empty
    syntax <- M.map lines <$> o .:? "syntax" .!= M.empty
    before <- lines <$> o .:? "before" .!= ""
    after <- lines <$> o .:? "after" .!= ""
    return Setting {..}
      where pluginMapToList = sortOn (.name) .
              M.foldlWithKey' (\a k v -> v { name = k } : a) []
