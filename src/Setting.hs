{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Setting where

import Data.Aeson
import Data.Function (on)
import qualified Data.HashMap.Lazy as HM
import Data.List (sortBy)
import Data.Text (Text, lines)
import Prelude hiding (lines)

import Plugin

data Setting =
     Setting { plugins  :: [Plugin]
             , filetype :: HM.HashMap Text [Text]
             , syntax   :: HM.HashMap Text [Text]
             , before   :: [Text]
             , after    :: [Text]
     } deriving Eq

instance FromJSON Setting where
  parseJSON = withObject "setting" $ \o -> do
    plugins <- pluginHashMapToList <$> o .:? "plugin" .!= HM.empty
    filetype <- HM.map lines <$> o .:? "filetype" .!= HM.empty
    syntax <- HM.map lines <$> o .:? "syntax" .!= HM.empty
    before <- lines <$> o .:? "before" .!= ""
    after <- lines <$> o .:? "after" .!= ""
    return Setting {..}
      where pluginHashMapToList = sortWith name . HM.foldlWithKey' (\a k v -> v { name = k } : a) []

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare
