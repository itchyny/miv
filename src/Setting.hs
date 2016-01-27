{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Setting where

import Data.Aeson
import Data.Function (on)
import qualified Data.HashMap.Lazy as HM
import Data.List (sortBy)
import Data.Text (Text, lines)
import Prelude hiding (lines)

import qualified Plugin as P

data Setting =
     Setting { plugin         :: [P.Plugin]
             , filetype       :: HM.HashMap Text [Text]
             , before         :: [Text]
             , after          :: [Text]
             , filetypeDetect :: HM.HashMap Text Text
     } deriving (Eq, Show)

instance FromJSON Setting where
  parseJSON = withObject "setting" $ \o -> do
    plugin <- pluginHashMapToList <$> o .:? "plugin" .!= HM.empty
    filetype <- HM.map lines <$> o .:? "filetype" .!= HM.empty
    before <- lines <$> o .:? "before" .!= ""
    after <- lines <$> o .:? "after" .!= ""
    filetypeDetect <- o .:? "filetypeDetect" .!= HM.empty
    return Setting {..}
      where pluginHashMapToList = sortWith P.name . HM.foldlWithKey' (\a k v -> v { P.name = k } : a) []

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare
