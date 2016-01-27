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
             , filetypeScript :: HM.HashMap Text [Text]
             , beforeScript   :: [Text]
             , afterScript    :: [Text]
             , filetypeDetect :: HM.HashMap Text Text
     } deriving (Eq, Show)

instance FromJSON Setting where
  parseJSON = withObject "setting" $ \o -> do
    plugin <- fmap (sortWith P.name . HM.foldlWithKey' (\a k v -> v { P.name = k } : a) []) (o .:? "plugin" .!= HM.empty)
    filetypeScript <- HM.map lines <$> o .:? "filetypeScript" .!= HM.empty
    beforeScript <- lines <$> o .:? "beforeScript" .!= ""
    afterScript <- lines <$> o .:? "afterScript" .!= ""
    filetypeDetect <- o .:? "filetypeDetect" .!= HM.empty
    return Setting {..}

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare
