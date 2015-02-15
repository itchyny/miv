{-# LANGUAGE OverloadedStrings #-}
module Setting where

import qualified Data.Yaml as Y
import Prelude hiding (lines)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)
import Data.Text (Text, lines)

import qualified Plugin as P
import qualified SettingI as SI

data Setting =
     Setting { plugin         :: [P.Plugin]
             , filetypeScript :: HM.HashMap Text [Text]
             , beforeScript   :: [Text]
             , afterScript    :: [Text]
             , filetypeDetect :: HM.HashMap Text Text
     } deriving (Eq)

decodeSetting :: FilePath -> IO (Maybe Setting)
decodeSetting = fmap (fmap toSetting) . Y.decodeFile

toSetting :: SI.SettingI -> Setting
toSetting s
  = Setting { plugin = sortWith P.name $ maybe [] (HM.foldlWithKey' (\a k v -> P.toPlugin k v : a) []) (SI.plugin s)
            , filetypeScript = maybe HM.empty (HM.map lines) (SI.filetypeScript s)
            , beforeScript = lines $ fromMaybe "" (SI.beforeScript s)
            , afterScript = lines $ fromMaybe "" (SI.afterScript s)
            , filetypeDetect = fromMaybe HM.empty (SI.filetypeDetect s)
  }

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare
