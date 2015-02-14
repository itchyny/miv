{-# LANGUAGE OverloadedStrings #-}
module Setting where

import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Text as T

import qualified Plugin as P
import qualified SettingI as SI

data Setting =
     Setting { plugin         :: [P.Plugin]
             , filetypeScript :: HM.HashMap T.Text [T.Text]
             , beforeScript   :: [T.Text]
             , afterScript    :: [T.Text]
             , filetypeDetect :: HM.HashMap T.Text T.Text
     } deriving (Eq, Show)

decodeSetting :: FilePath -> IO (Maybe Setting)
decodeSetting = fmap (fmap toSetting) . Y.decodeFile

toSetting :: SI.SettingI -> Setting
toSetting s
  = Setting { plugin = sortWith P.name $ maybe [] (HM.foldlWithKey' (\a k v -> P.toPlugin k v : a) []) (SI.plugin s)
            , filetypeScript = maybe HM.empty (HM.map T.lines) (SI.filetypeScript s)
            , beforeScript = T.lines $ fromMaybe "" (SI.beforeScript s)
            , afterScript = T.lines $ fromMaybe "" (SI.afterScript s)
            , filetypeDetect = fromMaybe HM.empty (SI.filetypeDetect s)
  }

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith = sortBy . on compare
