{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( getSiteConfig,
      SiteConfig,
      port,
      staticDir,
      scssPrefix
    ) where

import Data.Yaml
import GHC.Generics

import qualified Data.Text as T

data SiteConfig = SiteConfig {
  port :: Integer
  , staticDir :: T.Text
  , scssPrefix :: T.Text
  } deriving (Generic, Show)

instance FromJSON SiteConfig

getSiteConfig :: FilePath -> IO (Either ParseException SiteConfig)
getSiteConfig = decodeFileEither
