{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Map as Map
import Lib
import Network.HTTP.Types.Status
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static hiding (static)
import Web.Spock

import qualified Data.Text as T
import qualified Network.Wai.Middleware.Static as MS

main =
  do
    eitherConfig <- getSiteConfig "config.yml"
    case eitherConfig of
      Left pe -> putStrLn $ show pe
      Right config ->
        do
          when (not (prod config)) $
            compileAllSCSS (T.unpack $ staticDir config) (T.unpack $ scssCompileDir config)
          runSpock (fromIntegral $ port config) $ spockT id $
            do
              middleware $ myStaticPolicy config
              get ("echo" <//> var) $ \something ->
                text $ T.concat ["Echo: ", something]
              get ("test" <//> var) $ \testVar ->
                text $ T.concat ["My test: ", testVar]
              get ("html" <//> var) $ \testVar ->
                do
                  pathExists <- liftIO $ doesFileExist $ htmlDest config testVar
                  template <- liftIO $ renderTemplate config testVar $ Map.empty
                  case template of
                    Left e -> do
                      setStatus $ if pathExists then status500 else status404
                      text $ T.pack $ show e
                    Right t -> html t


myStaticPolicy :: SiteConfig -> Middleware
myStaticPolicy config =
  let
    isProd = prod config
    staticDirT = T.unpack $ staticDir config
    scssCompileDirT = T.unpack $ scssCompileDir config
    staticPrefixT = T.unpack $ staticPrefix config
    scssPrefixT = T.unpack $ scssPrefix config
    hasScssPolicy = hasSuffix ".scss"
    stripPrefixPolicy prefix = policy $
      (\s -> fmap T.unpack (T.stripPrefix (T.pack (prefix ++ "/")) (T.pack s)))
  in
    case isProd of
      True -> id
      False ->
        staticPolicy $
        (hasPrefix staticPrefixT >-> stripPrefixPolicy staticPrefixT >-> addBase staticDirT)
        <|>
        (hasPrefix scssPrefixT >-> stripPrefixPolicy scssPrefixT >-> addBase scssCompileDirT)

