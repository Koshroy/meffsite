{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Lib
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static hiding (static)
import Text.Sass
import Web.Spock

import qualified Data.Text as T
import qualified Network.Wai.Middleware.Static as MS

main =
  do
    eitherConfig <- getSiteConfig "config.yml"
    case eitherConfig of
      Left pe -> putStrLn $ show pe
      Right config ->
        runSpock (fromIntegral $ port config) $ spockT id $
        do
          middleware $
            myStaticPolicy (T.unpack $ scssPrefix config) (T.unpack $ staticDir config)
          get ("echo" <//> var) $ \something ->
            text $ T.concat ["Echo: ", something]
          get (static "test") $
            do
              str <- liftIO scssTest
              text $ T.pack str
          get ("test" <//> var) $ \testVar ->
            text $ T.concat ["My test: ", testVar]


scssTest :: IO String
scssTest = do
  compile <- compileFile "/home/torifuda/src/spock-hello-world/app/test.scss" def
  case compile of
    Left err -> errorMessage err
    Right s -> return s


myStaticPolicy :: String -> String -> Middleware
myStaticPolicy scssPrefix staticDir = staticPolicy $
  (hasPrefix scssPrefix) >->
  (policy $ (\s -> fmap T.unpack (T.stripPrefix (T.pack (scssPrefix ++ "/")) $ T.pack s))) >->
  (addBase staticDir)
