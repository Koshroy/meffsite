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
          middleware myStaticPolicy
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


myStaticPolicy :: Middleware
myStaticPolicy = staticPolicy $
  (hasPrefix "static") >->
  (policy $ (\s -> fmap T.unpack (T.stripPrefix "static/" $ T.pack s))) >->
  (addBase "/home/torifuda/tmp")
