{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Lib
import Text.Sass
import Web.Spock
import Web.Spock.StaticMiddleware

import qualified Data.Text as T

main =
      runSpock 3000 $ spockT id $
          do
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
