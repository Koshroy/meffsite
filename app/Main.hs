{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class
import Lib
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static hiding (static)
import System.Directory
import System.FilePath
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
        do
          when (not (prod config)) $
            compileAllSCSS (T.unpack $ staticDir config) "/home/torifuda/tmp/newtmp"
          runSpock (fromIntegral $ port config) $ spockT id $
            do
              middleware $ myStaticPolicy config
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
  -- "def" is the default scss compiler option
  compile <- compileFile "/home/torifuda/src/spock-hello-world/app/test.scss" def
  case compile of
    Left err -> errorMessage err
    Right s -> return s

compileAllSCSS :: FilePath -> FilePath -> IO ()
compileAllSCSS staticPath destPath = do
  contents <- getDirectoryContents staticPath
  let scssNames = filter (\p -> (takeExtension p) == ".scss") contents
  let scssPaths = fmap (\p -> staticPath </> p) scssNames
  mapM_ ((flip compileScssFile) destPath) scssPaths
  
  
compileScssFile :: FilePath -> FilePath -> IO ()
compileScssFile filePath destPath = do
  putStrLn "---------"
  putStrLn $ "Compiling scss file " ++ filePath ++ " to " ++ destPath
  compile <- compileFile filePath def
  case compile of
    Left err -> do
      msg <- errorMessage err
      putStrLn msg
    Right s -> do
      let targetFname = takeFileName $ replaceExtension filePath ".css"
      let targetPath = destPath </> targetFname
      writeFile targetPath s

myStaticPolicy :: SiteConfig -> Middleware
myStaticPolicy config =
  let
    isProd = prod config
    staticDirT = T.unpack $ staticDir config
    scssPrefixT = T.unpack $ scssPrefix config
    hasScssPolicy = hasSuffix ".scss"
    stripPrefixPolicy = policy $
      (\s -> fmap T.unpack (T.stripPrefix (T.pack (scssPrefixT ++ "/")) (T.pack s)))
  in
    case isProd of
      True -> id
      False -> 
        staticPolicy $
        (hasPrefix scssPrefixT) >->
        stripPrefixPolicy >->
        (addBase staticDirT)
