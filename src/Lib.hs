{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( compileAllSCSS,
      compileScssFile,
      getSiteConfig,
      htmlDest,
      SiteConfig,
      port,
      prod,
      renderTemplate,
      staticDir,
      staticPrefix,
      scssPrefix,
      scssCompileDir,

      System.Directory.doesFileExist
    ) where

import qualified Data.Map as Map
import Data.Yaml
import GHC.Generics
import System.Directory
import System.FilePath
import Text.Mustache
import qualified Text.Parsec.Error as P (ParseError)
import Text.Sass

import qualified Data.Text as T

data SiteConfig = SiteConfig {
  port :: Integer
  , staticDir :: T.Text
  , scssCompileDir :: T.Text
  , staticPrefix :: T.Text
  , scssPrefix :: T.Text
  , htmlDir :: T.Text  
  , prod :: Bool
  } deriving (Generic, Show)

instance FromJSON SiteConfig


getSiteConfig :: FilePath -> IO (Either ParseException SiteConfig)
getSiteConfig = decodeFileEither


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


htmlDest :: SiteConfig -> String -> FilePath
htmlDest config htmlPath = (T.unpack $ htmlDir config) </> htmlPath


renderTemplate :: SiteConfig -> String -> Map.Map String String -> IO (Either P.ParseError T.Text)
renderTemplate config templateName vars = do
  eCompiled <- automaticCompile [T.unpack $ htmlDir config] templateName
  let retval = do
        compiled <- eCompiled
        return $ substitute compiled vars
  return retval
