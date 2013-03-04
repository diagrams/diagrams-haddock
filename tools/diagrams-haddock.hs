{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad          (forM_, when)
import           Data.List              (intercalate)
import           Data.Version           (showVersion)
import           Diagrams.Haddock
import           System.Console.CmdArgs
import           System.Directory

import           Paths_diagrams_haddock (version)

data DiagramsHaddock
  = DiagramsHaddock
  { cacheDir  :: FilePath
  , outputDir :: FilePath
  , targets   :: [FilePath]
  }
  deriving (Show, Typeable, Data)

diagramsHaddockOpts :: DiagramsHaddock
diagramsHaddockOpts
  = DiagramsHaddock
  { cacheDir
    = ".diagrams-cache"
      &= typDir
      &= help "Directory for storing cached diagrams (default: .diagrams-cache)"

  , outputDir
    = "diagrams"
      &= typDir
      &= help "Directory to output compiled diagrams (default: diagrams)"

  , targets
    = def &= args &= typFile
  }
  &= program "diagrams-haddock"
  &= summary (unlines
       [ "diagrams-haddock v" ++ showVersion version ++ ", (c) 2013 Brent Yorgey"
       , ""
       , "Compile inline diagrams code in Haddock documentation."
       , ""
       , "Pass diagrams-haddock the names of files to be processed, and/or"
       , "the names of directories containing Cabal packages.  If no arguments"
       , "are given it assumes the current directory is a Cabal package."
       , ""
       , "For more detailed help, including details of how to create source files"
       , "for diagrams-haddock to process, consult the README at"
       , ""
       , "    http://github.com/diagrams/diagrams-haddock/"
       ])

main :: IO ()
main = do
  opts <- cmdArgs diagramsHaddockOpts
  forM_ (targets opts) $ \targ -> do
    d <- doesDirectoryExist targ
    f <- doesFileExist targ
    case (d,f) of
      (True,_) -> processCabalPackage opts targ
      (_,True) -> processFile opts targ
      _        -> targetError targ
  when (null (targets opts)) $ processCabalPackage opts "."

targetError :: FilePath -> IO ()
targetError targ = putStrLn $ "Warning: target " ++ targ ++ " does not exist, ignoring."

processCabalPackage :: DiagramsHaddock -> FilePath -> IO ()
processCabalPackage = undefined

processFile :: DiagramsHaddock -> FilePath -> IO ()
processFile opts file = do
  errs <- processHaddockDiagrams (cacheDir opts) (outputDir opts) file
  case errs of
    [] -> return ()
    _  -> putStrLn $ intercalate "\n" errs
