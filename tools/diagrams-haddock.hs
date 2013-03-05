{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad                      (forM_, when)
import           Data.List                          (intercalate)
import           Data.Version                       (showVersion)
import           Diagrams.Haddock
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath                    ((<.>), (</>))

import           Distribution.ModuleName            (toFilePath)
import qualified Distribution.PackageDescription    as P
import           Distribution.Simple.Configure      (maybeGetPersistBuildConfig)
import           Distribution.Simple.LocalBuildInfo (localPkgDescr)

import           Paths_diagrams_haddock             (version)

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
       [ "diagrams-haddock v" ++ showVersion version ++ ", (c) 2013 diagrams-haddock team (see LICENSE)"
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

-- | Process all the files in a Cabal package.  At the moment, only
--   looks at files corresponding to exported modules from a library.
--   In other words, it does not consider the source for executables
--   or any unexported modules.
processCabalPackage :: DiagramsHaddock -> FilePath -> IO ()
processCabalPackage opts dir = do
  mlbi <- maybeGetPersistBuildConfig distDir
  case mlbi of
    Nothing -> putStrLn $
      "No setup-config found in " ++ distDir ++ ", please run 'cabal configure' first."
    Just lbi -> do
      let mlib = P.library . localPkgDescr $ lbi
      case mlib of
        Nothing -> return ()
        Just lib -> do
          let srcDirs = P.hsSourceDirs . P.libBuildInfo $ lib
          mapM_ (tryProcessFile opts dir srcDirs) . map toFilePath . P.exposedModules $ lib

  where distDir = dir </> "dist"

-- | Try to find and process a file corresponding to a module exported
--   from a library.
tryProcessFile
  :: DiagramsHaddock   -- ^ options
  -> FilePath          -- ^ base directory
  -> [FilePath]        -- ^ haskell-src-dirs
  -> FilePath          -- ^ name of the module to look for, in \"A/B/C\" form
  -> IO ()
tryProcessFile opts dir srcDirs fileBase = do
  let files = [ dir </> srcDir </> fileBase <.> ext
              | srcDir <- srcDirs
              , ext <- ["hs", "lhs"]
              ]
  forM_ files $ \f -> do
    e <- doesFileExist f
    when e $ processFile opts f

-- | Process a single file with diagrams-haddock.
processFile :: DiagramsHaddock -> FilePath -> IO ()
processFile opts file = do
  errs <- processHaddockDiagrams (cacheDir opts) (outputDir opts) file
  case errs of
    [] -> return ()
    _  -> putStrLn $ intercalate "\n" errs
