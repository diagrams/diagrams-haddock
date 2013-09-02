{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}
module Main where

import           Control.Monad                      (forM_, when)
import           Data.List                          (intercalate)
import           Data.Version                       (showVersion)
import           Diagrams.Haddock
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment                 (getEnvironment)
import           System.FilePath                    ((<.>), (</>))

import           Distribution.ModuleName            (toFilePath)
import qualified Distribution.PackageDescription    as P
import           Distribution.Simple.Configure      (maybeGetPersistBuildConfig)
import           Distribution.Simple.LocalBuildInfo (localPkgDescr)

import           Language.Preprocessor.Cpphs

import           Paths_diagrams_haddock             (version)

data DiagramsHaddock
  = DiagramsHaddock
  { quiet       :: Bool
  , dataURIs    :: Bool
  , cacheDir    :: FilePath
  , outputDir   :: FilePath
  , distDir     :: Maybe FilePath
  , includeDirs :: [FilePath]
  , cppDefines  :: [String]
  , targets     :: [FilePath]
  }
  deriving (Show, Typeable, Data)

diagramsHaddockOpts :: DiagramsHaddock
diagramsHaddockOpts
  = DiagramsHaddock
  { quiet = False
    &= help "Suppress normal logging output"

  , dataURIs = False
    &= help "Generate embedded data URIs instead of external SVGs"
    &= name "dataURIs"

  , cacheDir
    = ".diagrams-cache"
      &= typDir
      &= help "Directory for storing cached diagrams (default: .diagrams-cache)"

  , outputDir
    = "diagrams"
      &= typDir
      &= help "Directory to output compiled diagrams (default: diagrams)"

  , distDir
    = def
      &= typDir
      &= help "Directory in which to look for setup-config (default: dist)"

  , includeDirs
    = []
      &= typDir
      &= help "Include directory for CPP includes"

  , cppDefines
    = []
      &= typ "NAME"
      &= help "Preprocessor defines for CPP pass"

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
      (_,True) -> processFile opts [] targ
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
  mhsenv <- getHsenv
  let fullDistDir = dir </> case (distDir opts, mhsenv) of
                              -- command-line arg trumps all
                              (Just d, _)           -> d

                              -- next, look for active hsenv
                              (Nothing, Just hsenv) -> "dist_" ++ hsenv

                              -- otherwise, default to "dist"
                              _                     -> "dist"

  mlbi <- maybeGetPersistBuildConfig fullDistDir
  case mlbi of
    Nothing -> putStrLn $
      "No setup-config found in " ++ fullDistDir ++ ", please run 'cabal configure' first."
    Just lbi -> do
      let mlib = P.library . localPkgDescr $ lbi
      case mlib of
        Nothing -> return ()
        Just lib -> do
          let buildInfo = P.libBuildInfo lib
              srcDirs   = P.hsSourceDirs buildInfo
              includes  = P.includeDirs buildInfo
              defines   = P.cppOptions buildInfo
              opts' = opts
                    { includeDirs = includeDirs opts ++ includes
                    }
              cabalDefines = parseCabalDefines defines
          mapM_ (tryProcessFile opts' cabalDefines dir srcDirs) . map toFilePath . P.exposedModules $ lib

getHsenv :: IO (Maybe String)
getHsenv = do
  env <- getEnvironment
  return $ lookup "HSENV_NAME" env

-- | Use @cpphs@'s options parser to handle the options from cabal.
parseCabalDefines :: [String] -> [(String,String)]
parseCabalDefines = either (const []) defines . parseOptions


-- | Try to find and process a file corresponding to a module exported
--   from a library.
tryProcessFile
  :: DiagramsHaddock   -- ^ options
  -> [(String,String)] -- ^ additional defines
  -> FilePath          -- ^ base directory
  -> [FilePath]        -- ^ haskell-src-dirs
  -> FilePath          -- ^ name of the module to look for, in \"A/B/C\" form
  -> IO ()
tryProcessFile opts defines dir srcDirs fileBase = do
  let files = [ dir </> srcDir </> fileBase <.> ext
              | srcDir <- srcDirs
              , ext <- ["hs", "lhs"]
              ]
  forM_ files $ \f -> do
    e <- doesFileExist f
    when e $ processFile opts defines f

-- | Process a single file with diagrams-haddock.
processFile :: DiagramsHaddock -> [(String,String)] -> FilePath -> IO ()
processFile opts defines file = do
    errs <- processHaddockDiagrams' cpphsOpts (quiet opts) (dataURIs opts) (cacheDir opts) (outputDir opts) file
    case errs of
      [] -> return ()
      _  -> putStrLn $ intercalate "\n" errs
  where
    cpphsOpts = defaultCpphsOptions
              { includes = includeDirs opts
              , defines  = map (,"") (cppDefines opts) ++ defines
              , boolopts = defaultBoolOptions { hashline = False }
              }
