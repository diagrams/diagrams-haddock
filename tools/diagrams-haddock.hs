{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}
module Main where

import           Control.Monad                      (forM_, when)
import           Data.List                          (intercalate)
import           Diagrams.Haddock
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment                 (getEnvironment)
import           System.FilePath                    ((<.>), (</>))

import           Distribution.ModuleName            (toFilePath)
import qualified Distribution.PackageDescription    as P
import           Distribution.Simple.Configure      (maybeGetPersistBuildConfig)
import           Distribution.Simple.LocalBuildInfo (localPkgDescr)
import           Distribution.Simple.Utils          (cabalVersion)

import           Language.Preprocessor.Cpphs

import           Paths_diagrams_haddock             (version)

------------------------------------------------------------
-- This is officially an awful mess.
--
-- In Cabal < 2.0, it used the Version type from Data.Version.
--
-- As of Cabal 2.0, Cabal switched to its own Version type,
-- and re-exported a 'showVersion' function.
--
-- As of Cabal 2.2, showVersion became deprecated and a 'prettyShow'
-- function was added.
--
-- We need to show both Cabal versions and Data.Version
-- versions, hence the CPP mess below.

-- 1. We definitely need Data.Version. Import it qualified.
import qualified Data.Version                       as V

-- 2. Import the proper 'Version' type to refer to Cabal versions.
#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Types.Version         (Version)
#else
import           Data.Version                       (Version)
#endif

-- 3. Import the proper function for showing Cabal versions.
#if   MIN_VERSION_Cabal(2,2,0)
import           Distribution.Pretty                (prettyShow)
#elif MIN_VERSION_Cabal(2,0,0)
import qualified Distribution.Types.Version         as DV
#endif

-- 4. Define a function for showing Cabal versions.
showCabalVersion :: Version -> String
#if   MIN_VERSION_Cabal(2,2,0)
showCabalVersion = prettyShow
#elif MIN_VERSION_Cabal(2,0,0)
showCabalVersion = DV.showVersion
#else
showCabalVersion = V.showVersion
#endif

------------------------------------------------------------
------------------------------------------------------------

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
      &= name "c"

  , outputDir
    = "diagrams"
      &= typDir
      &= help "Directory to output compiled diagrams (default: diagrams)"

  , distDir
    = def
      &= typDir
      &= help "Directory in which to look for setup-config (default: dist)"
      &= name "d"

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
       [ "diagrams-haddock v" ++ V.showVersion version ++ ", (c) 2013-2016 diagrams-haddock team (see LICENSE)"
       , "compiled using version " ++ showCabalVersion cabalVersion ++ " of the Cabal library"
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
                                -- active hsenv with default name
                              (Nothing, Just "")    -> "dist"
                                -- active hsenv with custom name
                              (Nothing, Just hsenv) -> "dist_" ++ hsenv

                              -- otherwise, default to "dist"
                              _                     -> "dist"

  mlbi <- maybeGetPersistBuildConfig fullDistDir
  case mlbi of
    Nothing -> putStrLn $ unlines [
        "diagrams-haddock: No appropriate setup-config found in " ++ fullDistDir
      , "Either it does not exist or it is in the wrong format."
      , "* You may need to run 'cabal configure' first."
      , "* Make sure that the version of Cabal used to compile"
      , "  diagrams-haddock (" ++ showCabalVersion cabalVersion ++ ") matches the version used"
      , "  by the cabal tool."
      , "* Use the -d option if you want diagrams-haddock to look in"
      , "  a different dist directory."
      ]
    Just lbi -> do
      let mlib = P.library . localPkgDescr $ lbi
      case mlib of
        Nothing -> return ()
        Just lib -> do
          let buildInfo = P.libBuildInfo lib
              srcDirs   = P.hsSourceDirs buildInfo
              incls     = P.includeDirs buildInfo
              defns     = P.cppOptions buildInfo
              opts' = opts
                    { includeDirs = includeDirs opts ++ incls
                    }
              cabalDefines = parseCabalDefines defns
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
tryProcessFile opts defns dir srcDirs fileBase = do
  let files = [ dir </> srcDir </> fileBase <.> ext
              | srcDir <- srcDirs
              , ext <- ["hs", "lhs"]
              ]
  forM_ files $ \f -> do
    e <- doesFileExist f
    when e $ processFile opts defns f

-- | Process a single file with diagrams-haddock.
processFile :: DiagramsHaddock -> [(String,String)] -> FilePath -> IO ()
processFile opts defns file = do
    errs <- processHaddockDiagrams' cpphsOpts (quiet opts) (dataURIs opts) (cacheDir opts) (outputDir opts) file
    case errs of
      [] -> return ()
      _  -> putStrLn $ intercalate "\n" errs
  where
    cpphsOpts = defaultCpphsOptions
              { includes = includeDirs opts
              , defines  = map (,"") (cppDefines opts) ++ defns
              , boolopts = defaultBoolOptions { hashline = False }
              }
