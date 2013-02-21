import Distribution.Simple
import Distribution.Simple.Setup   ( haddockDistPref, Flag(..))
import Distribution.Verbosity      ( normal )
import Distribution.Simple.Utils   ( copyFiles )
import Distribution.Text           ( display )
import System.FilePath ((</>))
import System.Directory

-- Ugly hack, logic copied from Distribution.Simple.Haddock
haddockOutputDir flags pkg = destDir
   where
     baseDir = case haddockDistPref flags of
                      NoFlag -> "."
                      Flag x -> x
     destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

main = defaultMainWithHooks simpleUserHooks {
    postHaddock = \args flags pkg lbi -> do
        copyFiles normal (haddockOutputDir flags pkg) [("diagrams","greenCircle.svg")]
        postHaddock simpleUserHooks args flags pkg lbi
  }
