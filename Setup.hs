import Data.Maybe
import Data.Monoid
import System.Directory
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), InstallDirs(..), absoluteInstallDirs)

main =
  defaultMainWithHooks hooks
  where
    hooks =
      simpleUserHooks {
        confHook = theConfHook
      }

theConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
theConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  make (fromFlag (configVerbosity flags))
  updateLocalBuildInfo localBuildInfo

make :: Verbosity -> IO ()
make verbosity =
  do
    rawSystemExit verbosity "mkdir" ["-p", "dist/build"]
    rawSystemExit verbosity "cp" ["-a", "foreign/libpg_query", "dist/build/libpg_query"]
    rawSystemExit verbosity "env" ["CFLAGS=-D_LIB", "make", "--directory=dist/build/libpg_query", "libpg_query.a"]

updateLocalBuildInfo :: LocalBuildInfo -> IO LocalBuildInfo
updateLocalBuildInfo =
  def
  where
    def x =
      do
        path <- getCurrentDirectory
        return (addExtraLibDir (path <> "/dist/build/libpg_query") x)
      where
    updatePackageDescription fn localBuildInfo =
      localBuildInfo { localPkgDescr = fn (localPkgDescr localBuildInfo) }
    updateLibrary fn =
      updatePackageDescription (\x -> x { library = fmap fn (library x) })
    updateLibBuildInfo fn =
      updateLibrary (\x -> x { libBuildInfo = fn (libBuildInfo x) })
    updateExtraLibDirs fn =
      updateLibBuildInfo (\x -> x { extraLibDirs = fn (extraLibDirs x) })
    addExtraLibDir path =
      updateExtraLibDirs (\x -> path : x)