{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module: Distribution.Simple.Toolkit
Copyright: (c) 2017 Shao Cheng
License: BSD3
Maintainer: astrohavoc@gmail.com
Stability: alpha
Portability: non-portable

This module provides helper functions for writing custom @Setup.hs@ scripts.
-}

module Distribution.Simple.Toolkit (
  -- * Writing build metadata in @Setup.hs@
    userHooksWithBuildInfo
  , simpleUserHooksWithBuildInfo
  , defaultMainWithBuildInfo
  -- * Retrieving build metadata via Template Haskell
  , packageDescriptionQ
  , packageDescriptionTypedQ
  , localBuildInfoQ
  , localBuildInfoTypedQ
  -- * Convenient functions for working with build metadata
  , getComponentInstallDirs
  , getComponentBuildInfo
  , getGHCLibDir
  , runLBIProgram
  , getLBIProgramOutput
  -- * Convenient functions for working with GHC API
  , getGHCPackageDBFlags
  -- * Extra 'Program's
  , cmakeProgram
  , makeProgram
  , ninjaProgram
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Map
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
#if !MIN_VERSION_Cabal(2,0,0)
import qualified Distribution.Simple.InstallDirs as InstallDirs
#endif
import Distribution.System
import Distribution.PackageDescription
import Distribution.Verbosity
import DynFlags
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

{-|
Attach a post-configure action to a 'UserHooks' which serializes 'PackageDescription' to @.pkg_descr.buildinfo@ and 'LocalBuildInfo' to @.lbi.buildinfo@.
They should be added to your project's @.gitignore@ file.
Don't forget to edit the <https://cabal.readthedocs.io/en/latest/developing-packages.html#custom-setup-scripts custom-setup> stanza of your project's @.cabal@ file and add @cabal-toolkit@ to the dependencies.
-}
userHooksWithBuildInfo :: UserHooks -> UserHooks
userHooksWithBuildInfo h =
  h
  { postConf =
      \args flags pkg_descr lbi -> do
        encodeFile ".pkg_descr.buildinfo" pkg_descr
        encodeFile ".lbi.buildinfo" lbi
        postConf h args flags pkg_descr lbi
  }


simpleUserHooksWithBuildInfo :: UserHooks
simpleUserHooksWithBuildInfo = userHooksWithBuildInfo simpleUserHooks

defaultMainWithBuildInfo :: IO ()
defaultMainWithBuildInfo = defaultMainWithHooks simpleUserHooksWithBuildInfo

syringe :: FilePath -> Q Type -> Q Exp
syringe p t = do
  buf <- runIO $ LBS.readFile p
  [|unsafePerformIO $ do
      bs <-
        BS.unsafePackAddressLen
          $(lift $ LBS.length buf)
          $(pure $ LitE $ StringPrimL $ LBS.unpack buf)
      pure ((decode $ LBS.fromStrict bs) :: $(t))|]

{-|
The Template Haskell splice to retrieve 'PackageDescription'.
-}
packageDescriptionQ :: Q Exp
packageDescriptionQ = syringe ".pkg_descr.buildinfo" [t|PackageDescription|]

packageDescriptionTypedQ :: Q (TExp PackageDescription)
packageDescriptionTypedQ = unsafeTExpCoerce packageDescriptionQ

{-|
The Template Haskell splice to retrieve 'LocalBuildInfo'.
-}
localBuildInfoQ :: Q Exp
localBuildInfoQ = syringe ".lbi.buildinfo" [t|LocalBuildInfo|]

localBuildInfoTypedQ :: Q (TExp LocalBuildInfo)
localBuildInfoTypedQ = unsafeTExpCoerce localBuildInfoQ

#if !MIN_VERSION_Cabal(2,0,0)
-- | As defined in @Cabal-2.0.0.2@. See 'Distribution.Simple.InstallDirs.absoluteInstallDirs'.
absoluteComponentInstallDirs 
  :: PackageDescription -> LocalBuildInfo
  -> UnitId
  -> CopyDest
  -> InstallDirs FilePath
absoluteComponentInstallDirs pkg lbi uid copydest =
  InstallDirs.absoluteInstallDirs
  (packageId pkg)
  uid
  (Distribution.Simple.compilerInfo (compiler lbi))
  copydest
  (hostPlatform lbi)
  (installDirTemplates lbi)
#endif

{-|
Retrieve the 'InstallDirs' corresponding to a 'ComponentName', assuming that component does exist and is unique.
-}
getComponentInstallDirs ::
     PackageDescription
  -> LocalBuildInfo
  -> ComponentName
  -> InstallDirs FilePath
getComponentInstallDirs pkg_descr lbi k =
  absoluteComponentInstallDirs
    pkg_descr
    lbi
    (componentUnitId $ getComponentLocalBuildInfo lbi k)
    NoCopyDest

{-|
Retrieve the 'BuildInfo' corresponding to a 'ComponentName', assuming that component does exist and is unique.
-}
getComponentBuildInfo :: PackageDescription -> ComponentName -> BuildInfo
getComponentBuildInfo pkg_descr k =
  componentBuildInfo $ getComponent pkg_descr k

{-|
Equivalent to what you get from @ghc --print-libdir@.
-}
getGHCLibDir :: LocalBuildInfo -> FilePath
getGHCLibDir lbi = compilerProperties (compiler lbi) ! "LibDir"

{-|
Run a 'Program' with default 'Verbosity'.
-}
runLBIProgram :: LocalBuildInfo -> Program -> [ProgArg] -> IO ()
runLBIProgram lbi prog =
  runDbProgram
    (fromFlagOrDefault normal $ configVerbosity $ configFlags lbi)
    prog
    (withPrograms lbi)

{-|
Run a 'Program' and retrieve @stdout@ with default 'Verbosity'.
-}
getLBIProgramOutput :: LocalBuildInfo -> Program -> [ProgArg] -> IO String
getLBIProgramOutput lbi prog =
  getDbProgramOutput
    (fromFlagOrDefault normal $ configVerbosity $ configFlags lbi)
    prog
    (withPrograms lbi)


#if MIN_VERSION_Cabal(2,0,0)
{-|
Extract 'PackageDBFlag's from 'LocalBuildInfo' to put into the 'packageDBFlags' field of 'DynFlags'. 
This is useful to ensure the invocation of GHC API shares the same package databases (e.g. a @stack@ snapshot)
-}
getGHCPackageDBFlags :: LocalBuildInfo -> [PackageDBFlag]
getGHCPackageDBFlags lbi =
  reverse $
  case withPackageDB lbi of
    (GlobalPackageDB:UserPackageDB:dbs)
      | all isSpecific dbs -> fmap single dbs
    (GlobalPackageDB:dbs)
      | all isSpecific dbs -> NoUserPackageDB : fmap single dbs
    dbs -> ClearPackageDBs : fmap single dbs
  where
    single (SpecificPackageDB db) = PackageDB $ PkgConfFile db
    single GlobalPackageDB = PackageDB GlobalPkgConf
    single UserPackageDB = PackageDB UserPkgConf
    isSpecific (SpecificPackageDB _) = True
    isSpecific _ = False
#else
-- 'PackageDBFlag' is a new wrapper around 'PkgConfRef' introduced in ghc-8.2.1 and Cabal-2.
-- ghc-8.0.2/Cabal-1.24 has a similar mechanism around 'extraPkgConfs' which was superseded
-- by 'packageDBFlags'.
{-|
Extract 'PkgConfRef's from 'LocalBuildInfo' to be prepended to the 'extraPkgConfs' field of 'DynFlags'. 
This is useful to ensure the invocation of GHC API shares the same package databases (e.g. a @stack@ snapshot)
-}
getGHCPackageDBFlags :: LocalBuildInfo -> [PkgConfRef]
getGHCPackageDBFlags = reverse . fmap toPkgConfRef . withPackageDB
  where
    toPkgConfRef (SpecificPackageDB db) = PkgConfFile db
    toPkgConfRef GlobalPackageDB = GlobalPkgConf
    toPkgConfRef UserPackageDB = UserPkgConf
#endif

endOfFirstLineVersion :: Verbosity -> FilePath -> IO (Maybe Version)
endOfFirstLineVersion =
  findProgramVersion "--version" $ last . words . head . lines

cmakeProgram :: Program
cmakeProgram =
  (simpleProgram "cmake") {programFindVersion = endOfFirstLineVersion}

makeProgram :: Program
makeProgram =
  (simpleProgram $
   case buildOS of
     Windows -> "mingw32-make"
     _ -> "make")
  {programFindVersion = endOfFirstLineVersion}

ninjaProgram :: Program
ninjaProgram =
  (simpleProgram "ninja")
  {programFindVersion = findProgramVersion "--version" id}
