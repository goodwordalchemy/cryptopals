{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cryptopals (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/bin"
libdir     = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/lib/x86_64-osx-ghc-8.6.5/cryptopals-0.1.0.0-JpTxgWsT18MKLTeOtvn8AF-cryptopals-exe"
dynlibdir  = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/share/x86_64-osx-ghc-8.6.5/cryptopals-0.1.0.0"
libexecdir = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/libexec/x86_64-osx-ghc-8.6.5/cryptopals-0.1.0.0"
sysconfdir = "/Users/dgoldberg/Code/cryptopals/.stack-work/install/x86_64-osx/88004b73fdc3a9b0cfc449cd46ca86406a313b1ab28c05c0e99b98dc16974176/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cryptopals_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cryptopals_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cryptopals_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cryptopals_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cryptopals_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cryptopals_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
