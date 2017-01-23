{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_RocketFlyingGame (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/mieszkomakuch/Library/Haskell/bin"
libdir     = "/Users/mieszkomakuch/Library/Haskell/ghc-8.0.2-x86_64/lib/RocketFlyingGame-1.0"
dynlibdir  = "/Users/mieszkomakuch/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/mieszkomakuch/Library/Haskell/share/ghc-8.0.2-x86_64/RocketFlyingGame-1.0"
libexecdir = "/Users/mieszkomakuch/Library/Haskell/libexec"
sysconfdir = "/Users/mieszkomakuch/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "RocketFlyingGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "RocketFlyingGame_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "RocketFlyingGame_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "RocketFlyingGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "RocketFlyingGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "RocketFlyingGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
