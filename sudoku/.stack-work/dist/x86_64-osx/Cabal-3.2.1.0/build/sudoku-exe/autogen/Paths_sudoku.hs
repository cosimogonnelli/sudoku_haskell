{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_sudoku (
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

bindir     = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/bin"
libdir     = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/lib/x86_64-osx-ghc-8.10.3/sudoku-0.1.0.0-Jdvp2bxnHRU2Aq13O12ukn-sudoku-exe"
dynlibdir  = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/share/x86_64-osx-ghc-8.10.3/sudoku-0.1.0.0"
libexecdir = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/libexec/x86_64-osx-ghc-8.10.3/sudoku-0.1.0.0"
sysconfdir = "/Users/cosimo/Documents/Coding/CS557_Functional_Programming/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/2ce660428f3c267fa30995bf3a0f861ac37c4ed5840a53b6cba93b99ce9ff5b9/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sudoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sudoku_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sudoku_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sudoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
