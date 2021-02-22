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

bindir     = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/bin"
libdir     = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/lib/x86_64-osx-ghc-8.10.4/sudoku-0.1.0.0-3ayhNbPl7aa3Y1UUZyJL2w-sudoku"
dynlibdir  = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/share/x86_64-osx-ghc-8.10.4/sudoku-0.1.0.0"
libexecdir = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/libexec/x86_64-osx-ghc-8.10.4/sudoku-0.1.0.0"
sysconfdir = "/Users/Totoro/Documents/PSU/Graduate CS/CS557_FunctionalLanguages/sudoku_haskell/sudoku/.stack-work/install/x86_64-osx/6a573ab267c14f56970d88d07dd7e465f1864045dee6333359671a9529171c4f/8.10.4/etc"

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
