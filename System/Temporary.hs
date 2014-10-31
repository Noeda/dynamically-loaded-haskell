-- | Safe temporary file implementation.
--
-- Safe in the sense that OS facilities to create a temporary file are used and
-- it is assumed that the OS is doing it right.
--

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Temporary
    ( withTemporaryFile )
    where

import System.IO
import Data.Monoid
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error
import Foreign.Ptr
import Control.Exception
import Control.Monad
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types

foreign import ccall unsafe mkstemp :: Ptr CChar -> IO CInt
foreign import ccall unsafe get_tempdir :: IO (Ptr CChar)

withTemporaryFile :: (Handle -> FilePath -> IO a) -> IO a
withTemporaryFile action = mask $ \restore -> do
    tmp_dir_name <- peekCString =<< get_tempdir
    withCString (tmp_dir_name <> "/hs-interfaceXXXXXX") $ \tmpname -> do
        result <- mkstemp tmpname
        when (result == -1) $ throwErrno "withTemporaryFile"

        str <- peekCString tmpname
        handle <- fdToHandle (Fd result)
        flip finally (removeLink str >> hClose handle) $ restore $
            action handle str

