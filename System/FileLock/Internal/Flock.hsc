module System.FileLock.Internal.Flock
#ifndef USE_FLOCK
  () where
#else
  (Lock, lock, tryLock, unlock) where

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

import Foreign (new)
import Foreign.Ptr (Ptr)
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Storable (Storable, poke, peek, peekByteOff, pokeByteOff, sizeOf, alignment)
import Control.Applicative
import qualified Control.Exception as E
import System.Posix.Files
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(..))
import System.Posix.Types

type Lock = Fd

data CFLock = CFLock {
  l_type   :: !CInt,
  l_whence :: !CInt,
  l_start  :: !COff,
  l_len    :: !COff,
  l_pid    :: !CPid
} deriving(Eq, Ord, Show)

instance Show Errno where
  show (Errno e) = show $ "errno=" ++ show e

instance Storable CFLock where
  sizeOf x = sizeOf (l_type x)
             + sizeOf (l_whence x)
             + sizeOf (l_start x)
             + sizeOf (l_len x)
             + sizeOf (l_pid x)
  alignment _ = alignment (undefined :: COff)
  peek ptr = CFLock <$> #{peek struct flock, l_type} ptr
                   <*> #{peek struct flock, l_whence} ptr
                   <*> #{peek struct flock, l_start} ptr
                   <*> #{peek struct flock, l_len} ptr
                   <*> #{peek struct flock, l_pid} ptr
  poke ptr (CFLock t w s l p) = do
      #{poke struct flock, l_type} ptr t'
      #{poke struct flock, l_whence} ptr w'
      #{poke struct flock, l_start} ptr s'
      #{poke struct flock, l_len} ptr l'
      #{poke struct flock, l_pid} ptr p'
    where
      t' = fromIntegral t :: CInt
      w' = fromIntegral w :: CInt
      s' = fromIntegral s :: COff
      l' = fromIntegral l :: COff
      p' = fromIntegral p :: CPid

lock :: FilePath -> Bool -> IO Lock
lock path exclusive = do
  fd <- open path
  (`E.onException` closeFd fd) $ do
    True <- flock fd exclusive True
    return fd

tryLock :: FilePath -> Bool -> IO (Maybe Lock)
tryLock path exclusive = do
  fd <- open path
  (`E.onException` closeFd fd) $ do
    success <- flock fd exclusive False
    if success
      then return $ Just $ fd
      else Nothing <$ closeFd fd

unlock :: Lock -> IO ()
unlock fd = closeFd fd

open :: FilePath -> IO Fd
open path = openFd path ReadWrite (Just stdFileMode) defaultFileFlags

flock :: Fd -> Bool -> Bool -> IO Bool
flock (Fd fd) exclusive block = do
  flck <- new modeOp
  r <- c_fcntl fd blockOp flck
  if r /= -1
    then return True -- success
    else do
      errno <- getErrno
      case () of
        _ | (errno == Errno #{const EAGAIN} ||
             errno == Errno #{const EACCES})
            -> return False -- already taken
          |  errno == Errno #{const EINTR}
            -> flock (Fd fd) exclusive block
          | otherwise -> throwErrno $ "fcntl(" ++ show(errno) ++ ")"
  where
    modeOp = case exclusive of
      False -> CFLock #{const F_RDLCK} #{const SEEK_SET} 0 0 0
      True -> CFLock #{const F_WRLCK} #{const SEEK_SET} 0 0 0
    blockOp = case block of
      True -> #{const F_SETLKW}
      False -> #{const F_SETLK} -- #{const LOCK_NB}

foreign import ccall "fcntl.h fcntl"
  c_fcntl :: CInt -> CInt -> (Ptr CFLock) -> IO CInt

#endif /* USE_FLOCK */
