{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -luuid #-}

module UUID
  ( newUUID
  ) where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CUChar)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "uuid/uuid.h uuid_generate"
  c_uuid_generate :: Ptr CUChar -> IO ()

foreign import ccall unsafe "uuid/uuid.h uuid_unparse_lower"
  c_uuid_unparse_lower :: Ptr CUChar -> CString -> IO ()

newUUID :: IO String
newUUID =
  allocaArray 16 $ \uuid ->
    allocaArray 37 $ \out -> do
      c_uuid_generate uuid
      c_uuid_unparse_lower uuid out
      peekCString out
