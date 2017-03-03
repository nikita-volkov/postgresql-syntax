module PostgreSQL.Syntax.Pointers
where

import PostgreSQL.Syntax.Prelude
import qualified Data.ByteString.Short as B
import qualified Data.ByteString.Short.Internal as A
import qualified Data.ByteString.Unsafe as C
import qualified Data.ByteString.Internal as D


{-# INLINE withModifyingPtrOfBytes #-}
withModifyingPtrOfBytes :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withModifyingPtrOfBytes bytes ptrAction =
  C.unsafeUseAsCString bytes (ptrAction . (castPtr :: Ptr CChar -> Ptr Word8))

{-# INLINE createBytesAndMap #-}
createBytesAndMap :: Int -> (Ptr Word8 -> IO (ByteString -> a)) -> IO a
createBytesAndMap l f =
  do
    fp <- D.mallocByteString l
    postprocess <- withForeignPtr fp $ \p -> f p
    return $! postprocess $! D.PS fp 0 l
