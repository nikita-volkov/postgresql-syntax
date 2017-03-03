module PostgreSQL.Syntax
where

import PostgreSQL.Syntax.Prelude
import qualified PostgreSQL.Syntax.Foreign as A
import qualified PostgreSQL.Syntax.Pointers as B
import qualified Data.ByteString.Unsafe as C
import qualified Data.ByteString.Internal as D
import qualified Data.Text.Encoding as E


-- |
-- Find an error in the SQL string.
-- 
-- The string is allowed to contain placeholders.
validate :: ByteString -> Maybe Text 
validate sql =
  unsafeDupablePerformIO $ do
    statusRef <- newIORef 1
    messageBytes <- 
      C.unsafeUseAsCString sql $ \sqlPtr -> D.createAndTrim 1000 $ \messagePtr -> do
        status <- A.validate sqlPtr (castPtr messagePtr)
        writeIORef statusRef status
        case status of
          0 -> return 0
          1 -> fmap fromIntegral (D.c_strlen (castPtr messagePtr))
    status <- readIORef statusRef
    case status of
      0 -> return Nothing
      1 -> return (Just (E.decodeUtf8 messageBytes))
