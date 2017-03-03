module PostgreSQL.Syntax.Foreign
where


import PostgreSQL.Syntax.Prelude


foreign import ccall unsafe "validate"
  validate :: Ptr CChar -> Ptr CChar -> IO CInt
