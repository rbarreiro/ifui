module IfuiServer.Promise

public export
record Promise a where
  constructor MkPromise
  run : (a -> IO ()) -> IO ()
  cancel : IO ()
