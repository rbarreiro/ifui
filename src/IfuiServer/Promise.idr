module IfuiServer.Promise

export
data Promise a = MkPromise ((a -> IO ()) -> IO ())
