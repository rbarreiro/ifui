module Ifui.IOStream

import Data.IORef

public export
record StreamHandler where
  constructor MkStreamHandler
  cancel : IO ()

public export
record IOStream a where
 constructor  MkIOStream 
 run : (a -> IO ()) -> IO StreamHandler

export
Functor IOStream where
  map f (MkIOStream run) = MkIOStream (\w => run (w . f))

export
onErrPrint : IOStream (Either String a) -> IOStream a
onErrPrint x =
  MkIOStream $ \w =>
    x.run $ \z => case z of 
                       (Left y) => putStrLn y
                       (Right y) => w y

export
accum : a -> IOStream (a -> a) -> IOStream a
accum init x = 
  MkIOStream $ \w => do
    st <- newIORef init
    x.run $ \z => do
      modifyIORef st z
      r <- readIORef st
      w r

