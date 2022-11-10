module IfuiServer.Promise

import Data.IORef

public export
record PromiseHandler where
  constructor MkPromiseHandler
  cancel : IO ()

public export
record Promise a where
 constructor  MkPromise 
 run : (a -> IO ()) -> IO PromiseHandler

export
Functor Promise where
  map f (MkPromise run) = MkPromise (\w => run (w . f))

%foreign "javascript:lambda:(callback, delay)=>{t = setTimeout(callback, delay); return (() => clearTimeout(t))}"
prim__setTimeout : (PrimIO ()) -> Int -> PrimIO (PrimIO ())
setTimeout : HasIO io => IO () -> Int -> io (IO ())
setTimeout callback delay = 
  primIO <$> (primIO $ prim__setTimeout (toPrim callback) delay)

export
Applicative Promise where
  pure x = MkPromise (\w => MkPromiseHandler <$> setTimeout (w x) 0 )
  (<*>) f x = MkPromise $ \w =>
                do
                  fres <- newIORef $ the (Maybe (a -> b)) Nothing
                  xres <-  newIORef $ the (Maybe a) Nothing
                  fh <- f.run $ \f_ => do
                    case !(readIORef xres) of
                         Nothing => writeIORef fres (Just f_)
                         Just x_ =>  w $ f_ x_
                  xh <- x.run $ \x_ => do
                    case !(readIORef fres) of
                         Nothing => writeIORef xres (Just x_)
                         Just f_ =>  w $ f_ x_
                  pure $ MkPromiseHandler $ fh.cancel >> xh.cancel

