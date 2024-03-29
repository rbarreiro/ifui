module Ifui.Promise

import Data.IORef
import Ifui.JSUtils

public export
record PromiseHandler where
  constructor MkPromiseHandler
  cancel : IO ()

public export
record Promise a where
 constructor  MkPromise 
 run : (a -> IO ()) -> IO PromiseHandler

public export
MkPromise' : ((a -> IO ()) -> IO ()) -> Promise a
MkPromise' f = 
  MkPromise $ \g => do
    h <- newIORef g
    f (\w => do g_ <- readIORef h; g w)
    pure $ MkPromiseHandler (writeIORef h (\w => pure ()))

export
Functor Promise where
  map f (MkPromise run) = MkPromise (\w => run (w . f))

export
onErrPrint : Promise (Either String a) -> Promise a
onErrPrint x =
  MkPromise $ \w =>
    x.run $ \z => case z of 
                       (Left y) => putStrLn y
                       (Right y) => w y

export
onErrThrow : Promise (Either String a) -> Promise a
onErrThrow x =
  MkPromise $ \w =>
    x.run $ \z => case z of 
                       (Left y) => throw y
                       (Right y) => w y

export
Applicative Promise where
  pure x = MkPromise' (\w => w x)
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

export
Monad Promise where
  (>>=) x y = 
    MkPromise (\w => do
                      h <- newIORef $ MkPromiseHandler $ pure ()
                      h1 <- x.run (\z => do h2 <- (y z).run w; writeIORef h h2)
                      writeIORef h h1
                      pure $ MkPromiseHandler (do h_ <- readIORef h; h_.cancel)
              )
export
HasIO Promise where
  liftIO x = 
    MkPromise' $ \w => do
      r <- x
      setTimeout (w r) 0
