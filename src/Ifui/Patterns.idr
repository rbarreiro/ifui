module Ifui.Patterns


export
loopState : Monad m => s -> (s -> m (Either s a)) -> m a
loopState x f = 
  case !(f x) of
       Right y => pure y
       Left y  => loopState y f

export
loopForever : Monad m => a -> (a -> m a) -> m b
loopForever x f = 
  loopForever !(f x) f
