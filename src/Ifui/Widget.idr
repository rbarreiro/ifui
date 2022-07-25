module Ifui.Widget

import Ifui.VirtualDom

record Widget a where
  constructor MkWidget
  start : (a -> IO (Maybe VNode)) -> IO (Maybe VNode)

Functor Widget where 
  map g x = MkWidget (\z => x.start (z . g))

widgetBind : Widget a -> (a -> Widget b) -> Widget b
widgetBind (MkWidget x) f = MkWidget (\z => x (\w => (let MkWidget u = f w in u z)))

widgetPure : a -> Widget a
widgetPure x = MkWidget (\z => z x)

Applicative Widget where
  pure = widgetPure 
  (<*>) f x = widgetBind f (\z => widgetBind x (\w => pure $ z w))

Monad Widget where
  (>>=) = widgetBind 

text : String -> Widget a


runWidgetInDom : Widget Void -> IO ()
