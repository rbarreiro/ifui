module Ifui.Widget

import Ifui.VirtualDom
import public Ifui.Dom

public export
data WidgetAttribute a = WidgetStringAttribute String String | WidgetEventListener String  (DomEvent -> IO a)

export
record Widget a where
  constructor MkWidget
  start : VNode -> (a -> IO ()) -> IO ()

export
Functor Widget where 
  map g x = MkWidget (\n, onEvt => x.start n (\w => onEvt $ g w) )

widgetBind : Widget a -> (a -> Widget b) -> Widget b
widgetBind x f = 
  MkWidget (\n, onEvt => x.start n (\w => let u = f w in u.start n onEvt))

widgetPure : a -> Widget a
widgetPure x = MkWidget (\n, onEvt => onEvt x)

export
Applicative Widget where
  pure = widgetPure 
  (<*>) f x = widgetBind f (\z => widgetBind x (\w => pure $ z w))

export
Monad Widget where
  (>>=) = widgetBind 

export
text : String -> Widget a
text x = MkWidget (\n, _ => setNodeText n x)

export
node : String -> List (WidgetAttribute a) -> List (Widget a) -> Widget a
node tag attrs children = 
  MkWidget $ \node, onEvt => do
    setNodeTag node tag
    setNodeAttributes node (convAttr onEvt <$> attrs)
    setNodeChildren node ((\w => (\n => w.start n onEvt)) <$> children)
    where
      convAttr : (a -> IO ()) -> WidgetAttribute a -> Attribute
      convAttr onEvt (WidgetStringAttribute n x) = StringAttr n x
      convAttr onEvt (WidgetEventListener x g) = EventListener x (\e => g e >>= onEvt)

export
runWidget : Widget () -> IO ()
runWidget x = 
  do
    n <- createEmptyVNode !body
    x.start n (\_ => pure ())


