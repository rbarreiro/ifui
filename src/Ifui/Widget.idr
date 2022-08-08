module Ifui.Widget

import public Ifui.VirtualDom
import public Ifui.Dom

public export
data WidgetAttribute a = WidgetSimpleAttribute AttributeSpec | WidgetEventListener String  (DomEvent -> IO a)


export
data Widget a = WidgetPure a
              | MarkupWidget (List (VNodes -> VNode -> (a -> IO ()) -> IO ()))

export
Functor WidgetAttribute where
  map f (WidgetSimpleAttribute x) = WidgetSimpleAttribute x
  map f (WidgetEventListener x g) = WidgetEventListener x (\e => f <$> g e)

export
Functor Widget where 
  map g (WidgetPure x) = WidgetPure $ g x
  map g (MarkupWidget xs) = MarkupWidget $ ?mapStart <$> xs
  where
    mapStart : (VNodes -> VNode -> (a -> IO ()) -> IO ()) -> VNodes -> VNode -> (b -> IO ()) -> IO () 
    mapStart start ns n onEvt = start ns n (\w => onEvt $ g w)

widgetBind : Widget a -> (a -> Widget b) -> Widget b
widgetBind (WidgetPure x) f = f x
widgetBind (MarkupWidget xs) f = 
  MarkupWidget $ bindStart <$> xs
  where
    convCont : VNodes -> (b -> IO ())  -> (VNodes -> VNode -> (b -> IO ()) -> IO ()) -> VNode -> IO ()
    convCont ns onEvt contStart x = contStart ns x onEvt

    cont : VNodes -> (b -> IO())  -> Widget b -> IO ()
    cont ns onEvt (WidgetPure x) = onEvt x
    cont ns onEvt (MarkupWidget ys) = updateVNodes ns (convCont ns onEvt <$> ys)

    bindStart : (VNodes -> VNode -> (a -> IO ()) -> IO ()) -> VNodes -> VNode -> (b -> IO ()) -> IO ()
    bindStart start ns n onEvt = start ns n (\w => let u = f w in cont ns onEvt u)

widgetPure : a -> Widget a
widgetPure x = WidgetPure x

export
Applicative Widget where
  pure = widgetPure 
  (<*>) f x = widgetBind f (\z => widgetBind x (\w => pure $ z w))

export
Monad Widget where
  (>>=) = widgetBind 

export
Semigroup (Widget a) where
  (<+>) (WidgetPure x) y = WidgetPure x 
  (<+>) (MarkupWidget xs) (WidgetPure x) = WidgetPure x
  (<+>) (MarkupWidget xs) (MarkupWidget ys) = MarkupWidget $ xs <+> ys

export
Monoid (Widget a) where
  neutral = MarkupWidget []

export
text : String -> Widget a
text x = MarkupWidget [\_, n, _ => setNodeText n x]

export
node : String -> List (WidgetAttribute a) -> List (Widget a) -> Widget a
node tag attrs children = 
  case concat children of
       (WidgetPure x) => 
          WidgetPure x
       (MarkupWidget xs) =>
          MarkupWidget [\ns, n, onEvt => do
            setNodeTag n tag
            setNodeAttributes n (convAttr onEvt <$> attrs)
            updateVNodes n.children (runChildrenStarts n.children onEvt <$> xs)
            ]
    where
    runChildrenStarts : VNodes -> (a -> IO ())  -> (VNodes -> VNode -> (a -> IO ()) -> IO ()) -> VNode -> IO ()
    runChildrenStarts ns onEvt f x = f ns x onEvt

    convAttr : (a -> IO ()) -> WidgetAttribute a -> Attribute
    convAttr onEvt (WidgetSimpleAttribute spec) = SimpleAttribute spec
    convAttr onEvt (WidgetEventListener x g) = EventListener x (\e => g e >>= onEvt)

--export
--runWidget : Widget () -> IO ()
--runWidget x = 
--  do
--    n <- createEmptyVNode !body
--    x.start n (\_ => pure ())


