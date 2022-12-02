module Ifui.Widget

import public Ifui.VirtualDom
import public Ifui.Dom
import public Ifui.Services
import public Data.List.Elem
import Ifui.WebSocketsClient
import Data.IORef
import Ifui.Json
import Data.Maybe
import Data.List

public export
data WidgetAttribute a = WidgetSimpleAttribute AttributeSpec | WidgetEventListener String  (DomEvent -> IO a)


export
data Widget a = WidgetPure a
              | MarkupWidget (List (VNodes -> VNode -> (a -> IO ()) -> IO ())) -- VNodes are needed for bind to work

export
Functor WidgetAttribute where
  map f (WidgetSimpleAttribute x) = WidgetSimpleAttribute x
  map f (WidgetEventListener x g) = WidgetEventListener x (\e => f <$> g e)

export
Functor Widget where
  map g (WidgetPure x) = WidgetPure $ g x
  map g (MarkupWidget xs) = MarkupWidget $ mapStart <$> xs
  where
    mapStart : (VNodes -> VNode -> (a -> IO ()) -> IO ()) -> VNodes -> VNode -> (b -> IO ()) -> IO ()
    mapStart start ns n onEvt = start ns n (\w => onEvt $ g w)

export
Semigroup (Widget a) where
  (<+>) (WidgetPure x) y = WidgetPure x
  (<+>) (MarkupWidget xs) (WidgetPure x) = WidgetPure x
  (<+>) (MarkupWidget xs) (MarkupWidget ys) = MarkupWidget $ xs <+> ys

export
Monoid (Widget a) where
  neutral = MarkupWidget []

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


widgetLoopState :  s -> (s -> Widget (Either s a)) -> Widget a
widgetLoopState x f = 
  widgetBind 
    (f x)  
    (\z =>
          case z of
               (Left y) => widgetLoopState y f
               (Right y) => WidgetPure y
    )



widgetAp : Widget (a -> b) -> Widget a -> Widget b
widgetAp x (WidgetPure y) =
  (\z => z y) <$> x
widgetAp (WidgetPure x) y =
  x <$> y
widgetAp x y =
  widgetLoopState (Nothing, Nothing) step
  where
    step : (Maybe (a -> b), Maybe a) -> Widget (Either (Maybe (a -> b), Maybe a) b)
    step (Just f, Just w)  = 
      WidgetPure $ Right $ f w
    step (f, w) = 
      widgetBind  
        ((Left <$> x) <+> (Right <$> y))
        (\z =>
              case z of
                   (Left y) => step (Just y, w)
                   (Right y) => step (f, Just y)
        )


export
Applicative Widget where
  pure = WidgetPure
  (<*>) = widgetAp
   

export
Monad Widget where
  (>>=) = widgetBind


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

export
data ServerConnection :  List (String, ServiceKind) -> Type where
  MkServerConnection : String -> WebSocket -> (0 ts : List (String, ServiceKind)) -> IORef (Integer) -> IORef (List (String, JSON -> IO ())) -> ServerConnection ts


export
serverConnect : String -> (0 ts : List (String, ServiceKind)) -> (ServerConnection ts -> IO ()) -> IO ()
serverConnect url server onOpen =
  do
    ws <- createWebSocket url
    handles <- newIORef []
    counter <- newIORef 0
    setOnMessage ws $ \e =>
                           do
                             s <- eventData e
                             case parse s of
                                  (Just (JArray [JString i, x])) =>
                                    do
                                      h <- readIORef handles
                                      case lookup i h of
                                           Nothing => putStrLn $ "Invalid handle id"
                                           (Just y) => y x
                                  o => 
                                    putStrLn ("Invalid request" ++ show o)

    setOnOpen ws $ onOpen $ MkServerConnection url ws server counter handles 

removeHandle : String -> IORef (List (String, JSON -> IO ())) -> IO ()
removeHandle str x = 
 do
   h <- readIORef x
   writeIORef x (deleteBy (\x, (y,_) => x == y)  str h)

export
callRPC : (JsonSerializable a, JsonSerializable b) => ServerConnection ts -> (s : String) -> {auto 0 p : Elem (s, RPC a b) ts} -> a -> Widget b
callRPC (MkServerConnection url socket srv counter handles) s y = 
   MarkupWidget [\ns, n, onEvt => 
                                 do
                                    let y_ = toJson y
                                    let proc = \j => fromMaybe (putStrLn $ "invalid json in service" ++ s)  (onEvt <$> fromJson j)
                                    setNodePromise n ("rpc/" ++ url ++ "/" ++ s ++ "?" ++ show y_) !(newIORef False) proc  $ do
                                      h <- readIORef handles
                                      i <- readIORef counter
                                      let i_ = show i
                                      send socket (show $ JArray [JString s, JString i_, y_])
                                      procResult <- newIORef (the (JSON -> IO ()) $ \w => pure ()) 
                                      writeIORef handles ((i_, \j => !(readIORef procResult) j >> removeHandle i_ handles)  :: h) 
                                      writeIORef counter (i+1)
                                      pure procResult
                ]
         

export
runWidget : Widget () -> IO ()
runWidget (WidgetPure x) = pure ()
runWidget (MarkupWidget xs) =
  do
    ns <- createEmptyVNodes !body
    updateVNodes ns (runone ns <$> xs)
    where
      runone : VNodes -> (VNodes -> VNode -> (() -> IO ()) -> IO ()) -> VNode -> IO ()
      runone x f y = f x y (\_ => pure ())
