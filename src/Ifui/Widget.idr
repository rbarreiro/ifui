module Ifui.Widget

import public Ifui.VirtualDom
import public Ifui.Dom
import public Ifui.Services
import public Ifui.ExtensibleRecords 
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
    step (Just f, Nothing) = 
      widgetBind  
        y
        (\z => step (Just f, Just z))
    step (Nothing, Just w) = 
      widgetBind  
        x
        (\z => step (Just z, Just w))
    step (Nothing, Nothing) = 
      widgetBind  
        ((Left <$> x) <+> (Right <$> y))
        (\z =>
              case z of
                   (Left y) => step (Just y, Nothing)
                   (Right y) => step (Nothing, Just y)
        )

export
Applicative Widget where
  pure = WidgetPure
  (<*>) = widgetAp

export
Monad Widget where
  (>>=) = widgetBind

export
HasIO Widget where
  liftIO x = 
    MarkupWidget  [\_, _, onEvt => x >>= onEvt]

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
data ServerConnection :  UKeyList String ServiceKind -> Type where
  MkServerConnection : String -> WebSocket -> (0 ts : UKeyList String ServiceKind) -> IORef (Integer) -> IORef (List (String, JSON -> IO ())) -> ServerConnection ts

onMsgFn : IORef (List (String, JSON -> IO ())) -> WsEvent -> IO ()
onMsgFn handles e =
 do
   s <- eventData e
   case parse s of
        (Just (JArray [JString i, x])) =>
          do
            h <- readIORef handles
            case lookup i h of
                 Nothing => putStrLn "Invalid handle id"
                 (Just y) => y x
        o => 
            putStrLn "Invalid request \{show o}"

export
serverConnect : String -> (0 ts : UKeyList String ServiceKind) -> (ServerConnection ts -> IO ()) -> IO ()
serverConnect url server onOpen =
  do
    ws <- createWebSocket url
    handles <- newIORef []
    counter <- newIORef 0
    setOnMessage ws (onMsgFn handles)
    setOnOpen ws $ onOpen $ MkServerConnection url ws server counter handles 

ptr2json : AnyPtr -> JSON
ptr2json = believe_me

json2ptr : JSON -> AnyPtr
json2ptr = believe_me

export
serverConnectWithAuth : (JsonSerializable loginTy, JsonSerializable roleTy) => String -> loginTy -> 
                        (sf : roleTy -> UKeyList String ServiceKind) -> Widget (DPair roleTy (\r => ServerConnection (sf r)))
serverConnectWithAuth url login sf = 
  MarkupWidget [\ns, n, onEvt => do
                  let login_ = toJson login
                  let proc = the (AnyPtr  -> IO ()) $ \ptr => 
                              onEvt (believe_me ptr)
                  setNodePromise n ("conect/" ++ url ++ "/" ++ show login_) proc $ do
                    procResult <- newIORef $  the (AnyPtr -> IO ()) $ \w => pure () 
                    isFinished <- newIORef False
                    ws <- createWebSocket url
                    handles <- newIORef []
                    counter <- newIORef 0
                    setOnOpen ws $ send ws (show $ toJson login) 
                    onMsg <- newIORef $ the (WsEvent -> IO ()) $ (\e => pure ())
                    writeIORef onMsg $ \e => do
                      s <- eventData e
                      case parse s of
                        Just j =>
                           do
                             let Just r = the (Maybe roleTy) (fromJson j) | Nothing => putStrLn "Invalid login response"
                             writeIORef onMsg (onMsgFn handles)
                             prc <- readIORef procResult
                             let serverConnection = (r ** MkServerConnection url ws (sf r) counter handles)
                             prc (believe_me $ the (DPair roleTy (\r => ServerConnection (sf r))) serverConnection)
                        Nothing =>
                             putStrLn "Invalid request \{s}"
                    setOnMessage ws $ \e => do
                      f <- readIORef onMsg
                      f e
                    pure $ MkPromiseNodeRef procResult (close ws) isFinished
               ]

removeHandle : String -> IORef (List (String, a)) -> IO ()
removeHandle str x = 
 do
   h <- readIORef x
   writeIORef x (deleteBy (\x, (y,_) => x == y)  str h)


export
callRPC : (JsonSerializable a, JsonSerializable b, HasValue s (RPC a b) ts) => 
            ServerConnection ts -> (s : String) -> a -> Widget b
callRPC (MkServerConnection url socket srv counter handles) s y = 
   MarkupWidget [\ns, n, onEvt => 
                                 do
                                    let y_ = toJson y
                                    let proc = \ptr => do
                                      let j = ptr2json ptr
                                      fromMaybe (putStrLn $ "invalid json in service \{s}")  (onEvt <$> fromJson j)
                                    setNodePromise n ("rpc/" ++ url ++ "/" ++ s ++ "?" ++ show y_) proc  $ do
                                      h <- readIORef handles
                                      i <- readIORef counter
                                      isFinished <- newIORef False
                                      let i_ = show i
                                      send socket (show $ JArray [JString s, JString i_, y_])
                                      procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
                                      writeIORef handles ((i_, \j => !(readIORef procResult) (json2ptr j) >> removeHandle i_ handles >> writeIORef isFinished True)  :: h) 
                                      writeIORef counter (i+1)
                                      let cancel = removeHandle i_ handles >> writeIORef isFinished True >> send socket (show $ JArray [JString "cancel", JString i_])
                                      pure $ MkPromiseNodeRef procResult cancel isFinished
                ]
export
callStream : (s : String) -> (JsonSerializable a, JsonSerializable b, HasValue s (StreamService a b) ts) => 
               ServerConnection ts -> a -> Widget b
callStream s (MkServerConnection url socket srv counter handles) y = 
   MarkupWidget [\ns, n, onEvt => 
                                 do
                                    let y_ = toJson y
                                    let proc = \ptr => do
                                      let j = ptr2json ptr
                                      fromMaybe (putStrLn $ "invalid json in service \{s}")  (onEvt <$> fromJson j)
                                    setNodePromise n ("streamService/" ++ url ++ "/" ++ s ++ "?" ++ show y_) proc  $ do
                                      h <- readIORef handles
                                      i <- readIORef counter
                                      isFinished <- newIORef False
                                      let i_ = show i
                                      send socket (show $ JArray [JString s, JString i_, y_])
                                      procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
                                      writeIORef handles ((i_, \j => !(readIORef procResult) (json2ptr j))  :: h) 
                                      writeIORef counter (i+1)
                                      let cancel = removeHandle i_ handles >> writeIORef isFinished True >> send socket (show $ JArray [JString "cancel", JString i_])
                                      pure $ MkPromiseNodeRef procResult cancel isFinished
                ]
        
export
callStreamAccum : (s : String) -> (JsonSerializable a, JsonSerializable b, HasValue s (StreamService a b) ts) => 
               ServerConnection ts -> a -> c -> (b -> c -> c) -> Widget c
callStreamAccum s (MkServerConnection url socket srv counter handles) y r0 acc = 
   MarkupWidget [\ns, n, onEvt => 
                                 do
                                    let y_ = toJson y
                                    let proc = \ptr => onEvt (believe_me ptr)
                                    setNodePromise n ("streamService/" ++ url ++ "/" ++ s ++ "?" ++ show y_) proc  $ do
                                      h <- readIORef handles
                                      i <- readIORef counter
                                      isFinished <- newIORef False
                                      let i_ = show i
                                      send socket (show $ JArray [JString s, JString i_, y_])
                                      procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
                                      a <- newIORef r0
                                      let handle = the (JSON -> IO ()) $ \j =>  do
                                        let Just x = fromJson {a=b} j | Nothing => putStrLn "invalid json in service \{s}"
                                        modifyIORef a (acc x)
                                        r <- readIORef a
                                        p <- readIORef procResult
                                        p $ believe_me r
                                      writeIORef handles ((i_, handle)  :: h) 
                                      writeIORef counter (i+1)
                                      let cancel = removeHandle i_ handles >> writeIORef isFinished True >> send socket (show $ JArray [JString "cancel", JString i_])
                                      pure $ MkPromiseNodeRef procResult cancel isFinished
                ]

accListWithId : (Eq t) => Change t -> List t -> List t
accListWithId x xs = 
  let old_val = the (Maybe t) (get "old_val" x)
      new_val = the (Maybe t) (get "new_val" x)
  in case (old_val, new_val) of
          (Nothing, Nothing) => xs
          (Nothing, (Just y)) => xs ++ [y]
          ((Just y), Nothing) => delete y xs
          ((Just y), (Just z)) => replaceOn y z xs
  
export
callStreamChangesAccumList : (s : String) -> (JsonSerializable a, JsonSerializable b, HasValue s (StreamService a (Change b)) ts, Eq b) => 
               ServerConnection ts -> a -> Widget (List b)
callStreamChangesAccumList s conn x = 
  callStreamAccum s conn x [] accListWithId

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
