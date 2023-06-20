module Ifui.Widget

import public Ifui.VirtualDom
import public Ifui.Dom
import public Ifui.Services
import public Ifui.ExtensibleTypes
import Ifui.WebSocketsClient
import Data.IORef
import Ifui.Json
import Ifui.Patterns
import Data.Maybe
import Data.List
import Ifui.Date

public export
data WidgetAttribute a = WidgetSimpleAttribute AttributeSpec | WidgetEventListener String  (DomEvent -> IO a)


export
data Widget a = WidgetPure a
              | MarkupWidget (VNode -> (a -> IO ()) -> IO ())
              | WidgetGroup (List (Widget a))

export
Semigroup (Widget a) where
  (<+>) (WidgetPure x) _ = WidgetPure x
  (<+>) _ (WidgetPure x) = WidgetPure x
  (<+>) (WidgetGroup xs) (WidgetGroup ys) = WidgetGroup (xs ++ ys)
  (<+>) (WidgetGroup xs) (MarkupWidget y) = WidgetGroup (xs ++ [MarkupWidget y])
  (<+>) (MarkupWidget x) (WidgetGroup ys) = WidgetGroup ((MarkupWidget x) :: ys)
  (<+>) (MarkupWidget xs) (MarkupWidget ys) = WidgetGroup  [MarkupWidget xs, MarkupWidget ys]

total
simplifyWList : List (Widget a) -> Either a (List (VNode -> (a -> IO ()) -> IO ()))
simplifyWList [] = Right []
simplifyWList ((WidgetPure x) :: xs) = Left x
simplifyWList ((MarkupWidget f) :: xs) = 
  case simplifyWList xs of
       (Left x) => Left x
       (Right x) => Right $ f :: x
simplifyWList ((WidgetGroup ys) :: xs) = 
  case (simplifyWList ys, simplifyWList xs) of
       ((Left x), _) => Left x
       ((Right _), (Left y)) => Left y
       ((Right x), (Right y)) => Right $ x ++ y

applyStart : (b -> IO ()) -> (VNode -> (b -> IO ()) -> IO ()) -> VNode -> IO ()
applyStart onEvt strt x = strt x onEvt

export
node : String -> List (WidgetAttribute a) -> List (Widget a) -> Widget a
node tag attrs children =
   case simplifyWList children of
        (Left x) => 
           WidgetPure x
        (Right xs) => 
           MarkupWidget $ \n, onEvt => do
             setNodeTag n tag
             updateVNodes n.children (applyStart onEvt <$> xs)
             setNodeAttributes n (convAttr onEvt <$> attrs)
   where
     convAttr : (a -> IO ()) -> WidgetAttribute a -> Attribute
     convAttr onEvt (WidgetSimpleAttribute spec) = SimpleAttribute spec
     convAttr onEvt (WidgetEventListener x g) = EventListener x (\e => g e >>= onEvt)

export
Monoid (Widget a) where
  neutral = node "span" [] []

export
Functor WidgetAttribute where
  map f (WidgetSimpleAttribute x) = WidgetSimpleAttribute x
  map f (WidgetEventListener x g) = WidgetEventListener x (\e => f <$> g e)

public export
Functor Widget where
  map g (WidgetPure x) = WidgetPure $ g x
  map g (MarkupWidget start) = MarkupWidget $ \n, onEvt => start n (\w => onEvt (g w))
  map g (WidgetGroup xs) = assert_total $ WidgetGroup $ (map g) <$> xs


contOnEvt : VNode -> (b -> IO ()) -> (a -> Widget b) -> a -> IO ()
contOnEvt n onEvt f w = 
  case f w of
    (WidgetPure x) => 
       onEvt x
    (MarkupWidget start) =>
       start n onEvt
    (WidgetGroup xs) =>
       case simplifyWList xs of
            Left z =>
              onEvt z
            Right [start] =>
              start n onEvt
            Right ys =>
              do
                setNodeTag n "span"
                setNodeAttributes n []
                updateVNodes n.children (applyStart onEvt <$> ys)

widgetBind : Widget a -> (a -> Widget b) -> Widget b
widgetBind (WidgetPure x) f = 
  f x
widgetBind (WidgetGroup xs) f = 
  case simplifyWList xs of
       (Left x) => 
          f x
       (Right [start]) =>
          MarkupWidget $ \n, onEvt => do
            start n (contOnEvt n onEvt f)
       (Right xs) =>
          MarkupWidget $ \n, onEvt => do
            setNodeTag n "span"
            setNodeAttributes n []
            updateVNodes n.children (applyStart (contOnEvt n onEvt f) <$> xs)
widgetBind (MarkupWidget start) f =
  MarkupWidget $ \n, onEvt =>
     start n (contOnEvt n onEvt f)
             

total
widgetLoopState :  s -> (s -> Widget (Either s a)) -> Widget a
widgetLoopState x f = 
  widgetBind 
    (f x)  
    (\z =>
          case z of
               (Left y) => assert_total $ widgetLoopState y f
               (Right y) => WidgetPure y
    )


total
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
        (\z => WidgetPure $ Left (Just f, Just z))
    step (Nothing, Just w) = 
      widgetBind  
        x
        (\z => WidgetPure $ Left (Just z, Just w))
    step (Nothing, Nothing) = 
      widgetBind  
        ((Left <$> x) <+> (Right <$> y))
        (\z =>
              case z of
                   (Left y) => WidgetPure $ Left (Just y, Nothing)
                   (Right y) => WidgetPure $ Left (Nothing, Just y)
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
    MarkupWidget $ \_, onEvt => x >>= onEvt

export
text : String -> Widget a
text x = MarkupWidget $ \n, _ => setNodeText n x


record ConnectionInfo where
  constructor MkConnectionInfo
  url : String
  socket : WebSocket
  counter : IORef (Integer)
  handles : IORef (List (String, JSON -> IO ()))

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

serverConnect' : String -> (ConnectionInfo -> IO ()) -> IO ()
serverConnect' url onOpen =
  do
    ws <- createWebSocket url
    handles <- newIORef []
    counter <- newIORef 0
    setOnMessage ws (onMsgFn handles)
    setOnOpen ws $ onOpen $ MkConnectionInfo url ws counter handles 

ptr2json_ : AnyPtr -> JSON
ptr2json_ = believe_me

json2ptr_ : JSON -> AnyPtr
json2ptr_ = believe_me

serverConnectWithAuth' : (JsonSerializable loginTy, JsonSerializable roleTy) => String -> loginTy -> 
                            Widget (roleTy, ConnectionInfo)
serverConnectWithAuth' url login  = 
  MarkupWidget $ \n, onEvt => do
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
                             let Just r = the (Maybe roleTy) (fromJson j) | Nothing => putStrLn "Invalid login response \{show j}"
                             writeIORef onMsg (onMsgFn handles)
                             prc <- readIORef procResult
                             let serverConnection = (r, MkConnectionInfo url ws counter handles)
                             writeIORef isFinished True
                             prc (believe_me $ the (roleTy, ConnectionInfo) serverConnection)
                        Nothing =>
                             putStrLn "Invalid request \{s}"
                    setOnMessage ws $ \e => do
                      f <- readIORef onMsg
                      f e
                    pure $ MkPromiseNodeRef procResult (close ws >> writeIORef isFinished True) isFinished


export
data SrvRef : ServiceKind -> Type where
  Base : ConnectionInfo -> SrvRef t
  Sub : (s : String)  -> SrvRef r -> SrvRef t


infixl 6 //

export
(//) : SrvRef (GroupService ts) -> {auto 0 prf : So (UniqueKeys ts)} -> (s : String) -> {auto p : KElem s ts} -> SrvRef (klookup ts p)
(//) g s = Sub s g

serviceConnectionPathStr : SrvRef x -> String
serviceConnectionPathStr (Base (MkConnectionInfo url socket counter handles)) = url
serviceConnectionPathStr (Sub s y) = serviceConnectionPathStr y ++ "/" ++ s

serverConnect : String -> (0 server : ServiceKind) -> (SrvRef server -> IO ()) -> IO ()
serverConnect url server onOpen = serverConnect' url (\x => onOpen $ Base x)

export
serverConnectWithAuth : (JsonSerializable loginTy, JsonSerializable roleTy) => String -> loginTy -> 
                        (sf : roleTy -> ServiceKind) -> Widget (DPair roleTy (\r => SrvRef (sf r)))
serverConnectWithAuth url login sf = 
  do
     (r, z) <- serverConnectWithAuth' url login
     pure $ (r ** (Base z))

removeHandle : String -> IORef (List (String, a)) -> IO ()
removeHandle str x = 
 do
   h <- readIORef x
   writeIORef x (deleteBy (\x, (y,_) => x == y)  str h)

streamSetup : String -> ConnectionInfo -> JSON -> (JSON -> Maybe a)  -> Widget a
streamSetup path (MkConnectionInfo url socket counter handles) input outputReader = 
   MarkupWidget $ \n, onEvt => 
     do
        let proc = \ptr => do
          let j = ptr2json_ ptr
          fromMaybe (putStrLn $ "invalid json in service \{path}")  (onEvt <$> outputReader j)
        setNodePromise n ("streamService/" ++ url ++ "/" ++ "?" ++ show input) proc  $ do
          h <- readIORef handles
          i <- readIORef counter
          isFinished <- newIORef False
          let i_ = show i
          send socket (show $ JArray [JString "call", JString i_, input])
          procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
          writeIORef handles ((i_, \j => !(readIORef procResult) (json2ptr_ j))  :: h) 
          writeIORef counter (i+1)
          let cancel = removeHandle i_ handles >> writeIORef isFinished True >> send socket (show $ JArray [JString "cancel", JString i_])
          pure $ MkPromiseNodeRef procResult cancel isFinished

rpcSetup : String -> ConnectionInfo -> JSON -> (JSON -> Maybe a)  -> Widget a
rpcSetup path (MkConnectionInfo url socket counter handles) input outputReader = 
   MarkupWidget $ \n, onEvt => 
                                 do
                                    let proc = \ptr => do
                                      let j = ptr2json_ ptr
                                      fromMaybe (putStrLn $ "invalid json in service \{path}")  (onEvt <$> outputReader j)
                                    setNodePromise n ("rpc/" ++ url ++ "/" ++ "?" ++ show input) proc  $ do
                                      h <- readIORef handles
                                      i <- readIORef counter
                                      isFinished <- newIORef False
                                      let i_ = show i
                                      send socket (show $ JArray [JString "call", JString i_, input])
                                      procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
                                      writeIORef handles ((i_, \j => writeIORef isFinished True >>
                                                                     removeHandle i_ handles >> 
                                                                     !(readIORef procResult) (json2ptr_ j)
                                                                        
                                                          )  :: h
                                                         ) 
                                      writeIORef counter (i+1)
                                      let cancel = removeHandle i_ handles >> 
                                                    writeIORef isFinished True >> 
                                                     send socket (show $ JArray [JString "cancel", JString i_])
                                      pure $ MkPromiseNodeRef procResult cancel isFinished

streamAccumSetup : String -> String -> ConnectionInfo -> JSON -> (JSON -> Maybe a) -> c -> (a -> c -> c) -> Widget c
streamAccumSetup callId path (MkConnectionInfo url socket counter handles) input outputReader r0 acc = 
   MarkupWidget $ \n, onEvt => 
       do
          let proc = \ptr => onEvt (believe_me ptr)
          setNodePromise n ("streamServiceAccum/" ++ url ++ "/" ++ callId  ++ "/" ++ "?" ++ show input) proc  $ do
            h <- readIORef handles
            i <- readIORef counter
            isFinished <- newIORef False
            let i_ = show i
            send socket (show $ JArray [JString "call", JString i_, input])
            procResult <- newIORef (the (AnyPtr -> IO ()) $ \w => pure ()) 
            a <- newIORef r0
            let handle = the (JSON -> IO ()) $ \j =>  do
              let Just x = outputReader j | Nothing => putStrLn "invalid json in service \{path}"
              modifyIORef a (acc x)
              r <- readIORef a
              p <- readIORef procResult
              p $ believe_me r
            writeIORef handles ((i_, handle)  :: h) 
            writeIORef counter (i+1)
            let cancel = removeHandle i_ handles >> writeIORef isFinished True >> send socket (show $ JArray [JString "cancel", JString i_])
            pure $ MkPromiseNodeRef procResult cancel isFinished


addServicePathToInput : SrvRef ts -> JSON -> JSON
addServicePathToInput (Base _) json = json
addServicePathToInput (Sub s x) json = addServicePathToInput x $ JArray [JString s, json]

getConnectionInfo : SrvRef ts -> ConnectionInfo
getConnectionInfo (Base x) = x
getConnectionInfo (Sub s x) = getConnectionInfo x

export
callRPC : (JsonSerializable a, JsonSerializable b) => SrvRef (RPC a b) ->
            a -> Widget b
callRPC sc y = 
  rpcSetup (serviceConnectionPathStr sc) (getConnectionInfo sc) (addServicePathToInput sc $ toJson y) fromJson

namespace StreamWidget
  public export
  data StreamWidget : Type -> Type where
    MkStreamWidget : ({0 z : Type} -> (a -> Widget z) -> Widget z) -> StreamWidget a

  export
  toWidget : (a -> Widget b) -> StreamWidget a -> Widget b
  toWidget f (MkStreamWidget g) = g f

  export
  Functor StreamWidget where
    map f (MkStreamWidget g) = MkStreamWidget (\w => g (w . f))

export
callStream : (JsonSerializable a, JsonSerializable b) => 
               SrvRef (StreamService a b) -> a -> StreamWidget b -- (b -> Widget c) -> Widget c
callStream sc y = MkStreamWidget $ \w =>
  let stream : Widget b
      stream = streamSetup (serviceConnectionPathStr sc) (getConnectionInfo sc) (addServicePathToInput sc $ toJson y) fromJson

  in loopState Nothing (\x => ((Left . Just) <$> stream) <+> (Right <$> (fromMaybe neutral $ w <$> x)))
 
export
callStreamAccum : (JsonSerializable a, JsonSerializable b) => 
               SrvRef (StreamService a b) -> a -> c -> (b -> c -> c) -> StreamWidget c   -- (c -> Widget d) -> Widget d
callStreamAccum sc y r0 acc =  MkStreamWidget $ \w =>
  do
    millis <- liftIO millisSinceEpoch
    let stream = streamAccumSetup (show millis) (serviceConnectionPathStr sc) (getConnectionInfo sc) (addServicePathToInput sc $ toJson y) fromJson r0 acc
    loopState Nothing (\x => ((Left . Just) <$> stream)  <+> (Right <$> (fromMaybe neutral $ w <$> x)))

accList : (JsonSerializable t) => Change t -> List t -> List t
accList x xs = 
  let old_val = the (Maybe t) (get "old_val" x)
      new_val = the (Maybe t) (get "new_val" x)
  in case (old_val, new_val) of
          (Nothing, Nothing) => xs
          (Nothing, (Just y)) => xs ++ [y]
          ((Just y), Nothing) => let j = toJson y in filter (\e => toJson e /= j)  xs
          ((Just y), (Just z)) =>  let j = toJson y in replaceWhen (\e => toJson e == j) z xs
  
export
callStreamChangesAccumList : (JsonSerializable a, JsonSerializable (Change b), JsonSerializable b) => 
               SrvRef (StreamService a (Change b)) -> a -> StreamWidget (List b)
callStreamChangesAccumList conn x = 
  callStreamAccum conn x [] accList

export
runWidget : Widget () -> IO ()
runWidget (WidgetPure x) = pure ()
runWidget (WidgetGroup xs) = 
  case simplifyWList xs of
       (Left x) => 
          pure x
       (Right [start]) => 
          do
            n <- createEmptyVNode !body
            start n pure
       (Right xs) => 
          do
            n <- createEmptyVNode !body
            updateVNodes n.children (applyStart pure <$> xs)
runWidget (MarkupWidget start) =
  do
    n <- createEmptyVNode !body
    start n pure
