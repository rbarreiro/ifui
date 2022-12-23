module IfuiServer.Server

import public IfuiServer.Promise
import public IfuiServer.IOStream 
import public Ifui.Services
import Ifui.ExtensibleRecords
import Language.JSON
import IfuiServer.WebSockets
import IfuiServer.Http
import System
import System.File
import Data.IORef
import Data.List

public export
data Service : String -> ServiceKind -> Type where
  MkRPC : (JsonSerializable a, JsonSerializable b) => (s : String) -> (a -> Promise b)  -> Service s (RPC a b)
  MkStreamService : (JsonSerializable a, JsonSerializable b) => (s : String) -> (a -> IOStream b)  -> Service s (StreamService a b)

public export
data Server : UKeyList String ServiceKind -> Type where
  Nil : Server []
  (::) : {auto p : UKeyListCanPrepend (s, spec) ts} -> Service s spec -> Server ts -> Server ((s, spec) :: ts)

public export
data ServerWithAuth : (loginTy : Type) -> (roleTy : Type) -> (roleTy -> UKeyList String ServiceKind) -> Type where
  MkServerWithAuth : (loginTy -> Promise roleTy) -> ((r:roleTy) -> Server (sf r)) -> ServerWithAuth loginTy roleTy sf

getServiceU: (n : String) -> Server ts -> Maybe (k : ServiceKind ** Service n k)
getServiceU n [] = 
  Nothing
getServiceU n ((MkRPC {a} {b} s f) :: y) = 
  if n == s then Just (RPC a b ** MkRPC n f)
            else getServiceU n y
getServiceU n ((MkStreamService {a} {b} s f) :: y) = 
  if n == s then Just (StreamService a b ** MkStreamService n f)
            else getServiceU n y

removeHandle : String -> IORef (List (String, a)) -> IO ()
removeHandle str x = 
 do
   h <- readIORef x
   writeIORef x (deleteBy (\x, (y,_) => x == y)  str h)

onMessageFn : IORef (List (String, IO ())) -> Server ts -> WsConnection -> String -> IO()
onMessageFn cancelHandles server wsc msg =
  case Language.JSON.parse msg of
       Just (JArray [JString "cancel", JString i]) => 
        case lookup i !(readIORef cancelHandles) of
             Nothing => 
               pure ()
             (Just x) =>
               do
                 removeHandle i cancelHandles
                 x
       Just (JArray [JString srv, JString i, x]) => 
        case getServiceU srv server of
             Just ((RPC a b) ** (MkRPC srv f)) => 
                case the (Maybe a) (fromJson x) of
                     Just y =>
                      do
                        promiseh <- (f y).run (\z => do removeHandle i cancelHandles; wsSend wsc (show (JArray [JString i, toJson z])))
                        h <- readIORef cancelHandles 
                        writeIORef cancelHandles ((i, promiseh.cancel) :: h)
                     Nothing =>
                      putStrLn "Invalid Input to service \{srv} \{show x}"
             Just ((StreamService a b) ** (MkStreamService srv f)) => 
                case the (Maybe a) (fromJson x) of
                     Just y =>
                      do
                        streamh <- (f y).run (\z => wsSend wsc (show (JArray [JString i, toJson z])))
                        h <- readIORef cancelHandles 
                        writeIORef cancelHandles ((i, streamh.cancel) :: h)
                     Nothing =>
                      putStrLn "Invalid Input to service \{srv} \{show x}"
             Nothing =>
              putStrLn "Invalid Service \{srv}"
       Nothing => 
        do
          putStrLn "Error parsing request \{msg}"
       o => 
        putStrLn "Invalid request \{show o}"

export
startWsServerWithAuth : (JsonSerializable l, JsonSerializable r)  => Int -> ServerWithAuth l r sf -> IO ()
startWsServerWithAuth port (MkServerWithAuth checkLogin getServices) = 
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      roleRef <- newIORef $ the (Maybe r) Nothing
      cancelHandles <- newIORef $ the (List (String, IO ()))  []
      setOnMessageStr wsc $ \msg => do
        mrole <- readIORef roleRef
        case mrole of
             Nothing => 
                case Language.JSON.parse msg of
                     Just j =>
                       case the (Maybe l) (fromJson j) of
                            Just login => do
                              _ <- (checkLogin login).run $ \w => do
                                writeIORef roleRef (Just w)
                                wsSend wsc (show $ stringify w)
                              pure ()
                            Nothing => 
                              putStrLn "Invalid login info \{msg}"
                     Nothing =>
                         putStrLn "Invalid login info \{msg}"
             Just role => 
                onMessageFn cancelHandles (getServices role) wsc msg

export
startWsServer : Int -> Server ts -> IO ()
startWsServer port server =
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      cancelHandles <- newIORef $ the (List (String, IO ()))  []
      setOnMessageStr wsc (onMessageFn cancelHandles server wsc)

export
serveStatic : Int -> String -> IO ()
serveStatic port path = 
  do
    staticServer <- createStaticServer path
    s <- createHttpServer $ \req, res => serve staticServer req res
    listen s port
      

