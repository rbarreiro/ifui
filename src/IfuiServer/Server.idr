module IfuiServer.Server

import public Ifui.Promise
import public Ifui.IOStream 
import public Ifui.Services
import Ifui.ExtensibleTypes
import Language.JSON
import IfuiServer.WebSockets
import IfuiServer.Http
import System
import System.File
import Data.IORef
import Data.List

public export
data Service : ServiceKind -> Type where
  MkRPC : (JsonSerializable a, JsonSerializable b) => (a -> Promise b)  -> Service (RPC a b)
  MkStreamService : (JsonSerializable a, JsonSerializable b) => (a -> IOStream b) -> Service (StreamService a b)
  MkGroupService : {xs : Vect n (String, ServiceKind)} -> {auto 0 prf : So (UniqueKeys xs)} ->  Record (mapValues Service xs) -> Service (GroupService xs)
  MkEmptyService : Service EmptyService

public export
data ServerWithAuth : (loginTy : Type) -> (roleTy : Type) -> (roleTy -> ServiceKind) -> Type where
  MkServerWithAuth : (loginTy -> Promise roleTy) -> ((r:roleTy) -> Service (sf r)) -> ServerWithAuth loginTy roleTy sf

getServiceU: (n : String) -> (xs : Vect m (String, ServiceKind)) -> Record (mapValues Service xs) -> Maybe (k : ServiceKind ** Service k)
getServiceU n [] x = 
  Nothing
getServiceU n ((y, z) :: l) ((MkEntry y x) :: w) =
  if n == y then Just (z ** x)
            else getServiceU n l w

removeHandle : String -> IORef (List (String, a)) -> IO ()
removeHandle str x = 
 do
   h <- readIORef x
   writeIORef x (deleteBy (\x, (y,_) => x == y)  str h)

runService : IORef (List (String, IO ())) -> WsConnection -> Service st -> String -> JSON -> IO ()
runService cancelHandles wsc service i x =
  case service of
       MkEmptyService =>
           putStrLn "Not possible to call EmptyService"
       (MkRPC {a} {b} f) => 
           case the (Maybe a) (fromJson x) of
                Just y =>
                 do
                   promiseh <- (f y).run (\z => do removeHandle i cancelHandles; wsSend wsc (show (JArray [JString i, toJson z])))
                   h <- readIORef cancelHandles 
                   writeIORef cancelHandles ((i, promiseh.cancel) :: h)
                Nothing =>
                 putStrLn "Invalid Input \{show x}"
       (MkStreamService {a} {b} f) => 
           case the (Maybe a) (fromJson x) of
                Just y =>
                 do
                   streamh <- (f y).run (\z => wsSend wsc (show (JArray [JString i, toJson z])))
                   h <- readIORef cancelHandles 
                   writeIORef cancelHandles ((i, streamh.cancel) :: h)
                Nothing =>
                 putStrLn "Invalid Input \{show x}"
       (MkGroupService {xs} y) => 
           case x of
               JArray [JString subs, z] =>
                  case getServiceU subs xs y of
                    Just (k ** w) =>
                      runService cancelHandles wsc w i z
                    Nothing =>
                      putStrLn "Invalid Sub Service \{subs}"
               o =>
                  putStrLn "Invalid subservice format \{show o}"  

onMessageFn : IORef (List (String, IO ())) -> Service st -> WsConnection -> String -> IO()
onMessageFn cancelHandles service wsc msg =
  case Language.JSON.parse msg of
       Just (JArray [JString "cancel", JString i]) => 
        case lookup i !(readIORef cancelHandles) of
             Nothing => 
               pure ()
             (Just x) =>
               do
                 removeHandle i cancelHandles
                 x
       Just (JArray [JString "call", JString i, x]) => 
          runService cancelHandles wsc service i x
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
                                wsSend wsc (stringify w)
                              pure ()
                            Nothing => 
                              putStrLn "Invalid login info \{msg}"
                     Nothing =>
                         putStrLn "Invalid login info \{msg}"
             Just role => 
                onMessageFn cancelHandles (getServices role) wsc msg

export
startWsServer : Int -> Service st -> IO ()
startWsServer port service =
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      cancelHandles <- newIORef $ the (List (String, IO ()))  []
      setOnMessageStr wsc (onMessageFn cancelHandles service wsc)

export
serveStatic : Int -> String -> IO ()
serveStatic port path = 
  do
    staticServer <- createStaticServer path
    s <- createHttpServer $ \req, res => serve staticServer req res
    listen s port
      

%foreign "node:lambda: () => {const crypto=require('crypto'); return crypto.randomUUID()}"
prim__randomUUID : () -> PrimIO String
export
randomUUID : IO String
randomUUID = primIO $ prim__randomUUID ()


