module IfuiServer.Server

import public Ifui.Json
import public IfuiServer.Promise
import Language.JSON
import IfuiServer.WebSockets
import IfuiServer.Http
import System
import System.File

public export
data ServiceKind = RPC Type Type
                 -- | StreamService Type Type

public export
data Service : String -> ServiceKind -> Type where
  MkRPC : (JsonSerializable a, JsonSerializable b) => (0 s : String) -> (a -> Promise b)  -> Service s (RPC a b)

public export
data Server : List (String, ServiceKind) -> Type where
  Nil : Server []
  (::) : Service s spec -> Server ts -> Server ((s, spec) :: ts)

getService : (s : String) -> Server ts -> Maybe (k : ServiceKind ** Service s k)

export
startWsServer : Int -> Server ts -> IO ()
startWsServer port server =
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      setOnMessage wsc $ \msg => do
        case parse msg of
             Just (JArray [JString srv, JString i, x]) => 
              case getService srv server of
                   Just ((RPC a b) ** (MkRPC srv f)) => 
                      case the (Maybe a) (fromJson x) of
                           Just y =>
                            (f y).run (\z => wsSend wsc (show (JArray [JString i, toJson z])))
                           Nothing =>
                            putStrLn ("Invalid Input to service " ++ srv ++ " " ++ show x)
                   Nothing =>
                    putStrLn ("Invalid Service" ++ srv)
             o => 
              putStrLn ("Invalid request" ++ show o)


export
serveStatic : Int -> String -> IO ()
serveStatic port path = 
  do
    staticServer <- createStaticServer path
    s <- createHttpServer $ \req, res => serve staticServer req res
    listen s port
      

