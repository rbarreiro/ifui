module IfuiServer.Server

import public IfuiServer.Promise
import Language.JSON
import IfuiServer.WebSockets
import IfuiServer.Http
import System
import System.File
import public Ifui.Services

public export
data Service : String -> ServiceKind -> Type where
  MkRPC : (JsonSerializable a, JsonSerializable b) => (s : String) -> (a -> Promise b)  -> Service s (RPC a b)

public export
data Server : List (String, ServiceKind) -> Type where
  Nil : Server []
  (::) : Service s spec -> Server ts -> Server ((s, spec) :: ts)


--export
--serviceRPC : (JsonSerializable a, JsonSerializable b) => (s : String) -> (a -> Promise b)  -> Service s (RPC a b)
--serviceRPC = MkRPC toJson fromJson toJson fromJson

getServiceU: (n : String) -> Server ts -> Maybe (k : ServiceKind ** Service n k)
getServiceU n [] = 
  Nothing
getServiceU n ((MkRPC {a} {b} s f) :: y) = 
  if n == s then Just (RPC a b ** MkRPC n f)
            else getServiceU n y

export
startWsServer : Int -> Server ts -> IO ()
startWsServer port server =
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      setOnMessageStr wsc $ \msg => do
        case Language.JSON.parse msg of
             Just (JArray [JString srv, JString i, x]) => 
              case getServiceU srv server of
                   Just ((RPC a b) ** (MkRPC srv f)) => 
                      case the (Maybe a) (fromJson x) of
                           Just y =>
                            do
                              _ <- (f y).run (\z => wsSend wsc (show (JArray [JString i, toJson z])))
                              pure ()
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
serveStatic : Int -> String -> IO ()
serveStatic port path = 
  do
    staticServer <- createStaticServer path
    s <- createHttpServer $ \req, res => serve staticServer req res
    listen s port
      

