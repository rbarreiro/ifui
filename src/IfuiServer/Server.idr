module IfuiServer.Server

import public IfuiServer.Promise
import Language.JSON
import IfuiServer.WebSockets
import IfuiServer.Http
import System
import System.File
import public Ifui.Services


export
serviceRPC : (JsonSerializable a, JsonSerializable b) => (s : String) -> (a -> Promise b)  -> Service s (RPC a b)
serviceRPC = MkRPC toJson fromJson toJson fromJson

getServiceU: (n : String) -> Server ts -> Maybe (k : ServiceKind ** Service n k)
getServiceU n [] = 
  Nothing
getServiceU n ((MkRPC {a} {b} a2j j2a b2j j2b s f) :: y) = 
  if n == s then Just (RPC a b ** MkRPC a2j j2a b2j j2b n f)
            else getServiceU n y

export
startWsServer : Int -> Server ts -> IO ()
startWsServer port server =
  do
    wss <- startWebSocketsServer port
    setOnConnection wss $ \wsc => do
      setOnMessage wsc $ \msg => do
        case parse msg of
             Just (JArray [JString srv, JString i, x]) => 
              case getServiceU srv server of
                   Just ((RPC a b) ** (MkRPC a2j j2a b2j j2b srv f)) => 
                      case the (Maybe a) (j2a x) of
                           Just y =>
                            do
                              _ <- (f y).run (\z => wsSend wsc (show (JArray [JString i, b2j z])))
                              pure ()
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
      

