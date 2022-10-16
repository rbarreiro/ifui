module IfuiServer.WebSockets

export
data WSConnection = MkWsConnection AnyPtr

export
data WsServer = MkWsServer AnyPtr

%foreign "node:lambda: port => require('ws').WebSocket.Server({port : port})"
prim__startWebCocketsServer : Int -> PrimIO AnyPtr 
startWebSocketsServer : HasIO io => Int -> io WsServer
startWebSocketsServer port = MkWsServer <$> (primIO $ prim__startWebCocketsServer port)
  
%foreign "node:lambda: (wss, onConnection) => wss.on('connection', onConnection) "
prim__setOnConnection : AnyPtr -> (AnyPtr -> PrimIO ()) -> PrimIO ()
setOnConnection : WsServer -> (WSConnection -> IO ()) -> IO ()
setOnConnection (MkWsServer wss) onConnection  = 
  primIO $ prim__setOnConnection wss  (\ptr => toPrim $ onConnection $ MkWsConnection ptr)

%foreign "node:lambda: (ws, msg) => ws.send(msg)"
prim__wsSend : AnyPtr -> String -> PrimIO ()
wsSend : HasIO io => WSConnection -> String -> io ()
wsSend (MkWsConnection ws) y = primIO $ prim__wsSend ws y

