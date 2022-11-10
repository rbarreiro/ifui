module IfuiServer.WebSockets

export
data WsConnection = MkWsConnection AnyPtr

export
data WsServer = MkWsServer AnyPtr

%foreign "node:lambda: port => {const WebSocket = require('ws'); return new WebSocket.Server({port : port})}"
prim__startWebSocketsServer : Int -> PrimIO AnyPtr 
export
startWebSocketsServer : HasIO io => Int -> io WsServer
startWebSocketsServer port = MkWsServer <$> (primIO $ prim__startWebSocketsServer port)
  
%foreign "node:lambda: (wss, onConnection) => wss.on('connection', (ws) => onConnection(ws)()) "
prim__setOnConnection : AnyPtr -> (AnyPtr -> PrimIO ()) -> PrimIO ()
export
setOnConnection : WsServer -> (WsConnection -> IO ()) -> IO ()
setOnConnection (MkWsServer wss) onConnection  = 
  primIO $ prim__setOnConnection wss  (\ptr => toPrim $ onConnection $ MkWsConnection ptr)

%foreign "node:lambda: (ws, msg) => ws.send(msg)"
prim__wsSend : AnyPtr -> String -> PrimIO ()
export
wsSend : HasIO io => WsConnection -> String -> io ()
wsSend (MkWsConnection ws) y = primIO $ prim__wsSend ws y

%foreign "node:lambda: (ws, onMessage) => ws.on('message', (msg) => onMessage(msg)())"
prim__setOnMessage: AnyPtr -> (String -> PrimIO ()) -> PrimIO ()
export
setOnMessage : WsConnection -> (String -> IO ()) -> IO ()
setOnMessage (MkWsConnection ws) onMessage  = 
  primIO $ prim__setOnMessage ws (\msg => toPrim $ onMessage msg)
