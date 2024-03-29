module Ifui.WebSocketsClient

export
data WebSocket = MkWebSockest AnyPtr

export
data WsEvent = MkWsEvent AnyPtr


%foreign "browser:lambda: url => new WebSocket(url)"
prim__createWebSocket : String -> PrimIO AnyPtr 
export
createWebSocket : HasIO io => String -> io WebSocket
createWebSocket url = MkWebSockest <$> (primIO $ prim__createWebSocket url)


%foreign "browser:lambda: (ws, onMessage) => {ws.onmessage = (evt) => onMessage(evt)()}"
prim__setOnMessage: AnyPtr -> (AnyPtr -> PrimIO ()) -> PrimIO ()
export
setOnMessage : WebSocket -> (WsEvent -> IO ()) -> IO ()
setOnMessage (MkWebSockest ws) onMessage  = 
  primIO $ prim__setOnMessage ws (\msg => toPrim $ onMessage $ MkWsEvent msg)

%foreign "browser:lambda: (ws, onOpen) => {ws.onopen = () => onOpen()}"
prim__setOnOpen: AnyPtr -> (PrimIO ()) -> PrimIO ()
export
setOnOpen : WebSocket -> (IO ()) -> IO ()
setOnOpen (MkWebSockest ws) onOpen = 
  primIO $ prim__setOnOpen ws (toPrim onOpen)

%foreign "browser:lambda: (ws, msg) => ws.send(msg)"
prim__send : AnyPtr -> String -> PrimIO ()
export
send : HasIO io => WebSocket -> String -> io ()
send (MkWebSockest ws) y = primIO $ prim__send ws y


%foreign "browser:lambda: evt => evt.data"
prim__eventData : AnyPtr -> PrimIO String
export
eventData : HasIO io => WsEvent -> io String
eventData (MkWsEvent e) = primIO $ prim__eventData e

%foreign "browser:lambda: (ws) => ws.close()"
prim__close : AnyPtr -> PrimIO ()
export
close : HasIO io => WebSocket -> io ()
close (MkWebSockest ws) = primIO $ prim__close ws
