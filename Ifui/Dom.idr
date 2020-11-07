module Ifui.Dom

export
data DomNode = MkNode AnyPtr

export
data DomEvent = MkEvent AnyPtr

%foreign "javascript:lambda: tag => document.createElement(tag)"
prim__createElement : String -> PrimIO AnyPtr
export
createElement : HasIO io => String -> io DomNode
createElement tag = MkNode <$> (primIO $ prim__createElement tag)

%foreign "javascript:lambda: (node, child) => node.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()
export
appendChild : HasIO io => DomNode -> DomNode -> io ()
appendChild (MkNode n) (MkNode c) = primIO $ prim__appendChild n c

%foreign "javascript:lambda: () => document.body"
prim__body : () -> PrimIO AnyPtr
export
body : HasIO io => io DomNode
body = MkNode <$> (primIO $ prim__body ())

%foreign "javascript:lambda: (x, n) => {n.textContent = x}"
prim__setTextContent : String -> AnyPtr -> PrimIO ()
export
setTextContent : HasIO io => String -> DomNode -> io ()
setTextContent s (MkNode n) = primIO $ prim__setTextContent s n

%foreign "javascript:lambda: (event, callback, node) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : String -> (AnyPtr -> PrimIO ()) -> AnyPtr -> PrimIO ()
export
addEventListener : HasIO io => String -> (DomEvent -> IO ()) -> DomNode -> io ()
addEventListener event callback (MkNode n) =
  primIO $ prim__addEventListener event (\ptr => toPrim  $ callback $ MkEvent ptr) n

%foreign "javascript:lambda: e => e.target.value"
prim__targetValue : AnyPtr -> PrimIO String
export
targetValue : HasIO io => DomEvent -> io String
targetValue (MkEvent x) = primIO $ prim__targetValue x

%foreign "javascript:lambda: n => n.children"
prim__getChildren : AnyPtr -> PrimIO AnyPtr
%foreign "javascript:lambda: n => n.length"
prim__length : AnyPtr -> PrimIO Int
%foreign "javascript:lambda: (i,xs) => xs[i]"
prim__get : Int -> AnyPtr -> PrimIO AnyPtr
export
getChildren : HasIO io => DomNode -> io (List DomNode)
getChildren (MkNode x) =
  do
    childrenPtr <- primIO $ prim__getChildren x
    len <- primIO $ prim__length childrenPtr
    nodesPtr <- traverse (\i => primIO $ prim__get i childrenPtr) [0..len]
    pure $ map MkNode nodesPtr

%foreign "javascript:lambda: n => n.firstElementChild"
prim__firstElementChild : AnyPtr -> PrimIO AnyPtr
export
firstElementChild : HasIO io => DomNode -> io DomNode
firstElementChild (MkNode x) = map MkNode $ primIO $ prim__firstElementChild x

%foreign "javascript:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()
export
consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x
