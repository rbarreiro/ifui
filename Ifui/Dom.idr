module Ifui.Dom

export
data DomNode = MkNode AnyPtr

export
data DomEvent = MkEvent AnyPtr

%foreign "browser:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()
export
consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x

%foreign "browser:lambda: tag => document.createElement(tag)"
prim__createElement : String -> PrimIO AnyPtr
export
createElement : HasIO io => String -> io DomNode
createElement tag = MkNode <$> (primIO $ prim__createElement tag)

%foreign "browser:lambda: (node, child) => node.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()
export
appendChild : HasIO io => DomNode -> DomNode -> io ()
appendChild (MkNode n) (MkNode c) = primIO $ prim__appendChild n c

%foreign "browser:lambda: () => document.body"
prim__body : () -> PrimIO AnyPtr
export
body : HasIO io => io DomNode
body = MkNode <$> (primIO $ prim__body ())

%foreign "browser:lambda: (x, n) => {n.textContent = x}"
prim__setTextContent : String -> AnyPtr -> PrimIO ()
export
setTextContent : HasIO io => String -> DomNode -> io ()
setTextContent s (MkNode n) = primIO $ prim__setTextContent s n

%foreign "browser:lambda: (event, callback, node) => node.addEventListener(event, x=>callback(x)())"
prim__addEventListener : String -> (AnyPtr -> PrimIO ()) -> AnyPtr -> PrimIO ()
export
addEventListener : HasIO io => String -> (DomEvent -> IO ()) -> DomNode -> io ()
addEventListener event callback (MkNode n) =
  primIO $ prim__addEventListener event (\ptr => toPrim  $ callback $ MkEvent ptr) n

%foreign "browser:lambda: e => e.target.value"
prim__targetValue : AnyPtr -> PrimIO String
export
targetValue : HasIO io => DomEvent -> io String
targetValue (MkEvent x) = primIO $ prim__targetValue x

%foreign "browser:lambda: n => n.children"
prim__getChildren : AnyPtr -> PrimIO AnyPtr
%foreign "browser:lambda: n => BigInt(n.length)"
prim__length : AnyPtr -> PrimIO Int
%foreign "browser:lambda: (i,xs) => xs[i]"
prim__get : Int -> AnyPtr -> PrimIO AnyPtr
export
getChildren : HasIO io => DomNode -> io (List DomNode)
getChildren (MkNode x) =
  do
    childrenPtr <- primIO $ prim__getChildren x
    len <- primIO $ prim__length childrenPtr
    nodesPtr <- traverse (\i => primIO $ prim__get i childrenPtr) [0..(len-1)]
    pure $ map MkNode nodesPtr

%foreign "browser:lambda: n => n.firstElementChild"
prim__firstElementChild : AnyPtr -> PrimIO AnyPtr
export
firstElementChild : HasIO io => DomNode -> io DomNode
firstElementChild (MkNode x) = map MkNode $ primIO $ prim__firstElementChild x

%foreign "browser:lambda: n => n.innerHTML=''"
prim__removeAllChildren : AnyPtr -> PrimIO ()
export
removeAllChildren : HasIO io => DomNode -> io ()
removeAllChildren (MkNode x) = primIO $ prim__removeAllChildren x
