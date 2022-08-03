module Ifui.Dom

export
data DomNode = MkNode AnyPtr

%name DomNode n

export
data DomEvent = MkEvent AnyPtr

%foreign "browser:lambda: x => console.log(x)"
prim__consoleLog : AnyPtr -> PrimIO ()
export
consoleLog : HasIO io => a -> io ()
consoleLog x = primIO $ prim__consoleLog (believe_me x)

%foreign "browser:lambda: tag => document.createElement(tag)"
prim__createElement : String -> PrimIO AnyPtr
export
createElement : HasIO io => String -> io DomNode
createElement tag = MkNode <$> (primIO $ prim__createElement tag)

%foreign "browser:lambda: txt => document.createTextNode(txt)"
prim__createTextNode : String -> PrimIO AnyPtr
export
createTextNode : HasIO io => String -> io DomNode
createTextNode txt = MkNode <$> (primIO $ prim__createTextNode txt)

%foreign "browser:lambda: (node, child) => node.appendChild(child)"
prim__appendChild : AnyPtr -> AnyPtr -> PrimIO ()
export
appendChild : HasIO io => DomNode -> DomNode -> io ()
appendChild (MkNode n) (MkNode c) = primIO $ prim__appendChild n c

%foreign "browser:lambda: (node, replacement) => node.replaceWith(replacement)"
prim__replaceWith : AnyPtr -> AnyPtr -> PrimIO ()
export
replaceWith : HasIO io => DomNode -> DomNode -> io ()
replaceWith (MkNode n) (MkNode r) = primIO $ prim__replaceWith n r

%foreign "browser:lambda: () => document.body"
prim__body : () -> PrimIO AnyPtr
export
body : HasIO io => io DomNode
body = MkNode <$> (primIO $ prim__body ())

%foreign "browser:lambda: (n, k, v) => n.setAttribute(k,v)"
prim__setAttribute : AnyPtr -> String -> String -> PrimIO ()
export
setAttribute : HasIO io => DomNode -> String -> String -> io ()
setAttribute (MkNode n) k v = primIO $ prim__setAttribute n k v

%foreign "browser:lambda: (n, k) => n.getAttribute(k)"
prim__getAttribute : AnyPtr -> String -> PrimIO String
export
getAttribute : HasIO io => DomNode -> String -> io String
getAttribute (MkNode n) k = primIO $ prim__getAttribute n k

%foreign "browser:lambda: n => n.value"
prim__getValue : AnyPtr -> PrimIO String
export
getValue : HasIO io => DomNode -> io String
getValue (MkNode n) = primIO $ prim__getValue n

%foreign "browser:lambda: (n, v) => n.value = v"
prim__setValue : AnyPtr -> String -> PrimIO ()
export
setValue : HasIO io => DomNode -> String -> io ()
setValue (MkNode n) v = primIO $ prim__setValue n v

%foreign "browser:lambda: (event, callback, node) =>{const f = x=>callback(x)(); node.addEventListener(event, f); return () => node.removeEventListener(event, f);}"
prim__addEventListener : String -> (AnyPtr -> PrimIO ()) -> AnyPtr -> PrimIO (PrimIO ())
export
addEventListener : HasIO io => String -> (DomEvent -> IO ()) -> DomNode -> io (IO ())
addEventListener event callback (MkNode n) =
  do
    remove <- primIO $ prim__addEventListener event (\ptr => toPrim  $ callback $ MkEvent ptr) n
    pure $ primIO remove

%foreign "browser:lambda: e => e.target.value"
prim__targetValue : AnyPtr -> PrimIO String
export
targetValue : HasIO io => DomEvent -> io String
targetValue (MkEvent x) = primIO $ prim__targetValue x

%foreign "browser:lambda: n => BigInt(n.length)"
prim__length : AnyPtr -> PrimIO Int
%foreign "browser:lambda: (i,xs) => xs[i]"
prim__get : Int -> AnyPtr -> PrimIO AnyPtr

%foreign "browser:lambda: n => n.children"
prim__getChildren : AnyPtr -> PrimIO AnyPtr
export
getChildren : HasIO io => DomNode -> io (List DomNode)
getChildren (MkNode x) =
  do
    childrenPtr <- primIO $ prim__getChildren x
    len <- primIO $ prim__length childrenPtr
    nodesPtr <- traverse (\i => primIO $ prim__get i childrenPtr) [0..(len-1)]
    pure $ map MkNode nodesPtr

%foreign "browser:lambda: n => n.childNodes"
prim__getChildNodes : AnyPtr -> PrimIO AnyPtr
export
getChildNodes : HasIO io => DomNode -> io (List DomNode)
getChildNodes (MkNode x) =
  do
    childsPtr <- primIO $ prim__getChildNodes x
    len <- primIO $ prim__length childsPtr
    nodesPtr <- traverse (\i => primIO $ prim__get i childsPtr) [0..(len-1)]
    pure $ map MkNode nodesPtr

%foreign "browser:lambda: n => n.firstElementChild"
prim__firstElementChild : AnyPtr -> PrimIO AnyPtr
export
firstElementChild : HasIO io => DomNode -> io DomNode
firstElementChild (MkNode x) = map MkNode $ primIO $ prim__firstElementChild x

%foreign "browser:lambda: n => n.firstChild"
prim__firstChild : AnyPtr -> PrimIO AnyPtr
export
firstChild : HasIO io => DomNode -> io DomNode
firstChild (MkNode x) = map MkNode $ primIO $ prim__firstChild x

%foreign "browser:lambda: n => n.innerHTML=''"
prim__removeAllChildren : AnyPtr -> PrimIO ()
export
removeAllChildren : HasIO io => DomNode -> io ()
removeAllChildren (MkNode x) = primIO $ prim__removeAllChildren x

%foreign "browser:lambda: (n, t) => n.textContent=t"
prim__setTextContent : AnyPtr -> String -> PrimIO ()
export
setTextContent : HasIO io => DomNode -> String -> io ()
setTextContent (MkNode x) s = primIO $ prim__setTextContent x s

%foreign "browser:lambda: n => n.remove()"
prim__removeNode : AnyPtr -> PrimIO ()
export
removeNode : HasIO io => DomNode -> io ()
removeNode (MkNode x) = primIO $ prim__removeNode x
