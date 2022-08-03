module Ifui.VirtualDom

import Data.List
import Ifui.Dom
import Data.IORef

public export
data Attribute = StringAttr String String 
                | EventListener String (DomEvent -> IO ())

data VAttribute = VStringAttr String String 
                | VEventListener String (IO ())

data VNodeRep = VNodeText String | VNodeNode String (List VAttribute)

export
data VNode = MkVNode (IORef DomNode) (IORef VNodeRep) (IORef (List VNode))


export
setNodeText : VNode -> String -> IO ()
setNodeText (MkVNode x z xs) y = 
  do
    n <- readIORef x
    rep <- readIORef z
    case rep of
         (VNodeText w) => if w == y then pure ()
                                     else do
                                        setTextContent n y
                                        writeIORef z (VNodeText y)
         (VNodeNode _ _) => do
                            new <- createTextNode y
                            replaceWith n new
                            writeIORef x (new)
                            writeIORef z (VNodeText y)


export
setNodeTag : VNode -> String -> IO ()
setNodeTag (MkVNode x z xs) y = 
  do
    n <- readIORef x
    rep <- readIORef z
    case rep of
         (VNodeText w) =>
                         do
                           new <- createElement y
                           replaceWith n new
                           writeIORef x (new)
                           writeIORef z (VNodeNode y [])
         (VNodeNode w ys) => if w == y then pure ()
                                       else do
                                         new <- createElement y
                                         replaceWith n new
                                         writeIORef x (new)
                                         writeIORef z (VNodeNode y [])
   
addAttribute : DomNode -> Attribute -> IO VAttribute 
addAttribute n (StringAttr x y) = 
  do
    setAttribute n x y
    pure $ VStringAttr x y 
addAttribute n (EventListener x f) = 
  do
    remove <- addEventListener x f n
    pure $ VEventListener x remove

removeAttribute : DomNode -> VAttribute -> IO ()
removeAttribute n (VStringAttr x _) = setAttribute n x ""
removeAttribute n (VEventListener _ y) = y

updateAttribute : DomNode ->  Attribute -> VAttribute  -> IO VAttribute 
updateAttribute n new@(StringAttr x z) old@(VStringAttr y w) = 
  case (x==y, z==w) of
       (False, s) => do
                       removeAttribute n old
                       addAttribute n new
       (True, False) => do
                          setAttribute n x z
                          pure $ VStringAttr x z
       (True, True) => 
                      pure old

updateAttribute n new@(StringAttr x z) old@(VEventListener y w) = 
  do
    removeAttribute n old
    addAttribute n new
updateAttribute n new@(EventListener x f) old@(VStringAttr y z) =
  do
    removeAttribute n old
    addAttribute n new
updateAttribute n new@(EventListener x f) old@(VEventListener y z) =
  do
    removeAttribute n old
    addAttribute n new

export
setNodeAttributes : VNode -> List Attribute  -> IO ()
setNodeAttributes (MkVNode x y _) xs = 
  do
    n <- readIORef x
    rep <- readIORef y
    case rep of
         (VNodeText z) => pure ()
         (VNodeNode z zs) => 
                            do
                              updatedAttrs <- sequence (zipWith (updateAttribute n) xs zs)
                              addedAttrs <- sequence $ addAttribute n <$> drop (length zs) xs
                              sequence_ $ removeAttribute n <$> drop (length xs) zs
                              writeIORef y (VNodeNode z (updatedAttrs ++ addedAttrs))

export 
createEmptyVNode : DomNode -> IO VNode
createEmptyVNode n = 
  do
    new <- createElement "span"
    appendChild n new
    pure $ MkVNode !(newIORef new) !(newIORef $ VNodeNode "span" []) !(newIORef [])

export
cleanVNode : VNode -> IO ()
cleanVNode (MkVNode x _ _) = 
  readIORef x >>= removeNode 

export
setNodeChildren : VNode -> List (VNode -> IO ())  -> IO ()
setNodeChildren (MkVNode x y ys) xs = 
  do
    n <- readIORef x
    rep <- readIORef y
    oldNodes <- readIORef ys
    case rep of
         (VNodeText z) => pure ()
         (VNodeNode _ _) => do
                               sequence_ $ zipWith ($) xs oldNodes 
                               let updated = take (length xs) oldNodes 
                               added <- sequence $ (\s => do nn <- createEmptyVNode n; s nn; pure nn) <$> drop (length oldNodes) xs
                               sequence_ $ cleanVNode <$> drop (length xs) oldNodes
                               writeIORef ys (updated ++ added)


