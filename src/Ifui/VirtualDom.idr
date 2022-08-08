module Ifui.VirtualDom

import Data.List
import Ifui.Dom
import Data.IORef

public export
data AttributeSpec = StringAttr String String 
                   | ValueAttr String

public export
data Attribute = SimpleAttribute AttributeSpec
               | EventListener String (DomEvent -> IO ())

data VAttribute = VSimpleAttribute  AttributeSpec
                | VEventListener String (IO ())

data VNodeRep = VNodeText String | VNodeNode String (List VAttribute)

mutual
  public export
  record VNode where
    constructor MkVNode
    domNode : IORef DomNode
    rep : IORef VNodeRep
    children : VNodes
  public export
  record VNodes where
    constructor MkVNodes
    parent : VNode
    nodes : IORef (List VNode)

export
updateVNodes : VNodes -> List (VNode -> IO ())  -> IO ()

--setNodeChildren : VNode -> 
--export
--data VNode = MkVNode (IORef DomNode) (IORef VNodeRep) (IORef (List VNode))
--
export
setNodeText : VNode -> String -> IO ()
setNodeText node y = 
  do
    n <- readIORef node.domNode
    rep <- readIORef node.rep
    case rep of
         (VNodeText w) => if w == y then pure ()
                                     else do
                                        setTextContent n y
                                        writeIORef node.rep (VNodeText y)
         (VNodeNode _ _) => do
                            new <- createTextNode y
                            replaceWith n new
                            writeIORef node.domNode  (new)
                            writeIORef node.rep (VNodeText y)


export
setNodeTag : VNode -> String -> IO ()
setNodeTag node y = 
  do
    n <- readIORef node.domNode 
    rep <- readIORef node.rep
    case rep of
         (VNodeText w) =>
                         do
                           new <- createElement y
                           replaceWith n new
                           writeIORef node.domNode (new)
                           writeIORef node.rep (VNodeNode y [])
         (VNodeNode w ys) => if w == y then pure ()
                                       else do
                                         new <- createElement y
                                         replaceWith n new
                                         writeIORef node.domNode (new)
                                         writeIORef node.rep (VNodeNode y [])
   
addAttribute : DomNode -> Attribute -> IO VAttribute 
addAttribute n (SimpleAttribute spec) =
 case spec of
     (StringAttr x y) => do
       setAttribute n x y
       pure $ VSimpleAttribute spec
     (ValueAttr x) => do
       setValue n x
       pure $ VSimpleAttribute spec
addAttribute n (EventListener x f) = 
  do
    remove <- addEventListener x f n
    pure $ VEventListener x remove

removeAttribute : DomNode -> VAttribute -> IO ()
removeAttribute n (VSimpleAttribute spec) =
  case spec of
       (StringAttr x y) => setAttribute n x ""
       (ValueAttr x) => setValue n ""
removeAttribute n (VEventListener _ y) = y

updateAttribute : DomNode ->  Attribute -> VAttribute  -> IO VAttribute 
updateAttribute n new@(SimpleAttribute newSpec) old@(VSimpleAttribute oldSpec) = 
  case (newSpec, oldSpec) of
       ((StringAttr x z), (StringAttr y w)) => 
          case (x==y, z==w) of
               (False, s) => do
                               removeAttribute n old
                               addAttribute n new
               (True, False) => do
                                  setAttribute n x z
                                  pure $ VSimpleAttribute newSpec
               (True, True) => 
                              pure old
       ((ValueAttr x), (ValueAttr y)) => 
          if x == y 
             then pure old 
             else do
               setValue n y
               pure $ VSimpleAttribute newSpec
       (_, _) =>
          do
            removeAttribute n old
            addAttribute n new
updateAttribute n new@(EventListener x f) old@(VEventListener y z) =
  do
    removeAttribute n old
    addAttribute n new
updateAttribute n new old = 
  do
    removeAttribute n old
    addAttribute n new

export
setNodeAttributes : VNode -> List Attribute  -> IO ()
setNodeAttributes node xs = 
  do
    n <- readIORef node.domNode 
    rep <- readIORef node.rep
    case rep of
         (VNodeText z) => pure ()
         (VNodeNode z zs) => 
                            do
                              updatedAttrs <- sequence (zipWith (updateAttribute n) xs zs)
                              addedAttrs <- sequence $ addAttribute n <$> drop (length zs) xs
                              sequence_ $ removeAttribute n <$> drop (length xs) zs
                              writeIORef node.rep (VNodeNode z (updatedAttrs ++ addedAttrs))

--export 
--createEmptyVNode : DomNode -> IO VNode
--createEmptyVNode n = 
--  do
--    new <- createElement "span"
--    appendChild n new
--    pure $ MkVNode !(newIORef new) !(newIORef $ VNodeNode "span" []) !(newIORef [])
--
--export
--cleanVNode : VNode -> IO ()
--cleanVNode (MkVNode x _ _) = 
--  readIORef x >>= removeNode 
--
export
setNodeChildren : VNode -> List (VNode -> IO ())  -> IO ()
--setNodeChildren (MkVNode x y ys) xs = 
--  do
--    n <- readIORef x
--    rep <- readIORef y
--    oldNodes <- readIORef ys
--    case rep of
--         (VNodeText z) => pure ()
--         (VNodeNode _ _) => do
--                               sequence_ $ zipWith ($) xs oldNodes 
--                               let updated = take (length xs) oldNodes 
--                               added <- sequence $ (\s => do nn <- createEmptyVNode n; s nn; pure nn) <$> drop (length oldNodes) xs
--                               sequence_ $ cleanVNode <$> drop (length xs) oldNodes
--                               writeIORef ys (updated ++ added)


