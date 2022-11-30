module Ifui.VirtualDom

import Data.List
import Ifui.Dom
import Data.IORef

public export
data AttributeSpec = StringAttr String String 
                   | ValueAttr String
                   | CSSClassAttr String

public export
data Attribute = SimpleAttribute AttributeSpec
               | EventListener String (DomEvent -> IO ())

data VAttribute = VSimpleAttribute  AttributeSpec
                | VEventListener String (IO ())

data VNodeRep = VNodeText String 
              | VNodeNode String (List VAttribute) 
              | VNodePromise String (IO ()) (IORef Bool) -- id to ckeck if is the same, cancel and is done flag

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
    parent : IORef DomNode
    nodes : IORef (List VNode)

export
createEmptyVNodes : DomNode -> IO VNodes
createEmptyVNodes n =
  pure $ MkVNodes !(newIORef n) !(newIORef [])


export 
createEmptyVNode : DomNode -> IO VNode
createEmptyVNode n = 
  do
    new <- createElement "span"
    appendChild n new
    domNode <- newIORef new
    pure $ MkVNode domNode !(newIORef $ VNodeNode "span" []) (MkVNodes domNode !(newIORef []))

export
cleanVNode : VNode -> IO ()
cleanVNode (MkVNode x _ _) = 
  readIORef x >>= removeNode 

export
updateVNodes : VNodes -> List (VNode -> IO ())  -> IO ()
updateVNodes ns xs = 
  do
    n <- readIORef ns.parent
    oldNodes <- readIORef ns.nodes
    sequence_ $ zipWith ($) xs oldNodes
    added <- sequence $ (\s => do nn <- createEmptyVNode n; s nn; pure nn) <$> drop (length oldNodes) xs
    sequence_ $ cleanVNode <$> drop (length xs) oldNodes
    writeIORef ns.nodes (take (length xs) oldNodes  ++ added)

export
setNodeText : VNode -> String -> IO ()
setNodeText node y = 
  do
    rep <- readIORef node.rep
    case rep of
         (VNodeText w) => if w == y then pure ()
                                     else do
                                        n <- readIORef node.domNode
                                        setTextContent n y
                                        writeIORef node.rep (VNodeText y)
         (VNodeNode _ _) => createNewNodeText
         (VNodePromise _ cancel _) => cancel >> createNewNodeText
  where
    createNewNodeText : IO ()
    createNewNodeText = 
      do
        n <- readIORef node.domNode
        new <- createTextNode y
        replaceWith n new
        writeIORef node.domNode  (new)
        writeIORef node.rep (VNodeText y)

export
setNodeTag : VNode -> String -> IO ()
setNodeTag node y = 
  do
    rep <- readIORef node.rep
    case rep of
         (VNodeText w) => createNewNodeTag
         (VNodeNode w ys) => if w == y then pure ()
                                       else createNewNodeTag
         (VNodePromise _ cancel _) => cancel >> createNewNodeTag
  where
    createNewNodeTag : IO ()
    createNewNodeTag = 
      do
        n <- readIORef node.domNode 
        new <- createElement y
        replaceWith n new
        writeIORef node.domNode (new)
        writeIORef node.rep (VNodeNode y [])

export
setNodePromise : VNode -> String -> IORef Bool -> IO (IO ()) -> IO ()
setNodePromise node id isFinished start = 
  do
    rep <- readIORef node.rep
    case rep of
         (VNodeText w) => 
                         createNewNodePromise
         (VNodeNode w ys) => 
                            createNewNodePromise
         (VNodePromise oldId oldCancel oldIsFinished) => 
                                                        if oldId == id then
                                                                       do
                                                                         done <- readIORef oldIsFinished
                                                                         if done then createNewNodePromise 
                                                                                 else pure ()
                                                                       else createNewNodePromise 

  where
    createNewNodePromise : IO ()
    createNewNodePromise = 
      do
        n <- readIORef node.domNode 
        new <- createElement "span"
        replaceWith n new
        writeIORef node.domNode (new)
        cancel <- start
        writeIORef node.rep (VNodePromise id cancel isFinished) 


addAttribute : DomNode -> Attribute -> IO VAttribute 
addAttribute n (SimpleAttribute spec) =
 case spec of
     (StringAttr x y) => do
       setAttribute n x y
       pure $ VSimpleAttribute spec
     (ValueAttr x) => do
       setValue n x
       pure $ VSimpleAttribute spec
     (CSSClassAttr x) => do
       addClass n x
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
       (CSSClassAttr x) => removeClass n x
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
               setValue n x
               pure $ VSimpleAttribute newSpec
       ((CSSClassAttr x), (CSSClassAttr y)) => 
          if x == y 
             then pure old 
             else do
                removeAttribute n old
                addAttribute n new
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
         (VNodeText _) => pure ()
         (VNodePromise _ _ _) => pure ()
         (VNodeNode z zs) => 
                            do
                              updatedAttrs <- sequence (zipWith (updateAttribute n) xs zs)
                              addedAttrs <- sequence $ addAttribute n <$> drop (length zs) xs
                              sequence_ $ removeAttribute n <$> drop (length xs) zs
                              writeIORef node.rep (VNodeNode z (updatedAttrs ++ addedAttrs))



