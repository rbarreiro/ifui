module Ifui.XDom

import Data.List
import Data.IORef

public export
data Attribute : Type -> Type where
  StringAttribute : String -> String -> Attribute a
  TargetValueEventListener : String -> (String -> a) -> Attribute a

public export
record ADom n where
    constructor MkADom
    create : String -> IO n
    appendChild : n -> n -> IO ()
    replace : n -> n -> IO ()
    setText : n -> String -> IO ()
    setStringAttribute : n -> String -> String -> IO ()
    addTargetValueEventListener : n -> String -> (String -> IO ()) -> IO (IO ())
    addEmptyEventListener : n -> String -> IO () -> IO (IO ())
    remove : n -> IO ()

public export
data Xml : Type -> Type where
  XNode : String -> List (Attribute a) -> Either String (List (Xml a)) -> Xml a

data VirtualAttribute : Type -> Type -> Type where
  VirtualStringAttribute : String -> String -> VirtualAttribute n a
  VirtualTargetValueEventListener : IO ()  ->  String -> (String -> a) -> VirtualAttribute n a

data VirtualDom : Type -> Type -> Type where
  VirtualNode : n -> String -> List (VirtualAttribute n a) -> Either String (List (VirtualDom n a)) -> VirtualDom n a

%foreign "javascript:lambda: (x, y) => x == y"
prim__refEq : AnyPtr -> AnyPtr -> PrimIO Int
export
refEq : HasIO io => a -> b -> io Bool
refEq x y = 
  do
    r <- primIO $ prim__refEq (believe_me x) (believe_me y)
    pure $ r>0

initAttribute : (a -> IO()) -> ADom n -> n -> Attribute a -> IO (VirtualAttribute n a)
initAttribute onEvent adom node (StringAttribute k v) = 
  do
    adom.setStringAttribute node k v
    pure $ VirtualStringAttribute k v
initAttribute onEvent adom node (TargetValueEventListener k f) = 
  do
    rm <- adom.addTargetValueEventListener node k (onEvent . f)
    pure $ VirtualTargetValueEventListener rm k f

removeAttribute : ADom n -> n -> VirtualAttribute n a -> IO ()
removeAttribute adom node (VirtualStringAttribute x y) = adom.setStringAttribute node x ""
removeAttribute adom node (VirtualTargetValueEventListener x y f) = x

updateAttribute : (b -> IO()) -> ADom n -> n -> VirtualAttribute n a -> Attribute b -> IO (VirtualAttribute n b)
updateAttribute onEvent adom node old@(VirtualStringAttribute x y) att@(StringAttribute z w) = 
  case x == z of
       True => case y == w of
                    True => pure $ VirtualStringAttribute x y
                    False => adom.setStringAttribute node x w >> pure  (VirtualStringAttribute x w)
       False => 
               do
                 removeAttribute adom node old
                 initAttribute onEvent adom node att
updateAttribute onEvent adom node old@(VirtualStringAttribute x y) att@(TargetValueEventListener z f) = 
  do
    removeAttribute adom node old
    initAttribute onEvent adom node att
updateAttribute onEvent adom node old@(VirtualTargetValueEventListener x y f) att@(StringAttribute z w) =
  do
    removeAttribute adom node old
    initAttribute onEvent adom node att
updateAttribute onEvent adom node (VirtualTargetValueEventListener x y f) att@(TargetValueEventListener z g) = 
  case y == z of
    True => 
           do
             eq <- refEq f g
             case eq of
                  True => pure $ VirtualTargetValueEventListener x y g
                  False => x >> initAttribute onEvent adom node att
    False => 
            do
              x
              initAttribute onEvent adom node att


getADomNode : VirtualDom n a -> n 
getADomNode (VirtualNode node _ _ _) = node

createVNode : (a -> IO()) -> ADom n -> Xml a -> IO (VirtualDom n a)
createVNode onEvent adom (XNode tag attrs w) = 
  do
    node <- adom.create tag
    vattrs <- traverse (initAttribute onEvent adom node) attrs
    case w of
         (Left x) => 
                    do
                      adom.setText node x
                      pure $ VirtualNode node tag vattrs (Left x)
         (Right x) => 
                     do
                       dchilds <- traverse (createVNode onEvent adom) x
                       traverse_ (\w => adom.appendChild node $ getADomNode w)  dchilds
                       pure $ VirtualNode node tag vattrs (Right dchilds)

updateVNode : (b -> IO()) -> ADom n -> VirtualDom n a -> Xml b -> IO (VirtualDom n b)
updateVNode onEvent adom ov@(VirtualNode node otag oAttrs z) nn@(XNode ntag attrs w) =
  case otag == ntag of
       True =>
              do
                newW <- case (z, w) of
                            ((Left x), (Left y)) =>
                                                   if x == y then pure $ Left y
                                                             else adom.setText node y >> pure (Left y)
                            ((Left x), (Right y)) => 
                                                    do
                                                       adom.setText node ""
                                                       dchilds <- traverse (createVNode onEvent adom) y
                                                       traverse_ (\w => adom.appendChild node $ getADomNode w)  dchilds
                                                       pure $ Right dchilds
                            ((Right x), (Left y)) => 
                                                    adom.setText node y >> pure (Left y)
                            ((Right x), (Right y)) => 
                                                     do
                                                       updated <- traverse (\(z, y) => updateVNode onEvent adom z y) (zip x y)
                                                       traverse_ (adom.remove . getADomNode) (drop (length y) x)
                                                       new <- traverse (createVNode onEvent adom) (drop (length x) y)
                                                       pure $ Right $ updated ++ new
                updatedAttrs <- traverse (\(z, y) => updateAttribute onEvent adom node z y) (zip oAttrs attrs)
                traverse_ (removeAttribute adom node) (drop (length attrs) oAttrs)
                newAttrs <- traverse (initAttribute onEvent adom node) (drop (length oAttrs) attrs)
                pure $ VirtualNode node ntag (updatedAttrs ++ newAttrs) newW
       False =>
              do
                new_node <- createVNode onEvent adom nn
                adom.replace node (getADomNode new_node)
                pure new_node
               

createOnEvent : {0 f : s -> Type} -> ADom n -> ((x:s) -> f x -> s) -> ((x : s) -> Xml (f x)) -> 
                                     IORef (Maybe (x:s ** VirtualDom n (f x))) -> f x -> IO ()
createOnEvent adom update view viewState y = 
  do
    vs <- readIORef viewState
    case vs of
         Just (x ** vdom) =>
                            do
                              let newX = update x (believe_me y)
                              newVdom <- updateVNode (createOnEvent adom update view viewState)  adom vdom (view newX)
                              writeIORef viewState (Just (newX ** newVdom))
         Nothing => pure ()

export
startView : {0 f : s -> Type} -> ADom n -> s -> ((x:s) -> f x -> s) -> ((x : s) -> Xml (f x)) -> IO n
startView adom x update view =
  do
    viewState <- newIORef $ the (Maybe (DPair s (VirtualDom n . f))) Nothing
    vdom <- createVNode (createOnEvent adom update view viewState) adom (view x)
    writeIORef viewState (Just $ (x ** vdom))
    pure $ getADomNode vdom

