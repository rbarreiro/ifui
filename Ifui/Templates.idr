module Ifui.Templates

import Data.IORef
import Ifui.Dom

public export
record TDom n where
  constructor MkTDom
  createChild : n -> String -> IO n
  setText : n -> String -> IO ()
  setStringAttribute : n -> String -> String -> IO ()
  addTargetValueEventListener : n -> String -> (String -> IO ()) -> IO ()

record RT n a h where
  constructor MkRT
  refresh : IORef (a -> IO ())
  state : IORef a
  td : TDom n
  update : ((x : a) -> h x -> a)

setRT : a ->  RT n a h -> IO ()
setRT s rt =
  do
    writeIORef rt.state s
    rfrsh <- readIORef rt.refresh
    rfrsh s

public export
data Attribute : (a : Type) -> (a -> Type) -> Type where
  StringAttribute : String -> String -> Attribute a h
  TargetValueEventListener : String -> ((x : a) -> String -> h x) -> Attribute a h

public export
data Template : (a : Type) -> (a -> Type) -> Type where
  TemplateNode : String -> List (Attribute a h) -> List (Template a h) -> Template a h
  TemplateText : String -> (a -> String) -> Template a h

initAttribute : RT n a h -> n -> Attribute a h -> a -> IO (a -> IO ())
initAttribute rt node (StringAttribute x y) s =
  do
    rt.td.setStringAttribute node x y
    pure (\_ => pure ())
initAttribute rt node (TargetValueEventListener x f) s =
  do
    rt.td.addTargetValueEventListener node x action
    pure (\_ => pure ())
  where
    action : String -> IO ()
    action str =
      do
        s <- readIORef rt.state
        setRT (rt.update s (f s str)) rt

initTemplate : RT n a h -> n -> Template a h -> a -> IO (a -> IO ())
initTemplate rt node (TemplateNode tag attrs xs) s =
  do
    n <- rt.td.createChild node tag
    updates_childs <- traverse (\t => initTemplate rt node t s) xs
    updates_attrs <- traverse (\attr => initAttribute rt node attr s) attrs
    pure $ \w => sequence_ (map ($ w) (updates_childs ++ updates_attrs) )
initTemplate rt node (TemplateText tag f) s =
  do
    n <- rt.td.createChild node tag
    rt.td.setText n (f s)
    pure $ \w => rt.td.setText n (f w)


export
templateLoop : TDom n ->
               n ->
               Template a h ->
               a ->
               ((x : a) -> h x -> a) ->
               IO ()
templateLoop d n t s u =
  do
    rfrsh <- newIORef (\_ => pure ())
    sref <- newIORef s
    let rt = MkRT rfrsh sref d u
    upd <- initTemplate rt n t s
    writeIORef rfrsh upd
