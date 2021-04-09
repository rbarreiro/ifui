module Ifui.Templates

import Data.IORef
import Data.List

public export
data Attribute : (a : Type) -> (a -> Type) -> Type where
  StringAttribute : String -> String -> Attribute a h
  DynStringAttribute : String -> (a -> String) -> Attribute a h
  TargetValueEventListener : String -> ((x : a) -> String -> h x) -> Attribute a h
  EmptyEventListener : String -> ((x : a) -> h x) -> Attribute a h

public export
data Template : (a : Type) -> (a -> Type) -> Type where
  TemplateNode : String -> List (Attribute a h) -> List (Template a h) -> Template a h
  TemplateText : String -> (a -> String) -> Template a h
  ListTemplate : String -> (a -> List b) -> Template (a, b) (h . Builtin.fst) -> Template a h

public export
record ADom n where
  constructor MkADom
  createChild : n -> String -> IO n
  setText : n -> String -> IO ()
  setStringAttribute : n -> String -> String -> IO ()
  getStringAttribute : n -> String -> IO String
  addTargetValueEventListener : n -> String -> (String -> IO ()) -> IO ()
  addEmptyEventListener : n -> String -> IO () -> IO ()
  removeNode : n -> IO ()
  createContainer : n -> IO n


record RT n a (h : a -> Type) w where
  constructor MkRT
  refresh : w -> IO ()
  getState : IO a
  aDom : ADom n
  update : ((x : a) -> h x -> w)

initAttribute : RT n a h w -> n -> Attribute a h -> IO (a -> IO ())
initAttribute rt node (StringAttribute x y) =
  do
    rt.aDom.setStringAttribute node x y
    pure (\_ => pure ())
initAttribute rt node (DynStringAttribute name val) =
  do
    s <- rt.getState
    rt.aDom.setStringAttribute node name (val s)
    pure update
    where 
      update : a -> IO ()
      update s = 
        do
          let nval = val s
          oval <- rt.aDom.getStringAttribute node "value"
          if nval == oval then pure ()
                          else rt.aDom.setStringAttribute node name nval
initAttribute rt node (TargetValueEventListener x f) =
  do
    rt.aDom.addTargetValueEventListener node x action
    pure (\_ => pure ())
  where
    action : String -> IO ()
    action str =
      do
        s <- rt.getState
        rt.refresh (rt.update s (f s str))
initAttribute rt node (EmptyEventListener x f) =
  do
    rt.aDom.addEmptyEventListener node x action
    pure (\_ => pure ())
  where
    action : IO ()
    action =
      do
        s <- rt.getState
        rt.refresh (rt.update s (f s))

mutual
  initListTemplate : RT n a h w ->
                     n ->
                     (a -> List b) ->
                     Template (a, b) (h . Builtin.fst) ->
                     IO (a -> IO ())
  initListTemplate rt node fl t =
    do
      s <- rt.getState
      list_s_n_u <- traverse (makeNew node) (fl s)
      masterRef <- newIORef list_s_n_u
      pure $ update node masterRef rt.aDom
    where
      buildItemRTs : IORef b -> RT n (a, b) (h . Builtin.fst) w
      buildItemRTs x =
        MkRT
          rt.refresh
          ((,) <$> rt.getState <*> readIORef x)
          rt.aDom
          (\(s,_), e => rt.update s e)

      makeNew : n -> b -> IO (IORef b, n, (a, b) -> IO ())      
      makeNew node x = 
        do
          sref <- newIORef x
          node <- rt.aDom.createContainer node
          let rt = buildItemRTs sref
          upd <- initTemplate rt node t
          pure (sref, node, upd)

      updateOne : a -> b -> (IORef b, n, (a, b) -> IO ()) -> IO ()
      updateOne x y (s, _, u) = 
        do
          writeIORef s y
          u (x, y)

      update : n -> IORef (List (IORef b, n, (a, b) -> IO ())) -> ADom n -> a -> IO ()
      update node masterRef aDom x = 
        do
          let new_bs = fl x
          old_list_s_n_u <- readIORef masterRef
          traverse_ (\(y, snu) => updateOne x y snu) (zip new_bs old_list_s_n_u) 
          traverse_ (\(_, n, _) => aDom.removeNode n) (drop (length new_bs) old_list_s_n_u)
          list_new <- traverse (makeNew node) (drop (length old_list_s_n_u) new_bs)
          writeIORef masterRef (take (length new_bs) old_list_s_n_u ++ list_new)

  initTemplate : RT n a h w -> n -> Template a h -> IO (a -> IO ())
  initTemplate rt node (TemplateNode tag attrs xs)=
    do
      n <- rt.aDom.createChild node tag
      updates_childs <- traverse (\t => initTemplate rt n t) xs
      updates_attrs <- traverse (\as => initAttribute rt n as) attrs
      pure $ \w => sequence_ (map ($ w) (updates_childs ++ updates_attrs) )
  initTemplate rt node (TemplateText tag f) =
    do
      s <- rt.getState
      n <- rt.aDom.createChild node tag
      rt.aDom.setText n (f s)
      pure $ \w => rt.aDom.setText n (f w)
  initTemplate rt node (ListTemplate tag lf t) =
    do
      n <- rt.aDom.createChild node tag
      initListTemplate rt n lf t


export
templateLoop : ADom n ->
               n ->
               Template a h ->
               a ->
               ((x : a) -> h x -> a) ->
               IO ()
templateLoop d n t s u =
  do
    rfrsh <- newIORef (\_ => pure ())
    sref <- newIORef s
    let rt = MkRT (doRefresh rfrsh sref) (readIORef sref) d u
    upd <- initTemplate rt n t
    writeIORef rfrsh upd
  where
    doRefresh : IORef (a -> IO ()) -> IORef a -> a -> IO ()
    doRefresh r sref s =
      do
        rv <- readIORef r
        writeIORef sref s
        rv s
