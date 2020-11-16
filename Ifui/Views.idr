module Ifui.Views


import Ifui.Dom
import public Ifui.Changes
import Data.List
import Data.IORef

mutual
  export
  data View : (a : Type) -> (a->a->Type) -> (a->Type) -> (a->Type) -> Type where
    MkView : (DomNode -> (x:a) -> g x -> UpdateHandler a h -> IO ()) ->
             (DomNode -> (x:a) -> (y:a) -> f x y -> UpdateHandler a h -> IO ()) ->
             View a f g h

  data UpdateHandler : (a : Type) -> (a->Type) -> Type where
    MkUpdateHandler : ((x:a) -> (y:a) -> f x y -> g x -> g y) ->
                      DomNode ->
                      View a f g h ->
                      ((x:a) -> g x -> h x -> (y:a ** f x y)) ->
                      IORef (DPair a g) ->
                      UpdateHandler a h


viewRoot : UpdateHandler a h -> IO DomNode
viewRoot (MkUpdateHandler _ r _ _ _) = firstElementChild r

updateViewConstEvent : UpdateHandler a (const t) -> t -> IO()
updateViewConstEvent uh@(MkUpdateHandler calcChanges _ (MkView _ update) calcUpdate ref) e =
  do
    (x ** s) <- readIORef ref
    let (y ** changes) = calcUpdate x s e
    update !(viewRoot uh) x y changes uh
    writeIORef ref (y ** (calcChanges x y changes s))

export
viewloop : DChanges a f g => (x:a) -> (g x) -> View a f g h -> ((x:a) -> g x -> h x -> (y:a ** f x y)) -> IO ()
viewloop x0 s0 view@(MkView init update) calcUpdate =
  do
    b <- body
    n <- createElement "div"
    s <- newIORef (x0 ** s0)
    let updateHandler = MkUpdateHandler applyDChanges n view calcUpdate s
    appendChild b n
    init n x0 s0 updateHandler
    pure ()


------

export
displayText : View a (\x,y => StringChanges a x y) (const String) h
displayText =
  MkView init update
  where
    init : DomNode -> (x : a) -> String -> UpdateHandler a h -> IO ()
    init r _ s0 _ =
      do
        n <- createElement "span"
        setTextContent s0 n
        appendChild r n
    update : DomNode -> (x:a) -> (y:a) -> StringChanges a x y -> UpdateHandler a h -> IO ()
    update n x y (setString str) _ = setTextContent str n

export
onChangeTextInput : View a f g (const String)
onChangeTextInput =
  MkView init update
  where
    init : DomNode -> (x : a) -> g x -> UpdateHandler a (const String) -> IO ()
    init r _ _ uh =
      do
        n <- createElement "input"
        addEventListener "change" (\e => targetValue e >>= updateViewConstEvent uh) n
        appendChild r n
    update : DomNode -> (x : a) -> (y : a) -> f x y -> UpdateHandler a (const String) -> IO ()
    update _ _ _ _ uh = pure ()

export
container : String -> List (View a f g h) -> View a f g h
container tag views =
  MkView init update
  where
    init : DomNode -> (x : a) -> g x -> UpdateHandler a h -> IO ()
    init r x s0 uh =
      do
        n <- createElement tag
        traverse_ (\(MkView i _) => i n x s0 uh) views
        appendChild r n
    update : DomNode -> (x : a) -> (y : a) -> f x y -> UpdateHandler a h -> IO ()
    update r x y change uh =
      do
        childNodes <- getChildren r
        traverse_ (\(n, (MkView _ u)) => u n x y change uh) (zip childNodes views)

export
div : List (View a f g h) -> View a f g h
div = container "div"
