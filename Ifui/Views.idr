module Ifui.Views


import Ifui.Dom
import public Ifui.Changes
import Data.List
import Data.IORef


export
data View : Type -> Type -> Type -> Type where
  MkView : (DomNode -> a -> (c -> IO ()) -> IO ()) ->
           (DomNode -> b -> (c -> IO ()) -> IO ()) ->
           View a b c

record ViewHandler a b c where
  constructor MkViewHandler
  node : DomNode
  view : View a b c
  calcUpdate : a -> c -> b
  state : IORef a

export
displayText : View String StringChanges b
displayText =
  MkView init update
  where
    init : DomNode -> String -> (c -> IO ()) -> IO ()
    init r s0 _ =
      do
        n <- createElement "span"
        setTextContent s0 n
        appendChild r n
    update : DomNode -> StringChanges -> (c -> IO ()) -> IO ()
    update n (setString x) _ =
      setTextContent x n

export
onChangeTextInput : View a b String
onChangeTextInput =
  MkView init update
  where
    init : DomNode -> a -> (String -> IO ()) -> IO ()
    init r _ procEvent =
      do
        n <- createElement "input"
        addEventListener "change" (\e => targetValue e >>= procEvent) n
        appendChild r n
    update : DomNode -> b -> (String -> IO ()) -> IO ()
    update _ _ procEvent = pure ()

export
container : String -> List (View a b c) -> View a b c
container tag views =
  MkView init update
  where
    init : DomNode -> a -> (c -> IO ()) -> IO ()
    init r s0 procEvent =
      do
        n <- createElement tag
        traverse_ (\(MkView i _) => i n s0 procEvent) views
        appendChild r n
    update : DomNode -> b -> (c -> IO ()) -> IO ()
    update r change procEvent =
      do
        childNodes <- getChildren r
        traverse_ (\(n, (MkView _ u)) => u n change procEvent) (zip childNodes views)

export
div : List (View a b c) -> View a b c
div = container "div"

mutual
  export
  changeView : Changes a b => b -> ViewHandler a b c -> IO ()
  changeView x handler =
    case handler.view of
      (MkView _ u) =>
        do
          n <- firstElementChild handler.node
          u n x (procViewEvent handler)

  procViewEvent : Changes a b => ViewHandler a b c -> c -> IO ()
  procViewEvent handler e =
    do
      s <- readIORef handler.state
      let change = handler.calcUpdate s e
      writeIORef handler.state (applyChanges s change)
      changeView change handler

export
viewloop : Changes a b => a -> View a b c -> (a -> c -> b) -> IO (ViewHandler a b c)
viewloop s0 view@(MkView init update) u =
  do
    b <- body
    n <- createElement "div"
    s <- newIORef s0
    let handler = MkViewHandler n view u s
    appendChild b n
    init n s0 (procViewEvent handler)
    pure handler
