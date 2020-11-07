import Ifui.Dom

import Data.IORef
import Data.Vect
import Data.List
import Ifui.Dom

data Attribute : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  OnChange : ((x:a) -> String -> g x) -> Attribute a f g

data View : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  ViewTextNode : View a (const String) g
  ViewNode : String -> List (Attribute a f g) -> List (View a f g) -> View a f g

data Update : (a:Type) -> (a -> Type) -> Type where
  SetValue : (x:a) -> f x -> Update a f

record RunningView a (f : a->Type) (g : a->Type) where
  constructor MkRunningView
  mode : a
  state : f mode
  view : View a f g
  updater : ((x:a) -> f x -> g x -> Update a f)
  root : DomNode

set : (x:a) -> f x -> Update a f
set = SetValue


displayText : View a (const String) g
displayText = ViewTextNode

onChange : ((x:a) -> String -> g x) -> Attribute a f g
onChange = OnChange

vectPrepend : b -> Update Nat (\n => Vect n b)
vectPrepend = ?vp

displayVect : View Nat (const a) g -> View Nat (\n => Vect n a) g
displayVect = ?dv

div : List (Attribute a f g) -> List (View a f g) -> View a f g
div attrs childs = ViewNode "div" attrs childs

input : List (Attribute a f g) -> View a f g
input attrs = ViewNode "input" attrs []

VRef : (a:Type) -> (a->Type) -> (a->Type) -> Type
VRef a f g = IORef (RunningView a f g)

updateAttribute : DomNode -> Attribute a f g -> Update a f -> IO ()
updateAttribute n (OnChange f1) (SetValue y z) = pure ()

updateView : DomNode -> View a f g -> Update a f -> IO ()
updateView n ViewTextNode (SetValue t v) =
  setTextContent v n
updateView n (ViewNode y xs ys) (SetValue z w) =
  do
    childNodes <- getChildren n
    traverse_ (\(c, v) => updateView c v (SetValue z w)) (zip childNodes ys)
    traverse_ (\a => updateAttribute n a (SetValue z w)) xs

updateRV : Update a f -> RunningView a f g -> RunningView a f g
updateRV (SetValue y z) = record {mode = y, state = z}

viewRoot : RunningView a f g -> IO DomNode
viewRoot rv = firstElementChild rv.root

procChange : VRef a f g -> ((x:a) -> String -> g x) -> DomEvent ->  IO ()
procChange ref fn e =
  do
    v <- targetValue e
    rv <- readIORef ref
    let u = rv.updater rv.mode rv.state (fn rv.mode v)
    r <- viewRoot rv
    updateView r rv.view u
    let rv' = updateRV u rv
    writeIORef ref rv'

initAttribute : VRef a f g -> (x:a) -> f x -> DomNode -> Attribute a f g -> IO ()
initAttribute ref a s node (OnChange fn) =
  addEventListener "change" (procChange ref fn) node

init : DomNode -> (x:a) -> f x -> VRef a f g -> View a f g -> IO ()
init r a s ref ViewTextNode =
  do
    n <- createElement "span"
    setTextContent s n
    appendChild r n
init r a s ref (ViewNode x xs ys) =
  do
    n <- createElement x
    traverse_ (initAttribute ref a s n) xs
    traverse_ (\z => init n a s ref z) ys
    appendChild r n


viewloop : (x:a) -> f x -> View a f g -> ((x:a) -> f x -> g x -> Update a f) -> IO ()
viewloop a s0 v u =
  do
    b <- body
    n <- createElement "div"
    appendChild b n
    ref <- newIORef $ MkRunningView a s0 v u n
    init n a s0 ref v
