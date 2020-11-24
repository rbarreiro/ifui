module Ifui.Views

import Ifui.Changes
import Data.HVect
import Data.List
import Decidable.Equality

public export
XmlView : Type -> Type -> Type
XmlView a b = View a (XmlTree b)

public export
XmlWidget : Type -> Type
XmlWidget a = XmlView a (a -> Changes a)

public export
XmlDView : (a:Type) -> (a->Type) -> (a->Type) -> Type
XmlDView a f g = DView a f (\x => XmlTree (g x))

public export
XmlDWidget : (a:Type) -> (a->Type) -> Type
XmlDWidget a f = XmlDView a f (\x => f x -> (y:a ** DChanges a f x y))

export
textView : String -> List (String, String) -> List (EventListener b) -> XmlView String b
textView tag attrs listeners =
  MkView (\x => Node (MkXmlNode tag attrs listeners x) [] ) update
  where
    update : Changes String -> Changes (Tree (XmlNode b))
    update NoChanges = NoChanges
    update c = updateNodeValue (updateXmlNodeText c)

export
nodeView : String -> List (String, String) -> List (EventListener b) -> List (XmlView a b) -> XmlView a b
nodeView tag attrs listeners childs =
  MkView init update
  where
    init : a -> Tree (XmlNode b)
    init x = Node (MkXmlNode tag attrs listeners "") (map (\(MkView i u) => i x) childs)

    update : Changes a -> Changes (Tree (XmlNode b))
    update NoChanges = NoChanges
    update x = changeChilds (map (\(MkView _ u)=> u x) childs)

export
fieldView : (j : Fin k) -> View (index j ts) b -> View (HVect ts) b
fieldView j (MkView i u) =
  MkView init update
  where
    init : HVect ts -> b
    init x = i $ HVect.index j x

    update : Changes (HVect ts) -> Changes b
    update NoChanges = NoChanges
    update (set x) = set $ i (index j x)
    update (updateHVectAt k c) =
      case decEq j k of
        (Yes Refl) => u c
        (No _) => NoChanges

export
dXmlWidget : DecEq a => XmlDWidget a f -> XmlWidget (DPair a f)
dXmlWidget (MkDView i u) =
  MkView init update
  where
    convertTree : (x:a) -> (f x -> (y : a ** DChanges a f x y)) -> DPair a f -> Changes (DPair a f)
    convertTree x f (MkDPair z val) =
      case decEq x z of
           (Yes Refl) =>
              let (y ** res) = f val
              in changeDPair (decEq) z y res
           (No _) => NoChanges

    init : DPair a f -> Tree (XmlNode (DPair a f -> Changes (DPair a f)))
    init (MkDPair x val) =
      let tree = i x val
      in map (map (convertTree x)) tree

    update : Changes (DPair a f) -> Changes (Tree (XmlNode (DPair a f -> Changes (DPair a f))))
    update NoChanges =
      NoChanges
    update (set (MkDPair x val)) =
      let tree = i x val
          newTree = map (map (convertTree x)) tree
      in set newTree
    update (changeDPair g x x NoDChanges) = NoChanges

mapEvent_aux_changes : (x -> y) -> Changes (Tree (XmlNode x)) -> Changes (Tree (XmlNode y))
mapEvent_aux_changes f (set z) = set $ map (map f) z
mapEvent_aux_changes f NoChanges = NoChanges
mapEvent_aux_changes f (updateNodeValue (set z)) = updateNodeValue $ set $ map f z
mapEvent_aux_changes f (updateNodeValue NoChanges) = NoChanges
mapEvent_aux_changes f (updateNodeValue (updateXmlNodeText z)) = updateNodeValue (updateXmlNodeText z)
mapEvent_aux_changes f (changeChilds xs) = changeChilds (map (mapEvent_aux_changes f) xs)

export
mapEvent : (x -> y) -> XmlView a x -> XmlView a y
mapEvent f (MkView i u) =
    MkView ((map (map f)) . i) (mapEvent_aux_changes f . u)

export
fieldWidget : (j : Fin k) -> XmlWidget (index j ts) -> XmlWidget (HVect ts)
fieldWidget j x = mapEvent (\c, s => updateHVectAt j (c $ index j s)) $ fieldView j x


export
textViewD : String -> List (String, String) -> List (EventListener ((x:a)-> g x)) -> XmlDView a (const String) g
textViewD tag attrs listeners =
  MkDView init update
  where
    init : (x : a) -> String -> Tree (XmlNode (g x))
    init x y = Node (MkXmlNode tag attrs (map (\(n,f)=> (n, \e=> f e x)) listeners) y) []

    update : (x : a) -> (y : a) ->
             DChanges a f x y ->
             DChanges a (\z => Tree (XmlNode (g z))) x y
    update x x NoDChanges = NoDChanges

export
nodeViewD : String -> List (String, String) -> List (EventListener ((x:a)-> g x)) -> List (XmlDView a f g) -> XmlDView a f g
nodeViewD tag attrs listeners childs =
  MkDView init update
  where
    init : (x : a) -> f x -> Tree (XmlNode (g x))
    init x v =
      Node (MkXmlNode tag attrs (map (\(n,f)=> (n, \e=> f e x)) listeners) "")
           (map (\(MkDView i _) => i x v) childs)

    update : (x : a) -> (y : a) ->
             DChanges a f x y ->
             DChanges a (\z => Tree (XmlNode (g z))) x y
    update x x NoDChanges = NoDChanges

export
vectViewD : String -> List (String, String) ->
            List (EventListener ((x:Nat)-> g x)) ->
              XmlDView Nat (const a) g -> XmlDView Nat (\n=>Vect n a) g
vectViewD tag attrs listeners (MkDView i u) =
  MkDView init update
  where
    init : (x : Nat) -> Vect x a -> Tree (XmlNode (g x))
    init x val =
      Node (MkXmlNode tag attrs (map (\(z,f) => (z, \e => f e x )) listeners) "")
           (map (\z => i x z) (toList val))

    update : (x : Nat) -> (y : Nat) ->
             DChanges Nat (\z=>Vect z a) x y ->
             DChanges Nat (\z=>Tree (XmlNode (g z))) x y
    update x x NoDChanges = NoDChanges
