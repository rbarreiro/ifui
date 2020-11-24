module Ifui.Changes

import Data.HVect
import Data.List
import Decidable.Equality

public export
data Tree a = Node a (List (Tree a))

public export
Functor Tree where
  map f (Node x xs) = Node (f x) (map (map f) xs)

public export
record Event where
  constructor MkEvent
  targetValue : String

public export
EventListener : Type -> Type
EventListener = \a => (String, Event -> a)

public export
record XmlNode a where
  constructor MkXmlNode
  tag : String
  attributes : List (String, String)
  eventListeners : List (EventListener a)
  text : String

public export
Functor XmlNode where
  map f (MkXmlNode tag attributes eventListeners text) =
    MkXmlNode tag attributes (map (\(n,g) => (n, f . g)) eventListeners) text

public export
XmlTree : Type -> Type
XmlTree = \a => Tree (XmlNode a)

mutual
  public export
  data Changes : Type -> Type where
    NoChanges : Changes a
    set : a -> Changes a
    updateNodeValue : Changes a -> Changes (Tree a)
    updateXmlNodeText : Changes String -> Changes (XmlNode a)
    changeChilds : List (Changes (Tree a)) -> Changes (Tree a)
    mapChangesMaybe : Changes a -> Changes (Maybe a)
    mapChangesList : Changes a -> Changes (List a)
    updateHVectAt : (i : Fin k) -> Changes (index i ts) -> Changes (HVect ts)
    changeDPair : ((x:a) -> (y:a) -> Dec (x=y)) -> (x:a) -> (y:a) -> DChanges a f x y -> Changes (DPair a f)

  public export
  data DChanges : (a:Type) -> (a->Type) -> a -> a -> Type where
    NoDChanges : DChanges a f x x


public export
data View a b = MkView (a -> b) (Changes a -> Changes b)

public export
data DView : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkDView : ((x:a) -> f x -> g x) -> ((x:a) -> (y:a) -> DChanges a f x y -> DChanges a g x y ) -> DView a f g

putAt : (i:Fin k) -> index i ts -> HVect ts -> HVect ts
putAt FZ y (x :: z) = y :: z
putAt (FS x) y (z :: w) = z :: putAt x y w

mutual
  export
  applyChanges : Changes a -> a -> Either String a
  applyChanges (set v) w =
    pure v
  applyChanges NoChanges w =
    pure w
  applyChanges (updateNodeValue c) w =
    case w of
      (Node z xs) => pure $ Node !(applyChanges c z) xs
  applyChanges (updateXmlNodeText c) w =
    case w of
      (MkXmlNode tag attrs listeners txt) =>
        pure $ MkXmlNode tag attrs listeners !(applyChanges c txt)
  applyChanges (changeChilds cs) w =
    case w of
         (Node z xs) =>
            if length cs /= length xs then Left "changeChilds: missmatch length"
              else pure $ Node z !(sequence $ zipWith applyChanges cs xs)
  applyChanges (mapChangesMaybe c) w =
    sequence $ map (applyChanges c) w
  applyChanges (mapChangesList c) w =
    sequence $ map (applyChanges c) w
  applyChanges (updateHVectAt i c) w =
    pure $ putAt i !(applyChanges c (index i w)) w
  applyChanges (changeDPair dec x y dchanges) w =
    case w of
      (z ** val) =>
        case dec x z of
          (Yes Refl) => pure (y ** !(applyDChanges dchanges val))
          (No _) => Left "changeDPair: missmatch x"

  applyDChanges : DChanges a f x y -> f x -> Either String (f y)
  applyDChanges NoDChanges w = pure w
