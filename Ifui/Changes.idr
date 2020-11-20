module Ifui.Changes

import Data.HVect
import Data.List

public export
data Tree a = Node a (List (Tree a))

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
XmlTree : Type -> Type
XmlTree = \a => Tree (XmlNode a)

public export
data Changes : Type -> Type -> Type where
  set : b -> Changes a b
  updateNodeValue : Changes a a -> Changes (Tree a) (Tree a)
  updateXmlNodeText : Changes String String -> Changes (XmlNode a) (XmlNode a)
  changeChilds : List (Changes (Tree a) (Tree a)) -> Changes (Tree a) (Tree a)
  mapChangesMaybe : Changes a a -> Changes (Maybe a) (Maybe a)
  mapChangesList : Changes a a -> Changes (List a) (List a)

public export
data View a b = MkView (a -> b) (Changes a a -> Changes b b)

  -- updateAtHVect : Fin n -> Changes (Vect.index i ts) t -> Changes (HVect ts) (HVect (replaceAt i t ts))

export
applyChanges : Changes x y -> x -> y
applyChanges (set v) w =
  v
applyChanges (updateNodeValue c) w =
  case w of
    (Node z xs) => Node (applyChanges c z) xs
applyChanges (updateXmlNodeText c) w =
  case w of
    (MkXmlNode tag attrs listeners txt) =>
      MkXmlNode tag attrs listeners (applyChanges c txt)
applyChanges (changeChilds cs) w =
  case w of
       (Node z xs) => Node z (List.zipWith applyChanges cs xs ++ drop (length cs) xs)
applyChanges (mapChangesMaybe c) w =
  map (applyChanges c) w
applyChanges (mapChangesList c) w =
  map (applyChanges c) w

    -- applyDChanges (updateAtHVect i v) w = ?help w
