module Ifui.Views

import Ifui.Changes

public export
XmlView : Type -> Type -> Type
XmlView = \a, b => View a (XmlTree b)

export
textView : String -> List (String, String) -> List (EventListener b) -> XmlView String b
textView tag attrs listeners =
  MkView (\x => Node (MkXmlNode tag attrs listeners x) [] ) update
  where
    update : Changes String String -> Changes (Tree (XmlNode b)) (Tree (XmlNode b))
    update c = updateNodeValue (updateXmlNodeText c)

export
nodeView : String -> List (String, String) -> List (EventListener b) -> List (XmlView a b) -> XmlView a b
nodeView tag attrs listeners childs =
  MkView init update
  where
    init : a -> Tree (XmlNode b)
    init x = Node (MkXmlNode tag attrs listeners "") (map (\(MkView i u) => i x) childs)

    update : Changes a a -> Changes (Tree (XmlNode b)) (Tree (XmlNode b))
    update x = changeChilds (map (\(MkView _ u)=> u x) childs)
