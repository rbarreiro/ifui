module Ifui.Html

import Ifui.XDom
import Ifui.Dom

hDom : ADom DomNode
hDom =
  MkADom
    createElement
    appendChild
    replaceWith
    setTextContent
    setAttr
    addTargetValueEventListener
    addEmptyEventListener
    removeNode
  where
    setAttr : DomNode -> String -> String -> IO ()
    setAttr n k v =
      if k == "value" then setValue n v
                      else setAttribute n k v
    createChild : DomNode -> String -> IO DomNode
    createChild n tag =
      do
        c <- createElement tag
        appendChild n c
        pure c
    addTargetValueEventListener : DomNode -> String -> (String -> IO ()) -> IO (IO ())
    addTargetValueEventListener n x f =
      addEventListener x (\w => targetValue w >>= f) n
    addEmptyEventListener : DomNode -> String -> IO () -> IO (IO ())
    addEmptyEventListener n x f =
      addEventListener x (\_ => f) n

export
data HtmlAttribute : Type -> Type where
  MkHtmlAttribute : Attribute a -> HtmlAttribute a

export
data Html : Type -> Type where
  MkHtml : Xml a -> Html a

removeHtmlAttributeConstructor : HtmlAttribute a -> Attribute a
removeHtmlAttributeConstructor (MkHtmlAttribute x) = x

export
startHtmlView : {f : s -> Type} ->  s -> ((x:s) -> f x -> s) -> ((x : s) -> Html (f x)) -> IO ()
startHtmlView s update view =
  do
    node <- startView hDom s update (\x => let MkHtml z = view x in z)
    appendChild !body node

export
textSpan : String -> Html a
textSpan s = MkHtml $ XNode "span" [] (Left s)

export
input : List (HtmlAttribute a) -> Html a
input attributes = MkHtml $ XNode "input" (map removeHtmlAttributeConstructor attributes) (Right [])
