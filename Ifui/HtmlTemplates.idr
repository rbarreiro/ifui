module Ifui.HtmlTemplates

import Ifui.Dom
import public Ifui.Templates

export
textSpan : (a -> String) -> Template a h
textSpan = TemplateText "span"

export
div : List (Template a h) -> Template a h
div = TemplateNode "div" []

export
textInput : ((x : a) -> String -> h x) -> Template a h
textInput f = TemplateNode "input" [TargetValueEventListener "change" f] []

hDom : TDom DomNode
hDom =
  MkTDom
    createChild
    setTextContent
    setAttribute
    addTargetValueEventListener
  where
    createChild : DomNode -> String -> IO DomNode
    createChild n tag =
      do
        c <- createElement tag
        appendChild n c
        pure c
    addTargetValueEventListener : DomNode -> String -> (String -> IO ()) -> IO ()
    addTargetValueEventListener n x f =
      addEventListener x (\w => targetValue w >>= f) n

export
htmlLoop : Template a h ->
           a ->
           ((x : a) -> h x -> a) ->
           IO ()
htmlLoop t s u =
  do
    n <- createElement "div"
    appendChild !body n
    templateLoop hDom n t s u
