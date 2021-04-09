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
textInput : (a -> String) -> ((x : a) -> String -> h x) -> Template a h
textInput val f = TemplateNode "input" [DynStringAttribute "value" val, TargetValueEventListener "change" f] []

export
button : String -> ((x : a) -> h x) -> Template a h
button lbl f = TemplateNode "button" [EmptyEventListener "click" f] [textSpan $ const lbl]

export
ul : (a -> List b) -> Template (a, b) (h . Builtin.fst) -> Template a h
ul = ListTemplate "ul"

export
li : List (Template a h) -> Template a h
li = TemplateNode "li" []

hDom : ADom DomNode
hDom =
  MkADom
    createChild
    setTextContent
    setAttr
    getAttr
    addTargetValueEventListener
    addEmptyEventListener
    removeNode
    (\n => createChild n "span")
  where
    getAttr : DomNode -> String -> IO String
    getAttr n k =
      if k == "value" then getValue n
                      else getAttribute n k
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
    addTargetValueEventListener : DomNode -> String -> (String -> IO ()) -> IO ()
    addTargetValueEventListener n x f =
      addEventListener x (\w => targetValue w >>= f) n
    addEmptyEventListener : DomNode -> String -> IO () -> IO ()
    addEmptyEventListener n x f =
      addEventListener x (\_ => f) n

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
