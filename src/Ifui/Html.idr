module Ifui.Html

import Ifui.Widget
import Ifui.Patterns

export
data HtmlAttribute f a = MkHtmlAttribute (WidgetAttribute a)

export
Functor (HtmlAttribute f) where
  map f (MkHtmlAttribute x) = MkHtmlAttribute $ f<$> x

unwrapHtmlAttribute : HtmlAttribute f a -> WidgetAttribute a
unwrapHtmlAttribute (MkHtmlAttribute x) = x

public export
interface OnClick a where
  onClick : HtmlAttribute a ()
onClick_ : WidgetAttribute ()
onClick_ = WidgetEventListener "click" (\_ => pure ())

public export
interface OnChange a where
  onChange : HtmlAttribute a String
onChange_ : WidgetAttribute String
onChange_ = WidgetEventListener "change" (\e => target e >>= getValue)

public export
interface Value a where
  value : String -> HtmlAttribute a b
value_ : String -> WidgetAttribute b
value_ x = WidgetSimpleAttribute $ ValueAttr x

public export
interface Class_ a where
  class_ : String -> HtmlAttribute a b

class__ : String -> WidgetAttribute b
class__ x = WidgetSimpleAttribute $ CSSClassAttr x

type__ : String -> WidgetAttribute b
type__ x = WidgetSimpleAttribute $ StringAttr "type" x

export
data ButtonAttribute : Type where
export
OnClick ButtonAttribute where
  onClick = MkHtmlAttribute $ onClick_
export
button : List (HtmlAttribute ButtonAttribute a) -> List (Widget a) -> Widget a
button xs ys = node "button" (unwrapHtmlAttribute <$> xs) ys

export
data TextInputAttribute : Type where
export
OnChange TextInputAttribute where
  onChange = MkHtmlAttribute $ onChange_
export
Value TextInputAttribute where
  value x = MkHtmlAttribute $ value_ x
export
textInput : List (HtmlAttribute TextInputAttribute a) -> Widget a
textInput xs = node "input" (type__ "text" :: (unwrapHtmlAttribute <$> xs)) []

export
data SpanAttribute : Type where
export
span : List (HtmlAttribute SpanAttribute a) -> List (Widget a) -> Widget a
span xs ys = node "span" (unwrapHtmlAttribute <$> xs) ys

export
data DivAttribute : Type where
export
Class_ DivAttribute where
  class_ = MkHtmlAttribute . class__
export
div : List (HtmlAttribute DivAttribute a) -> List (Widget a) -> Widget a
div xs ys = node "div" (unwrapHtmlAttribute <$> xs) ys

export
data FormAttribute : Type where
export
form : List (HtmlAttribute FormAttribute a) -> (a -> Widget a) -> a -> Widget a
form attrs children s = 
  loopState
    s
    (\x => node "form" 
             (
               (WidgetEventListener "submit" (\e =>do preventDefault e; pure $ Right x)) :: 
                                                      (unwrapHtmlAttribute <$> (\w => Right <$> w)  <$> attrs)
             ) 
             [Left <$> children x] )

