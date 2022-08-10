module Ifui.Html

import Ifui.Widget

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
onChange_ = WidgetEventListener "change" (\e => targetValue e)

public export
interface Value a where
  value : String -> HtmlAttribute a b
value_ : String -> WidgetAttribute b
value_ x = WidgetSimpleAttribute $ ValueAttr x

export
data ButtonAttribute : Type where
export
OnClick ButtonAttribute where
  onClick = MkHtmlAttribute $ onClick_
export
button : List (HtmlAttribute ButtonAttribute a) -> List (Widget a) -> Widget a
button xs ys = node "button" (unwrapHtmlAttribute <$> xs) ys

export
data InputAttribute : Type where
export
OnChange InputAttribute where
  onChange = MkHtmlAttribute $ onChange_
export
Value InputAttribute where
  value x = MkHtmlAttribute $ value_ x
export
input : List (HtmlAttribute InputAttribute a) -> Widget a
input xs = node "input" (unwrapHtmlAttribute <$> xs) []

export
data SpanAttribute : Type where
export
span : List (HtmlAttribute SpanAttribute a) -> List (Widget a) -> Widget a
span xs ys = node "span" (unwrapHtmlAttribute <$> xs) ys

export
data DivAttribute : Type where
export
div : List (HtmlAttribute DivAttribute a) -> List (Widget a) -> Widget a
div xs ys = node "div" (unwrapHtmlAttribute <$> xs) ys

