module Ifui.Html

import Ifui.Widget

data HtmlAttribute f a = MkHtmlAttribute (WidgetAttribute a)

unwrapHtmlAttribute : HtmlAttribute f a -> WidgetAttribute a
unwrapHtmlAttribute (MkHtmlAttribute x) = x

public export
interface OnClick a where
  onClick : HtmlAttribute a ()


onClick_ : WidgetAttribute ()
onClick_ = WidgetEventListener "click" (\_ => pure ())

export
data ButtonAttribute = MkButtonAttribute
export
OnClick ButtonAttribute where
  onClick = MkHtmlAttribute $ onClick_
export
button : List (HtmlAttribute ButtonAttribute a) -> List (Widget a) -> Widget a
button xs ys = node "button" (unwrapHtmlAttribute <$> xs) ys

