module Ifui.BulmaViews

import Ifui
import Ifui.Bulma
import Ifui.PureExpressions


public export
interface View a where
  bulmaView : a -> Widget b


export
View String where
  bulmaView = text

export
{s : String} -> View a  => View (Entry s a) where
  bulmaView x = labeled s (bulmaView x)

export
{ts : Vect n (String, Type)} -> AllI View ts =>
                View (Record ts) where
  bulmaView {ts = []} x = neutral
  bulmaView {ts = ((y, z) :: xs)} (x :: w) = bulmaView x <+> bulmaView w
