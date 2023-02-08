module Ifui.BulmaViews

import Ifui
import Ifui.OneOf
import Ifui.ReadWidgetBulma 


export
interface WidgetShow a where
  wShow : a -> Widget b

