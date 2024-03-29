module Main

import Ifui
import Ifui.Bulma
import Ifui.ReadWidgetBulma
import Ifui.ExtensibleTypes

IData : Type
IData =  Record [("Name", String), ("Surname", String), ("whatever", String )]

mainWidget : Widget ()
mainWidget =
  do
    x <- the (Widget (Maybe (IData))) getFormBulma
    text $ show x

main : IO ()
main = runWidget $ mainWidget
