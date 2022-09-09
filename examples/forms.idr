module Main

import Ifui
import Ifui.ReadWidget
import Ifui.ExtensibleRecords

Vars : Type
Vars = Record [("a", Double), ("b", Double), ("c", Double)]

varsReader : Reader Vars
varsReader = getReader

mainWidget : Widget a
mainWidget =
  loopForever Nothing (\x => Just <$> varsReader x)

main : IO ()
main = runWidget $ mainWidget

