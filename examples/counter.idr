module Main

import Ifui
import Ifui.Html

counter : Int -> Widget a
counter count =
  do
    button [onClick] [text (show count)]
    counter (count + 1)

main : IO ()
main = runWidget $ counter 0
