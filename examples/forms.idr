module Main

import Ifui

vars : Type
vars = Record [("a" : Double), ("b" : Double), ("c" : Double)]

main : IO ()
main = runWidget $ ?h

