module Main

import Api
import Ifui
import Ifui.Bulma

mainWidget : ServerConnection ApiServices -> Maybe String -> Widget ()
mainWidget srv Nothing = 
  do
    val <- callRPC {a=Unit} {b=String} srv "getText" ()
    mainWidget srv (Just val)
mainWidget srv (Just x) = 
  text x

main : IO ()
main =
  do
    srv <- serverConnect "localhost:6012" todoApi
    runWidget $ mainWidget srv Nothing
