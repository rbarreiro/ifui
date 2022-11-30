module Main

import Api
import Ifui
import Ifui.Bulma

mainWidget : ServerConnection ApiServices -> Maybe String -> Widget ()
mainWidget srv Nothing = 
  do
    val <- callRPC {a=Unit} {b=List String} srv "todoList" ()
    mainWidget srv (Just $ show val)
mainWidget srv (Just x) = 
  text x

main : IO ()
main =
  serverConnect ("ws://\{!getLocationHostname}:6402") ApiServices $ \srv =>
      runWidget $ mainWidget srv Nothing
