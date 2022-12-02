module Main

import Api
import Ifui
import Ifui.Bulma
import Ifui.ReadWidgetBulma

newTodoPrompt : Widget (Maybe (String))
newTodoPrompt = getFormBulma

mainWidget : ServerConnection ApiServices -> Maybe String -> Widget ()
mainWidget srv Nothing = 
  do
    val <- callRPC {a=Unit} {b=List String} srv "todoList" ()
    mainWidget srv (Just $ show val)
mainWidget srv (Just x) = 
  do
    x <- div [] [newTodoPrompt, text x]
    case x of
         Nothing => mainWidget srv Nothing
         Just z => callRPC srv "createTodo" z >> mainWidget srv Nothing
    

main : IO ()
main =
  serverConnect ("ws://\{!getLocationHostname}:6402") ApiServices $ \srv =>
      runWidget $ mainWidget srv Nothing
