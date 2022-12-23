module Main

import Api
import Ifui
import Ifui.Bulma
import Ifui.ReadWidgetBulma
import Data.Maybe

newTodoPrompt : Widget (Maybe (String))
newTodoPrompt = getFormBulma

todosView : (HasValue "createTodo" (RPC String ()) ts)  =>  
              ServerConnection ts -> Widget ()
todosView srv =
  do
    Just desc <- newTodoPrompt | Nothing => todosView srv
    callRPC srv "createTodo" desc
    todosView srv


todosView : (HasValue "todoList" (StreamService () (Change String)) ts)  =>  
              ServerConnection ts -> Maybe (List String) -> Widget ()
todosView srv todos =
  do
   r <- div [] [callStreamChangesAccumList "todoList" srv (), text $ show todos]
   todosView srv $ Just r  



mainWidget : Widget ()
mainWidget =
  do
    Just login <- getFormBulma {a=Login} | Nothing => mainWidget
    loginRes <- serverConnectWithAuth "ws://\{!getLocationHostname}:6402" login RoleServer 
    case loginRes of
         (False ** _) => text "Failed Login"
         (True ** srv) => div [] [todosView srv, todosView srv Nothing]

main : IO ()
main =
      runWidget $ mainWidget
