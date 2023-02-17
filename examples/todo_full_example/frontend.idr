module Main

import Api
import Ifui
import Ifui.Bulma
import Ifui.ReadWidgetBulma
import Data.Maybe

newTodoPrompt : Widget (Maybe (String))
newTodoPrompt = getFormBulma

todosInsert : (HasValue "createTodo" (RPC String ()) ts)  =>  
              ServerConnection ts -> Widget a
todosInsert srv =
  do
    Just desc <- newTodoPrompt | Nothing => todosInsert srv
    callRPC "createTodo" srv desc
    todosInsert srv


todosView : (HasValue "todoList" (StreamService () (Change String)) ts)  =>  
              ServerConnection ts -> Widget a
todosView srv =
  callStreamChangesAccumList "todoList" srv () (\x => text $ show todos)

mainWidget : Widget ()
mainWidget =
  do
    Just login <- getFormBulma {a=Login} | Nothing => mainWidget
    loginRes <- serverConnectWithAuth "ws://\{!getLocationHostname}:6402" login RoleServer 
    case loginRes of
         (False ** _) => text "Failed Login"
         (True ** srv) => div [todosInsert srv, todosView srv]

main : IO ()
main =
      runWidget $ mainWidget
