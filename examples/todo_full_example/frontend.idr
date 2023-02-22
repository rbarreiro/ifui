module Main

import Api
import Ifui
import Ifui.Bulma
import Ifui.ReadWidgetBulma
import Data.Maybe

newTodoPrompt : Widget (Maybe (String))
newTodoPrompt = getFormBulma

todosInsert : SrvRef (RPC String ()) -> Widget a
todosInsert srv =
  do
    Just desc <- newTodoPrompt | Nothing => todosInsert srv
    callRPC srv desc
    todosInsert srv


todosView : SrvRef (StreamService () (Change String)) -> Widget a
todosView srv =
  callStreamChangesAccumList srv () (\todos => text $ show todos)

mainWidget : Widget ()
mainWidget =
  do
    Just login <- getFormBulma {a=Login} | Nothing => mainWidget
    loginRes <- serverConnectWithAuth "ws://\{!getLocationHostname}:6402" login RoleServer 
    case loginRes of
         (False ** _) => text "Failed Login"
         (True ** srv) => div [todosInsert (srv // "createTodo"), todosView (srv // "todoList")]

main : IO ()
main =
      runWidget $ mainWidget
