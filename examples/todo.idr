import Ifui.Html
import Data.Vect
import Data.List

data Action : Nat -> Type where
  SetInput : String -> Action n
  AddTodo : Action n

State : Type
State = (String, (n:Nat ** Vect n String))

ActionTy : State -> Type
ActionTy = \(_, (n ** _)) => Action n

setInput : (s : State) -> String -> ActionTy s
setInput (_, (n ** _)) x = SetInput x

addTodo : (s : State) -> ActionTy s
addTodo (_, (n ** _)) = AddTodo

view : (s : State) -> Html (ActionTy s)
view x = textSpan "ola"

update : (s : State) -> ActionTy s -> State
update (y, (n ** v)) (SetInput s) = (s, (n ** v))
update (y, (n ** v)) AddTodo = ("", (S n ** y :: v))

main : IO ()
main =
  startHtmlView ("", (0 ** [])) update view
