import Ifui.HtmlTemplates
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

view : Template State ActionTy
view =
  div
    [ textInput fst setInput
    , button "add" addTodo
    , ul (\(_, (n ** x)) => toList x) (li [textSpan snd])
    ]

update : (s : State) -> ActionTy s -> State
update (y, (n ** v)) (SetInput s) = (s, (n ** v))
update (y, (n ** v)) AddTodo = ("", (S n ** y :: v))

main : IO ()
main =
  htmlLoop view ("", (1 ** ["todo1"])) update
