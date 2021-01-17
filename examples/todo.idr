import Ifui.HtmlTemplates
import Data.Vect

data Action : Nat -> Type where
  SetInput : String -> Action n

State : Type
State = (String, (n:Nat ** Vect n String))

ActionTy : State -> Type
ActionTy = \(_, (n ** _)) => Action n

setInput : (s : State) -> String -> ActionTy s
setInput (_, (n ** _)) x = SetInput x

view : Template State ActionTy
view =
  div
    [ textInput setInput
    , textSpan fst
    ]

update : (s : State) -> ActionTy s -> State
update (y, (n ** v)) (SetInput s) = (s, (n ** v))

main : IO ()
main =
  htmlLoop view ("", (0 ** [])) update
