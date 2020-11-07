import Ifui.Views

data TodoAction = ChangeStr String

state0 : String
state0 = ""

view : View () (const String) (const TodoAction)
view = div [] [input [onChange (\_, x => ChangeStr x)], displayText]

update : () -> String -> TodoAction -> Update () (const String)
update _ _ (ChangeStr s) = set () s

main : IO ()
main =
  do
    viewloop () state0 view update
