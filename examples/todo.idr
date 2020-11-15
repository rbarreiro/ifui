import Ifui.Views

EventTy : Type
EventTy = String

StateTy : Type
StateTy = String

state0 : StateTy
state0 = ""

view : View StateTy StringChanges EventTy
view = div [onChangeTextInput, displayText]

update : StateTy -> EventTy -> StringChanges
update _ s = setString s

main : IO ()
main =
  do
    viewloop state0 view update
    pure ()
