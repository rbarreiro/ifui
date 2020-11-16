import Ifui.Views

EventTy : () -> Type
EventTy = \_ => String

StateTy : () -> Type
StateTy = \_ => String

view : View () (\x,y => StringChanges () x y) StateTy EventTy
view = div [onChangeTextInput, displayText]

update : (x:()) -> StateTy x -> EventTy x -> (y:() ** StringChanges () x y)
update _ _ str = (() ** setString str)

main : IO ()
main =
  do
    viewloop () "" view update
    pure ()
