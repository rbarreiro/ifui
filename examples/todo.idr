import Ifui.HtmlViews

StateTy : Type
StateTy = String

view : HtmlView StateTy (StateTy -> Changes StateTy StateTy)
view = div [onChangeTextInput (\x,_=> set x), displayText]

main : IO ()
main =
  do
    startView "" view

-- data Event : Nat -> Type where
--   ChangeStr : String -> Event n
-- --  AddTodo : Event n
--
-- StateTys : Nat -> Vect 2 Type
-- StateTys = \n => [String, Vect n String]
--
-- State : Nat -> Type
-- State = \n => HVect (StateTys n)
--
-- StateChanges : Nat -> Nat -> Type
-- StateChanges = \n,m => HVectChanges (StateTys n) (StateTys m)
--
-- view : View Nat StateChanges State Event
-- view = div [onChangeTextInput (\_ => ChangeStr)]--, displayText] --, button (\_=>AddTodo) "Add"
--
-- update : (x:Nat) -> State x -> Event x -> (y:Nat ** StateChanges x y)
-- update x _ (ChangeStr str) = (x ** updateAtHVect 0 (setStr str))
-- --update x _ AddTodo = (x ** )
--
--
-- main : IO ()
-- main =
--   do
--     viewloop 0 ["", []] view update (\x,y,c,v => applyDChanges (StateTys x) (StateTys y) c v)
--     pure ()
