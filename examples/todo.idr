import Ifui.HtmlViews

import Data.HVect
import Decidable.Equality

StateTy : Type
StateTy = HVect [String, (n:Nat ** Vect n String)]

todosView : HtmlWidget (n:Nat ** Vect n String)
todosView = dWidget $ vectUlLiD $ displayTextD

view : HtmlWidget StateTy
view = div [ onChangeTextInput (\x,_ => updateHVectAt 0 (set x))
           , fieldWidget 1 todosView
           ]

main : IO ()
main =
  do
    startView ["", (1 ** ["t1"])] view
