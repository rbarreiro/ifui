module Ifui.OneOf

import public Data.Vect

public export
data OneOf : Vect n Type -> Type where
  This : (k : Fin n) -> index k ts  -> OneOf ts
