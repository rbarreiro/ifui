module Ifui.Variant

import public Data.List.Elem

public export
data Variant : List (String, Type) -> Type where
  MkVariant : (s : String) -> t -> Elem (s,t) ts -> Variant ts
