module Ifui.ExtensibleRecords

import public Data.List.Elem

public export
data Entry : String -> Type -> Type where
  MkEntry : (0 s : String) -> t -> Entry s t

public export
(^=) : (0 s : String) -> t -> Entry s t
(^=) = MkEntry 

public export
data Record : List (String, Type) -> Type where
  Nil : Record []
  (::) : Entry s t -> Record ts -> Record ((s,t) :: ts)


