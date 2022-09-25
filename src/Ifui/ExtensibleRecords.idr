module Ifui.ExtensibleRecords

import public Data.Vect
import public Data.Vect.Elem

public export
data Entry : String -> Type -> Type where
  MkEntry : (0 s : String) -> t -> Entry s t

public export
value : Entry s t -> t
value (MkEntry s y) = y

infixr 4 ^=

public export
(^=) : (0 s : String) -> t -> Entry s t
(^=) = MkEntry 

public export
data Record : Vect n (String, Type) -> Type where
  Nil : Record []
  (::) : Entry s t -> Record ts -> Record ((s,t) :: ts)



public export
data Alt : Vect n (String, Type) -> Type where
  MkAlt : Entry s t -> Elem (s, t) ts  -> Alt ts
