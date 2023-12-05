module Ifui.Services

import public Ifui.Json
import public Data.List.Elem
import public Ifui.ExtensibleTypes

public export
data ServiceKind : Type where 
  RPC : (0 a : Type) -> (0 b : Type) -> ServiceKind
  StreamService : (0 a : Type) -> (0 b : Type) -> ServiceKind
  GroupService : (0 xs : Vect n (String, ServiceKind)) -> ServiceKind
  EmptyService : ServiceKind

public export
Change : Type -> Type
Change a = Record [("old_val", Maybe a), ("new_val", Maybe a)]

