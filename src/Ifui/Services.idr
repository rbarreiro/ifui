module Ifui.Services

import public Ifui.Json
import public Data.List.Elem
import public Ifui.ExtensibleRecords

public export
data ServiceKind : Type where 
  RPC : (0 a : Type) -> (0 b : Type) -> ServiceKind
  StreamService : (0 a : Type) -> (0 b : Type) -> ServiceKind
  GroupService : (0 xs : UKeyList String ServiceKind) -> ServiceKind
--  CRUDCollection : (0 id : Type) -> (0 create : Type) -> (0 view : Type) -> ServiceKind

namespace SubFields
  public export
  data SubFields : FieldList -> FieldList -> Type where
    Nil : SubFields [] ts
    (::) : {pk : KElem k ts} ->  {auto pp : UKeyListCanPrepend (k, klookup ts pk) ss} -> SubFields ss ts -> SubFields ((k, klookup ts pk) :: ss) ts
  

public export
Change : Type -> Type
Change a = Record [("old_val", Maybe a), ("new_val", Maybe a)]

