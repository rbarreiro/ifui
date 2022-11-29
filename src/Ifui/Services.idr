module Ifui.Services

import IfuiServer.Promise
import public Ifui.Json
import public Data.List.Elem

public export
data ServiceKind : Type where 
  RPC : (0 a : Type) -> (0 b : Type) -> ServiceKind
                 -- | StreamService Type Type

--export
--getService : Elem (s, t) ts -> Server ts -> Service s t
--getService Here (x :: y) = x
--getService (There x) (y :: z) = getService x z


