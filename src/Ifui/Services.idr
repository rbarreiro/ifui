module Ifui.Services

import IfuiServer.Promise
import public Ifui.Json
import public Data.List.Elem

public export
data ServiceKind : Type where 
  RPC : (0 a : Type) -> (0 b : Type) -> ServiceKind
                 -- | StreamService Type Type

public export
data Service : String -> ServiceKind -> Type where
  MkRPC : (a -> JSON) -> (JSON -> Maybe a) -> (b -> JSON) -> (JSON -> Maybe b) -> (s : String) -> (a -> Promise b)  -> Service s (RPC a b)

public export
data Server : List (String, ServiceKind) -> Type where
  Nil : Server []
  (::) : Service s spec -> Server ts -> Server ((s, spec) :: ts)

export
getService : Elem (s, t) ts -> Server ts -> Service s t
getService Here (x :: y) = x
getService (There x) (y :: z) = getService x z


