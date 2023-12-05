module Ifui.MediaTypes

import public Data.Vect
import public Ifui.Json

public export
data PDF =
  HttpPDF String Nat -- url, numpages

export
JsonSerializable PDF where
  toJson (HttpPDF url numpages) = toJson (url, numpages)

  fromJson x = (\(url, numpages) => HttpPDF url numpages) <$> fromJson {a = (String, Nat)} x


public export
Tensor : List Nat -> Type -> Type
Tensor [] x = x
Tensor (k :: ks) x = Vect k (Tensor ks x)
