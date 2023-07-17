module Ifui.MediaTypes

import Ifui.Json

public export
data PDF =
  HttpPDF String

export
JsonSerializable PDF where
  toJson (HttpPDF str) = toJson str

  fromJson x = HttpPDF <$> fromJson x
