module Ifui.Json

import public Language.JSON

public export
interface JsonSerializable a where
  toJson : a -> JSON
  fromJson : JSON -> Maybe a
  stringify : a -> String

  stringify x = show $ toJson x
