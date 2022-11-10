module Ifui.Json

import public Language.JSON

public export
interface JsonSerializable a where
  toJson : a -> JSON
  fromJson : JSON -> Maybe a
  stringify : a -> String

  stringify x = show $ toJson x

export
JsonSerializable () where
  toJson _ = JNull
  fromJson _ = Just ()

export
JsonSerializable String where
  toJson x = JString x
  fromJson (JString x) = Just x
  fromJson _ = Nothing

