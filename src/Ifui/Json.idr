module Ifui.Json

import public Language.JSON
import Ifui.ExtensibleRecords

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

export
JsonSerializable Bool where
  toJson x = JBoolean x

  fromJson (JBoolean x) = Just x
  fromJson _ = Nothing

export
JsonSerializable a => JsonSerializable (Maybe a) where
  toJson Nothing = JNull
  toJson (Just x) = toJson x

  fromJson JNull = Just Nothing
  fromJson o = Just <$> fromJson {a=a} o

export
JsonSerializable a => JsonSerializable (List a) where
  toJson x = JArray (map toJson x)

  fromJson (JArray x) = sequence $ map fromJson x
  fromJson _ = Nothing

export
interface JsonObjectSerializable a where
  toListJson : a -> List (String, JSON)
  fromListJson : List (String, JSON) -> Maybe a

export
JsonObjectSerializable (Record ts) => JsonSerializable (Record ts) where
  toJson x = JObject $ toListJson x

  fromJson (JObject x) = fromListJson x
  fromJson _ = Nothing

export
JsonObjectSerializable (Record []) where
  toListJson  _ = []

  fromListJson [] = Just []
  fromListJson _ = Nothing

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (JsonSerializable t, JsonObjectSerializable (Record ts)) => JsonObjectSerializable (Record ((s,t) :: ts)) where
  toListJson ((MkEntry s x) :: y) =
    (s, toJson x) :: toListJson y

  fromListJson [] = 
    Nothing 
  fromListJson ((k, v) :: xs) = 
    do
      ws <-fromListJson {a = Record ts} xs 
      w <- fromJson {a=t} v
      pure $ MkEntry s w  :: ws

