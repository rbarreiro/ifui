module Ifui.Json

import public Language.JSON
import Ifui.ExtensibleRecords
import Data.List

public export
interface JsonSerializable a where
  toJson : a -> JSON
  fromJson : JSON -> Maybe a
  stringify : a -> String

  stringify x = show $ toJson x


export
JsonSerializable JSON where
  toJson x = x
  fromJson x = Just x

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
JsonSerializable Int where
  toJson x = JNumber $ cast x

  fromJson (JNumber x) = Just $ cast x
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
JsonObjectSerializable a => JsonSerializable a where
  toJson x = JObject $ toListJson x

  fromJson (JObject x) = fromListJson x
  fromJson _ = Nothing

export
JsonObjectSerializable (Record []) where
  toListJson  _ = []

  fromListJson [] = Just []
  fromListJson _ = Nothing

export
{s : String} -> {p : UKeyListCanPrepend (s, Maybe t) ts} -> (JsonSerializable (Maybe t), JsonObjectSerializable (Record ts)) => JsonObjectSerializable (Record ((s, Maybe t) :: ts)) where
  toListJson ((MkEntry s x) :: y) =
    (s, toJson x) :: toListJson y

  fromListJson [] = 
    Nothing 
  fromListJson (xs) = 
    do
      ws <-fromListJson {a = Record ts} xs 
      case lookup s xs of
        Nothing => pure $ MkEntry s Nothing  :: ws
        Just j =>
          do
           w <- fromJson {a=Maybe t} j
           pure $ MkEntry s w :: ws

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (JsonSerializable t, JsonObjectSerializable (Record ts)) => JsonObjectSerializable (Record ((s,t) :: ts)) where
  toListJson ((MkEntry s x) :: y) =
    (s, toJson x) :: toListJson y

  fromListJson [] = 
    Nothing 
  fromListJson (xs) = 
    do
      ws <-fromListJson {a = Record ts} xs 
      j <- lookup s xs 
      w <- fromJson {a=t} j
      pure $ MkEntry s w :: ws

fromListJsonUKeyListAux : (JsonSerializable b) => List (String, JSON) -> Maybe (UKeyList String b)
fromListJsonUKeyListAux [] = 
  Just []
fromListJsonUKeyListAux ((x, y) :: xs) =
  do
    xs_ <- fromListJsonUKeyListAux {b=b} xs
    y_ <- fromJson {a=b} y
    prf <- calcCanPrepend (x, y_) xs_
    pure ((::) (x, y_) xs_  {p=prf})

(JsonSerializable b) => JsonObjectSerializable (UKeyList String b) where
  toListJson [] = []
  toListJson ((x, y) :: l) = (x, toJson y) :: toListJson l

  fromListJson = fromListJsonUKeyListAux

export
{s : String} -> JsonSerializable t => JsonSerializable (Variant [(s, t)]) where
  toJson (MkVariant s x {p=Here}) = 
    JObject [("k", JString s), ("v", toJson x)]
  toJson (MkVariant s x {p=(There y)}) impossible

  fromJson (JObject [("k", JString k), ("v", v)]) = 
    if s == k then do
                      w <- fromJson {a = t}  v
                      pure $ MkVariant s w {p=Here}
              else Nothing
  fromJson _ = 
    Nothing

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (JsonSerializable t, JsonSerializable (Variant ts)) => JsonSerializable (Variant ((s, t) :: ts)) where
  toJson (MkVariant s x {p=Here}) = 
    JObject [("k", JString s), ("v", toJson x)]
  toJson (MkVariant str x {p=(There y)}) =
    toJson {a=Variant ts} (MkVariant str x {p=y})
  
  fromJson z@(JObject [("k", JString k), ("v", v)]) = 
    if s == k then do
                      w <- fromJson {a = t}  v
                      pure $ MkVariant s w {p=Here}
              else case fromJson {a=Variant ts} z of
                      Nothing => Nothing 
                      (Just (MkVariant str x {p = pe})) => Just $ MkVariant str x {p = There pe}
  fromJson _ = 
    Nothing

public export 
interface JsonSerializableTreeBranch (0 f : Type -> Type) where
  toJsonBranch : (f (Tree ts)) -> (Tree ts -> JSON) -> JSON
  fromJsonBranch : JSON -> (JSON -> Maybe (Tree ts)) -> Maybe (f (Tree ts))

export
JsonSerializableTreeBranch (UKeyList String) where
  toJsonBranch x cont = 
    toJson $ map cont x
  fromJsonBranch x cont = 
    do
      z <- fromJson {a=UKeyList String JSON} x
      let z_ = map cont z
      fromAllJust z_
    

public export
interface JsonSerializableTreeHeads (0 ts : UKeyList String (Type -> Type)) where
  toJsonTreeHeads : (s : String) -> (p : KElem s ts) -> ((klookup ts p) (Tree rs)) -> (Tree rs -> JSON) -> JSON
  fromJsonTreeHeads : JSON -> (JSON -> Maybe (Tree rs)) -> Maybe (k : String ** (p : KElem k ts ** (klookup ts p) (Tree rs)))

export
{s : String} -> JsonSerializableTreeBranch f => JsonSerializableTreeHeads [(s, f)] where
  toJsonTreeHeads s KHere x cont = 
    JObject [("k", JString s), ("v", toJsonBranch x cont)]
  toJsonTreeHeads s (KThere y) x cont impossible
    
  fromJsonTreeHeads (JObject [("k", JString k), ("v", v)]) cont = 
    if s == k then do
                     w <- fromJsonBranch {f=f} v cont
                     pure $ (s ** (KHere ** w))
              else Nothing
  fromJsonTreeHeads _ _ = 
    Nothing
 
{s : String} -> {pp : UKeyListCanPrepend (s, f) ts} -> (JsonSerializableTreeBranch f, JsonSerializableTreeHeads ts) => JsonSerializableTreeHeads ((s, f) :: ts) where
  toJsonTreeHeads s KHere x cont = 
    JObject [("k", JString s), ("v", toJsonBranch x cont)]
  toJsonTreeHeads s (KThere y) x cont = 
    toJsonTreeHeads {ts=ts} s y x cont

  fromJsonTreeHeads z@(JObject [("k", JString k), ("v", v)]) cont =
    if s == k then do
                     w <- fromJsonBranch {f=f} v cont
                     pure $ (s ** (KHere ** w))
              else case fromJsonTreeHeads {ts=ts} z cont of
                        Nothing => Nothing
                        (Just ((k_ ** ((p_ ** x))))) => Just $ (k_ ** ( KThere p_ ** x))
  fromJsonTreeHeads _ _ = 
    Nothing

export
JsonSerializableTreeHeads ts => JsonSerializable (Tree ts) where
  toJson (N s x {p}) = toJsonTreeHeads {ts=ts} s p x toJson

  fromJson x = 
    do
      (k ** (p ** x)) <- fromJsonTreeHeads {ts=ts} x fromJson
      pure $ N k x {p=p}

