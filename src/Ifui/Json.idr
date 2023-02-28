module Ifui.Json

import public Language.JSON
import public Ifui.ExtensibleRecords
import Data.List

public export
interface JsonSerializable a where
  toJson : a -> JSON
  fromJson : JSON -> Maybe a
  stringify : a -> String

  stringify x = show $ toJson x

public export 
interface JsonSerializable1 (0 f : Type -> Type) where
  toJson1 : (f a) -> (a -> JSON) -> JSON
  fromJson1 : JSON -> (JSON -> Maybe a) -> Maybe (f a)

export
(JsonSerializable a, JsonSerializable1 f) => JsonSerializable (f a) where
  toJson x = toJson1 x (toJson {a = a})
  fromJson x = fromJson1 x (fromJson {a=a})
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
JsonSerializable (Maybe String) where
  toJson (Just x) = JString x
  toJson Nothing = JNull

  fromJson (JString x) = Just (Just x)
  fromJson JNull = Just Nothing
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
JsonSerializable1 List where
  toJson1 x g = JArray (map g x)

  fromJson1 (JArray x) g = sequence $ map g x
  fromJson1 _ _ = Nothing

export
interface JsonObjectSerializable a where
  toListJson : a -> List (String, JSON)
  fromListJson : List (String, JSON) -> Maybe a

export
JsonObjectSerializable a => JsonSerializable a where
  toJson x = JObject $ toListJson x

  fromJson (JObject x) = fromListJson x
  fromJson _ = Nothing

All : (f : Type -> Type) -> UKeyList String Type -> Type
All f []        = ()
All f ((k,v) :: xs) = (f v, All f xs)

export
{zs : UKeyList String Type} -> All JsonSerializable zs => JsonObjectSerializable (Record zs) where
  toListJson {zs = []} _ = 
    []
  toListJson {zs = ((s, t) :: l)} ((MkEntry s x) :: z) = 
    (s, toJson x) :: toListJson z

  fromListJson {zs = []} x = 
    Just []
  fromListJson {zs = ((::) {d = d} {p = p} (k, v) l)} x = 
    do
      ws <- fromListJson {a = Record l} x 
      j <- lookup k x
      w <- fromJson {a=v} j
      pure $ ExtensibleRecords.(::) {p = p} (MkEntry k w) ws


export
JsonSerializable (Record ts) => JsonSerializable (Maybe (Record ts)) where
  toJson Nothing = JNull
  toJson (Just x) = toJson x

  fromJson JNull = Just Nothing
  fromJson o = Just <$> fromJson o

export
{zs : UKeyList String Type} -> All JsonSerializable zs => JsonSerializable (Variant zs) where
  toJson {zs = []} x = 
    JNull
  toJson {zs = ((s, z) :: l)} (MkVariant s x Here) = 
    JObject [("k", JString s), ("v", toJson x)]
  toJson {zs = ((s, z) :: l)} (MkVariant k x (There y)) = 
    toJson {a = Variant l} (MkVariant k x y)

  fromJson {zs = []} x = 
    Nothing
  fromJson {zs = ((s, t) :: l)} z@(JObject [("k", JString k), ("v", v)]) = 
    if s == k then do
                    w <- fromJson {a = t} v
                    pure $ MkVariant s w Here
              else case fromJson {a=Variant l} z of
                        Nothing => Nothing
                        (Just (MkVariant str x pe)) => Just $ MkVariant str x (There pe)
  fromJson _ = Nothing

fromListJsonUKeyList :  (JSON -> Maybe b) -> List (String, JSON) -> Maybe (UKeyList String b)
fromListJsonUKeyList _ [] = 
  Just []
fromListJsonUKeyList fromJsonB  ((x, y) :: xs) =
  do
    xs_ <- fromListJsonUKeyList fromJsonB xs
    y_ <- fromJsonB y
    prf <- calcCanPrependKey x xs_
    pure ((::) (x, y_) xs_  {p=prf})

toListJsonUKeyList : (b -> JSON) -> UKeyList String b -> List (String, JSON)
toListJsonUKeyList f [] = []
toListJsonUKeyList f ((k, v) :: l) = (k, f v)  :: toListJsonUKeyList f l


export
JsonSerializable1 (UKeyList String) where
  toJson1 x cont = 
    JObject $ toListJsonUKeyList cont x
  fromJson1 (JObject x) cont = 
    fromListJsonUKeyList cont x
  fromJson1 _ cont = 
    Nothing
    
export
JsonSerializable b => JsonSerializable1 (const b) where
  toJson1 x cont = toJson x 
  fromJson1 x cont = fromJson x

public export
interface JsonSerializableTreeHeads (0 ts : UKeyList String (Type -> Type)) where
  toJsonTreeHeads : (s : String) -> (p : KElem s ts) -> ((klookup ts p) (Tree rs)) -> (Tree rs -> JSON) -> JSON
  fromJsonTreeHeads : JSON -> (JSON -> Maybe (Tree rs)) -> Maybe (k : String ** (p : KElem k ts ** (klookup ts p) (Tree rs)))


All1 : (f : (Type -> Type) -> Type) -> UKeyList String (Type -> Type) -> Type
All1 f []        = ()
All1 f ((k,v) :: xs) = (f v, All1 f xs)

export
{zs : UKeyList String (Type -> Type)} -> All1 JsonSerializable1 zs => JsonSerializableTreeHeads zs where
  toJsonTreeHeads {zs = []} s p x cont = 
    JNull
  toJsonTreeHeads {zs = ((_, v) :: l)} s KHere x cont = 
    JObject [("k", JString s), ("v", toJson1 x cont)]
  toJsonTreeHeads {zs = ((y, w) :: l)} s (KThere z) x cont =
    toJsonTreeHeads s z x cont

  fromJsonTreeHeads {zs = []} (JObject [("k", JString k), ("v", v)]) cont = 
    Nothing
  fromJsonTreeHeads {zs = ((s, f) :: l)} z@(JObject [("k", JString k), ("v", v)]) cont =
    if s == k then do
                    w <- fromJson1 {f = f} v cont
                    pure (s ** KHere ** w)
              else case fromJsonTreeHeads {ts=l} z cont of
                        Nothing => Nothing
                        (Just ((k_ ** ((p_ ** x_))))) => Just (k_ ** ( KThere p_ ** x_ ))
  fromJsonTreeHeads _ _ = 
    Nothing


export
JsonSerializableTreeHeads ts => JsonSerializable (Tree ts) where
  toJson (N s x {p}) = toJsonTreeHeads {ts=ts} s p x toJson

  fromJson x = 
    do
      (k ** (p ** x)) <- fromJsonTreeHeads {ts=ts} x fromJson
      pure $ N k x {p=p}

testTree : Tree [("Record", UKeyList String), ("String", const ())]
testTree = N "String" ()

testJ : JSON
testJ = toJson testTree

%foreign "javascript:lambda: x => JSON.stringify(x)"
export
prim__json_stringify : AnyPtr -> String

%foreign "javascript:lambda: () => null"
prim__null : () -> AnyPtr

%foreign "javascript:lambda: x => x+0"
prim__readBool : AnyPtr -> Int

%foreign "javascript:lambda: x => x>0"
prim__mkBool : Int  -> AnyPtr

double2ptr : Double -> AnyPtr 
double2ptr = believe_me 

ptr2double : AnyPtr -> Double
ptr2double = believe_me

ptr2str : AnyPtr -> String
ptr2str = believe_me

str2ptr : String -> AnyPtr
str2ptr = believe_me

ptr2int : AnyPtr -> Int
ptr2int = believe_me 

%foreign "javascript:lambda: x => typeof x"
prim__typeof : AnyPtr -> String

%foreign "javascript:lambda: () => ({})"
prim__newObj : () -> AnyPtr
%foreign "javascript:lambda: (x, k, v) => {const res= {...x}; res[k]=v; return res}"
prim__setItem : AnyPtr -> String -> AnyPtr -> AnyPtr
%foreign "javascript:lambda: (x, k) => x[k]"
prim__getItem : AnyPtr -> String -> AnyPtr
%foreign "javascript:lambda: (x, k) => x.hasOwnProperty(k) + 0"
prim__hasItem : AnyPtr -> String -> Int
%foreign "javascript:lambda: (x) => Object.keys(x)"
prim__objectKeys : AnyPtr -> AnyPtr 

mkJsObj : List (String, AnyPtr) -> AnyPtr
mkJsObj xs = foldl (\ptr, (key, val) => prim__setItem ptr key val) (prim__newObj ()) xs

%foreign "javascript:lambda: () => Object.freeze([])"
prim__newArray : () -> AnyPtr
%foreign "javascript:lambda: (val, ori) => Object.freeze(ori.concat([Object.freeze(val)]))"
prim__arrayAppend : AnyPtr -> AnyPtr -> AnyPtr
%foreign "javascript:lambda: x => x.length"
prim__arrayLength : AnyPtr -> Int
%foreign "javascript:lambda: (x, pos) => x[pos]"
prim__arrayGet : AnyPtr -> Int -> AnyPtr
%foreign "javascript:lambda: x => Array.isArray(x)+0 "
prim__isArray : AnyPtr -> Int

mkJsArray : List AnyPtr -> AnyPtr
mkJsArray xs = foldl (\ptr, val => prim__arrayAppend val ptr) (prim__newArray ()) xs

export
json2ptr : JSON -> AnyPtr
json2ptr JNull = 
  prim__null ()
json2ptr (JBoolean x) = 
  prim__mkBool $ if x then 1 else 0
json2ptr (JNumber dbl) = 
  double2ptr dbl
json2ptr (JString str) = 
  str2ptr str
json2ptr (JArray jsons) = 
  mkJsArray $ map json2ptr jsons
json2ptr (JObject xs) = 
  mkJsObj $ map (\(k,v) => (k, json2ptr v)) xs

%foreign "javascript:lambda: x => (x === null || x === undefined)+0"
prim__isNullOrUndefined : AnyPtr -> Int

readList : AnyPtr -> List AnyPtr
readList x =
  let l = prim__arrayLength x
  in if l == 0 then []
               else [prim__arrayGet x i | i <-[0..(l - 1)]]

export
ptr2json : AnyPtr -> Maybe JSON
ptr2json x = 
  if prim__isNullOrUndefined x > 0 then Just $ JNull
  else if prim__isArray x > 0 then JArray <$> (sequence $ ptr2json <$> readList x)
  else
    case prim__typeof x of
         "string" =>
            Just $ JString $ ptr2str x
         "number" =>
            Just $ JNumber $ ptr2double x
         "boolean" =>
            Just $ JBoolean $ prim__readBool x > 0
         "object" =>
            let keys = ptr2str <$> (readList $ prim__objectKeys x)
            in JObject <$> sequence [(k,) <$>  (ptr2json $ prim__getItem x k) | k <- keys]
         o => 
            Nothing

export
toPtr : JsonSerializable a => a -> AnyPtr 
toPtr = json2ptr . toJson

export
fromPtr : JsonSerializable a => AnyPtr -> Maybe a
fromPtr x = ptr2json x >>= fromJson 

ChangeTest : Type -> Type
ChangeTest a = Record [("old_val", Maybe a), ("new_val", Maybe a)]

TestTy : Type 
TestTy = ChangeTest (Record [("spec", Tree [("stuff2", const ()), ("stuff", const ()), ("Record", UKeyList String), ("String", const ())])])

-- takes to much time
--testInstanceRec : TestTy -> JSON
--testInstanceRec = toJson 

