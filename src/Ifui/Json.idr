module Ifui.Json

import public Language.JSON
import public Ifui.ExtensibleTypes
import Ifui.Date
import Data.List

public export
ty : String
ty = "$ty_d7a70054-d918-11ed-afa1-0242ac120002"

total
eqJson : JSON -> JSON -> Bool
eqJson JNull JNull = 
  True
eqJson JNull _ = 
  False
eqJson (JBoolean x) (JBoolean y) = 
  x == y
eqJson (JBoolean _) _ = 
  False
eqJson (JNumber x) (JNumber y) = 
  x == y
eqJson (JNumber _) _ = 
  False
eqJson (JString x) (JString y) = 
  x == y
eqJson (JString _) _ = 
  False
eqJson (JArray x) (JArray y) = assert_total $
  length x == length y && all (\(z,w) => eqJson z w) (zip x y) 
eqJson (JArray _) _ = 
  False
eqJson (JObject xs) (JObject ys) = assert_total $
   length xs == length ys && (
     let comp : (String, JSON) -> (String, JSON) -> Ordering
         comp = (\(i,_), (j, _) => compare i j) 
         
         xs_ : List (String, JSON)
         xs_ = sortBy comp xs
         ys_ : List (String, JSON)
         ys_ = sortBy comp ys
     in all (\((i,z), (j, w)) => i == j && eqJson z w) (zip xs_ ys_))
eqJson (JObject _) _ = 
  False

export
Eq JSON where
  (==) = eqJson 

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
JsonSerializable (Maybe JSON) where
  toJson (Just x) = JArray [x]
  toJson Nothing = JNull

  fromJson (JArray [x]) = Just (Just x)
  fromJson JNull = Just Nothing
  fromJson _ = Nothing

export
JsonSerializable (Maybe a) => JsonSerializable (Maybe (Maybe a)) where
  toJson Nothing = JNull
  toJson (Just x) = toJson x

  fromJson JNull = Just Nothing
  fromJson o = Just <$> fromJson o

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
JsonSerializable Date where
  toJson x = JObject [(ty, JString "date"), ("value", JString $ show x)]  

  fromJson (JObject x) =
    do
      JString "date" <- lookup ty x | _ => Nothing
      JString d <- lookup "value" x | _ => Nothing
      readISODate d

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
JsonSerializable (Maybe Int) where
  toJson (Just x) = JNumber $ cast x
  toJson Nothing = JNull

  fromJson (JNumber x) = Just (Just $ cast x)
  fromJson JNull = Just Nothing
  fromJson _ = Nothing

export
JsonSerializable Integer where
  toJson x = JNumber $ cast x

  fromJson (JNumber x) = Just $ cast x
  fromJson _ = Nothing

export
JsonSerializable Bool where
  toJson x = JBoolean x

  fromJson (JBoolean x) = Just x
  fromJson _ = Nothing

export
interface JTuple (0 a : Type) where
  tupleToJson : a -> List JSON
  tupleFromJson : List JSON -> Maybe a

export
JTuple String where
  tupleToJson x = [toJson x]
  
  tupleFromJson [x] = fromJson x
  tupleFromJson _ = Nothing

export
JTuple JSON where
  tupleToJson x = [toJson x]
  
  tupleFromJson [x] = fromJson x
  tupleFromJson _ = Nothing

export
JsonSerializable (Maybe a) => JTuple (Maybe a) where
  tupleToJson x = [toJson x]
  
  tupleFromJson [x] = fromJson x
  tupleFromJson _ = Nothing

export
(JTuple b, JsonSerializable a) => JTuple (a, b) where
  tupleToJson (x, y) = 
    toJson x :: tupleToJson y

  tupleFromJson (x :: xs) =
    (,) <$> fromJson x <*> tupleFromJson xs
  tupleFromJson _ =
    Nothing

export
JTuple (a, b) => JsonSerializable (a, b) where
  toJson x = JArray $ tupleToJson x

  fromJson (JArray x) = tupleFromJson x
  fromJson _ = Nothing 

testJTuple : JSON
testJTuple = toJson ("a", "b", Just "c")

export
JsonSerializable b => JsonSerializable1 (const b) where
  toJson1 x cont = toJson x 
  fromJson1 x cont = fromJson x

export
JsonSerializable1 List where
  toJson1 x g = JArray (map g x)

  fromJson1 (JArray x) g = sequence $ map g x
  fromJson1 _ _ = Nothing

export
JsonSerializable a => JsonSerializable1 (\t => List (a, t)) where
  toJson1 x g = JArray (map (\(z,w) => toJson (toJson z, g w)) x)

  fromJson1 (JArray x) g = sequence $ map (\j => do (z, w) <- fromJson {a = (a, JSON)} j; (z,) <$> g w) x
  fromJson1 _ _ = Nothing

export
recordToJson : {zs : Vect n (String, Type)} -> {auto 0  prf : So (UniqueKeys zs)} ->
                 Record (Vect.mapValues (\t => (t -> JSON)) zs) -> Record zs  -> List (String, JSON)
recordToJson {zs = []} x y = 
  []
recordToJson {zs = ((z, w) :: xs)} ((MkEntry z x) :: s) ((MkEntry z y) :: v) = 
  ((z, x y) :: recordToJson {zs = xs} {prf = soAnd2 prf} s v)

export
recordFromJson : {zs : Vect n (String, Type)} -> {auto 0 prf : So (UniqueKeys zs)} -> 
                    Record (Vect.mapValues (\t => (JSON -> Maybe t)) zs) -> List (String, JSON) -> Maybe (Record zs)
recordFromJson {zs = []} x xs = Just 
  []
recordFromJson {zs = ((y, z) :: ys)} ((MkEntry y x) :: w) xs = 
  do
    ws <- recordFromJson {zs = ys} {prf = soAnd2 prf} w xs
    j <- lookup y xs
    w <- x j
    pure $ MkEntry y w :: ws

toPartsRecord :(zs : Vect n (String, Type)) -> Record (mapValues JsonSerializable zs) -> Record (Vect.mapValues (\t => (t -> JSON)) zs)
toPartsRecord [] x = []
toPartsRecord ((y, z) :: xs) ((MkEntry y x) :: w) = MkEntry y toJson :: toPartsRecord xs w

fromPartsRecord :(zs : Vect n (String, Type)) -> Record (mapValues JsonSerializable zs) -> Record (Vect.mapValues (\t => (JSON -> Maybe t)) zs)
fromPartsRecord [] x = []
fromPartsRecord ((y, z) :: xs) ((MkEntry y x) :: w) = MkEntry y fromJson :: fromPartsRecord xs w

export
{zs : Vect n (String, Type)} -> (prf : So (UniqueKeys zs)) => 
       (i : Record  (mapValues (JsonSerializable) zs)) => 
                JsonSerializable (Record zs) where
  toJson x =
    JObject $ recordToJson {zs = zs} {prf = prf} (toPartsRecord zs i) x 

  fromJson (JObject x) = recordFromJson {zs = zs} {prf = prf} (fromPartsRecord zs i) x
  fromJson _ = Nothing


testRecord : Record [("a", String), ("b", Int)]
testRecord = ["a" ^= "a", "b" ^= 2]

testRecordJson : JSON
testRecordJson = toJson testRecord

export
JsonSerializable (Record ts) => JsonSerializable (Maybe (Record ts)) where
  toJson Nothing = JNull
  toJson (Just x) = toJson x

  fromJson JNull = Just Nothing
  fromJson o = Just <$> fromJson o

    
public export
TreeHeadsToJson : Vect n (String, Type -> Type) -> Type -> Vect n (String, Type)
TreeHeadsToJson zs a = (mapValues (\f => ((f a) -> (a -> JSON) -> JSON)) zs)

toJsonTreeHead : (zs : Vect n (String, Type -> Type)) -> (k : Fin n) -> 
                   index2 k (TreeHeadsToJson zs a) -> (index2 k zs) a -> (a -> JSON)  -> JSON
toJsonTreeHead [] FZ _ _ _ impossible
toJsonTreeHead [] (FS y) _ _ _ impossible
toJsonTreeHead ((y, z) :: xs) FZ f x cont = JObject [("k", JString y), ("v", f x cont)]
toJsonTreeHead ((y, w) :: xs) (FS z) f x cont = toJsonTreeHead xs z f x cont

export
treeToJson : {zs : Vect n (String, Type -> Type)} -> {auto 0 prf : So (UniqueKeys zs)} ->
                 Record (TreeHeadsToJson zs (Tree zs)) -> Tree zs -> JSON
treeToJson x (MkTree k y) = 
  let w = valueIndex k x
  in toJsonTreeHead zs k w y (treeToJson {zs = zs} {prf = prf} x) 

public export
TreeHeadsFromJson : Vect n (String, Type -> Type) -> Type -> Vect n (String, Type)
TreeHeadsFromJson zs a = (mapValues (\f => JSON -> (JSON -> Maybe a) -> Maybe (f a)) zs)

fromJsonTreeHead : (zs : Vect n (String, Type -> Type)) -> (k : Fin n) -> 
                   index2 k (TreeHeadsFromJson zs a) -> JSON -> (JSON -> Maybe a)  -> Maybe ((index2 k zs) a)
fromJsonTreeHead [] FZ _ _ _ impossible
fromJsonTreeHead [] (FS y) _ _ _ impossible
fromJsonTreeHead ((y, z) :: xs) FZ f json cont = f json cont
fromJsonTreeHead ((y, z) :: xs) (FS w) f json cont = fromJsonTreeHead xs w f json cont

export
treeFromJson : {zs : Vect n (String, Type -> Type)} -> {auto 0 prf : So (UniqueKeys zs)} ->
                 Record (TreeHeadsFromJson zs (Tree zs)) -> JSON -> Maybe (Tree zs)
treeFromJson x (JObject y) =
  do
    JString s <- lookup "k" y | _ => Nothing
    k <- findKey s zs
    v <- lookup "v" y
    let w = valueIndex k x
    MkTree k <$> fromJsonTreeHead zs k w v (treeFromJson {zs = zs} {prf = prf} x)
treeFromJson x _ = Nothing

toPartsTree :(zs : Vect n (String, Type -> Type)) -> Record (mapValues JsonSerializable1 zs) -> Record (TreeHeadsToJson zs a)
toPartsTree [] x = []
toPartsTree ((y, z) :: xs) ((MkEntry y x) :: w) = MkEntry y toJson1 :: toPartsTree xs w

fromPartsTree :(zs : Vect n (String, Type -> Type)) -> Record (mapValues JsonSerializable1 zs) -> Record (TreeHeadsFromJson zs a)
fromPartsTree [] x = []
fromPartsTree ((y, z) :: xs) ((MkEntry y x) :: w) = MkEntry y fromJson1 :: fromPartsTree xs w

export
{zs : Vect n (String, Type -> Type)} -> (prf : So (UniqueKeys zs)) => 
       (i : Record  (mapValues (JsonSerializable1) zs)) => 
                JsonSerializable (Tree zs) where
  toJson x = treeToJson {zs = zs} {prf = prf} (toPartsTree zs i) x

  fromJson x = treeFromJson {zs = zs} {prf = prf} (fromPartsTree zs i) x

export
JsonSerializable (Tree ts) => JsonSerializable (Maybe (Tree ts)) where
  toJson Nothing = JNull
  toJson (Just x) = toJson x

  fromJson JNull = Just Nothing
  fromJson o = Just <$> fromJson o

testTree : Tree [("Record", \w => List (String, w)), ("String", const ())]
testTree = N "String" ()

testTreeJson : JSON
testTreeJson  = toJson testTree

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

%foreign "javascript:lambda: x => (new Date(Date.parse(x)))"
prim__readDate : String -> AnyPtr

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
  case lookup ty xs of
       Just (JString "date") =>
          fromMaybe (prim__null ()) $ do
            JString d <- lookup "value" xs | _ => Nothing
            pure $ prim__readDate d
       _ =>
          mkJsObj $ map (\(k,v) => (k, json2ptr v)) xs

%foreign "javascript:lambda: x => (x === null || x === undefined)+0"
prim__isNullOrUndefined : AnyPtr -> Int

%foreign "javascript:lambda: x => x instanceof Date"
prim__isDate : AnyPtr -> Int

%foreign "javascript:lambda: x => x.toISOString()"
prim__dateToISOString : AnyPtr -> String

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
            if prim__isDate x > 0 then Just $ JObject [(ty, JString "date"), ("value", JString $ prim__dateToISOString x)]
                                  else let keys = ptr2str <$> (readList $ prim__objectKeys x)
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
TestTy = ChangeTest (Record [("spec", Tree [("stuff2", const ()), ("stuff", const ()), ("Record", \w => List (String, w)), ("String", const ())])])

testInstanceRec : TestTy -> JSON
testInstanceRec = toJson 


