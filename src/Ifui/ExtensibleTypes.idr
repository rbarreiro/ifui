module Ifui.ExtensibleTypes

import public Data.Vect
import public Data.So
import public Data.Vect.Elem

public export
soAnd2 : {a : Bool} -> So (a && b) -> So b
soAnd2 = snd . soAnd

namespace List
  public export
  data KElem : a -> List (a, b) -> Type where
    KHere : KElem k ((k,v) :: xs)
    KThere : KElem k xs -> KElem k (x::xs)

  public export
  kElemToNat : KElem x xs -> Nat
  kElemToNat KHere = Z
  kElemToNat (KThere y) = S (kElemToNat y)

  public export
  kElemToFin : KElem x xs -> Fin (length xs)
  kElemToFin KHere = FZ
  kElemToFin (KThere y) = FS $ kElemToFin y

  public export
  indexKElem : Nat -> (xs : List (a, b)) -> Maybe (x ** KElem x xs)
  indexKElem k [] = Nothing 
  indexKElem Z ((x, y) :: xs) = Just (x ** KHere)
  indexKElem (S k) (_ :: xs) = (\(x ** p) => (x ** KThere p)) <$> indexKElem k xs

  public export
  klookup : (ts : List (a, b)) -> KElem k ts -> b
  klookup ((k, v) :: xs) KHere = v
  klookup (x :: xs) (KThere y) = klookup xs y

  public export
  klookup' : (ts : List (a, b)) -> (k : a) -> {auto p : KElem k ts} -> b
  klookup' ts _ = klookup ts p 

  public export
  HasKey : Eq a => a -> List (a, b) -> Bool
  HasKey x [] = False
  HasKey x ((y, z) :: xs) = 
    if x == y then True
              else HasKey x xs

  public export
  UniqueKeys : Eq a => List (a, b) -> Bool
  UniqueKeys [] = True
  UniqueKeys ((x, y) :: xs) = not (HasKey x xs) && UniqueKeys xs

  public export
  mapValues : (a -> b) -> List (k, a) -> List (k, b)
  mapValues f [] = []
  mapValues f ((x, y) :: xs) = (x, f y) :: mapValues f xs

  public export
  kElemVect : (xs : List (k, a)) -> Vect (length xs) (i : k ** KElem i xs)
  kElemVect [] = []
  kElemVect ((i,j) :: xs) = (i ** KHere) :: ( (\(i ** j) => (i ** KThere j)) <$> kElemVect xs)


namespace Vect
  public export
  data KElem : a -> Vect n (a, b) -> Type where
    KHere : KElem k ((k,v) :: xs)
    KThere : KElem k xs -> KElem k (x::xs)

  public export
  index2 : Fin n -> Vect n (a, b) -> b
  index2 x xs = snd $ index x xs

  public export
  kElemToFin :  {0 ts : Vect n (a, b)} -> KElem k ts -> Fin n
  kElemToFin KHere = FZ
  kElemToFin (KThere x) = FS $ kElemToFin x

  public export
  klookup : (ts : Vect n (a, b)) -> KElem k ts -> b
  klookup ts x = index2 (kElemToFin x)  ts

  public export
  AllI : (f : b -> Type) -> Vect n (a, b) -> Type
  AllI f [] = ()
  AllI f ((k,v) :: xs) = (f v, AllI f xs)

  public export
  HasKey : Eq a => a -> Vect n (a, b) -> Bool
  HasKey x [] = False
  HasKey x ((y, z) :: xs) = 
    if x == y then True
              else HasKey x xs

  public export
  UniqueKeys : Eq a => Vect n (a, b) -> Bool
  UniqueKeys [] = True
  UniqueKeys ((x, y) :: xs) = not (HasKey x xs) && UniqueKeys xs

  public export
  HasValue : Eq a => a -> Vect n a -> Bool
  HasValue x [] = False
  HasValue x (y :: xs) = 
    if x == y then True
              else HasValue x xs

  public export
  UniqueValues : Eq a => Vect n a -> Bool
  UniqueValues [] = True
  UniqueValues (x :: xs) = not (HasValue x xs) && UniqueValues xs

  public export
  mapValues : (a -> b) ->  Vect n (k, a) -> Vect n (k, b)
  mapValues f [] = []
  mapValues f ((x, y) :: xs) = (x, f y) :: mapValues f xs

  public export
  mapValuesWithKey : (k -> a -> b) ->  Vect n (k, a) -> Vect n (k, b)
  mapValuesWithKey f [] = []
  mapValuesWithKey f ((x, y) :: xs) = (x, f x y) :: mapValuesWithKey f xs

  public export
  findKey : Eq a => a -> Vect n (a, b) -> Maybe (Fin n)
  findKey x [] = 
    Nothing
  findKey x ((y, z) :: xs) = 
    if x == y then Just FZ
              else FS <$> findKey x xs

public export
listToVect : (x : List a) -> Vect (length x) a
listToVect [] = []
listToVect (x :: xs) = x :: listToVect xs

namespace Record
  public export
  data Entry : String -> Type -> Type where
    MkEntry : (0 s : String) -> t -> Entry s t

  public export
  value : Entry s t -> t
  value (MkEntry s y) = y

  infixr 4 ^=

  public export
  (^=) : (0 s : String) -> t -> Entry s t
  (^=) = MkEntry 

  export
  {s : String} -> Show t => Show (Entry s t) where
    show (MkEntry s x) = s ++ "^= " ++ show x

  public export
  data Record : Vect n (String, Type) -> Type where
    Nil : Record []
    (::) :  Entry s t -> Record ts -> Record ((s,t) :: ts)
  
  public export
  Record' : List (String, Type) -> Type
  Record' x = Record $ listToVect x

  public export
  valueIndex : (k : Fin n) -> {0 ts : Vect n (String, Type)} -> Record ts -> Vect.index2 k ts
  valueIndex FZ ((MkEntry s x) :: y) = x
  valueIndex (FS y) (x :: z) = valueIndex y z


  public export
  get : {0 ts : Vect n (String, Type)} -> {auto prf : So (UniqueKeys ts)} -> (s : String) -> 
         {auto p : KElem s ts} -> Record ts -> klookup ts p
  get s x = valueIndex (kElemToFin p) x

  export
  AllI Eq ts => Eq (Record ts) where
    [] == [] = True
    ((MkEntry s x) :: z) == ((MkEntry s y) :: w) = x == y && z == w

  export
  {ts : Vect n (String, Type)} -> AllI Show ts => Show (Record ts) where
    show {ts = []} x =
      "[]"
    show {ts = ((s, z) :: [])} ((MkEntry s x) :: []) =
      "[\{s}^=\{show x}]"
    show {ts = ((s, w) :: (z :: l))} ((MkEntry s x) :: r) =
      "[\{s}^=\{show x}, " ++ (let z = show r in substr 1 (length z) z)

namespace Tree
  public export
  data Tree : Vect n (String, (Type -> Type)) -> Type where
    MkTree : (k : Fin n) -> (index2 k ts) (Tree ts) -> Tree ts

  public export
  Tree' : List (String, (Type -> Type)) -> Type
  Tree' x = Tree $ listToVect x

  public export
  N : {ts : Vect n (String, Type -> Type)} -> {auto 0 prf : So (UniqueKeys ts)} -> (s : String) -> 
         {auto p : KElem s ts} -> (klookup ts p) (Tree ts) -> Tree ts
  N {ts} s {p} x with (kElemToFin p)
    N {ts} s {p} x | i = MkTree i x

  public export
  E : {ts : Vect n (String, Type -> Type)} -> {auto 0 prf : So (UniqueKeys ts)} -> (s : String) -> 
         {auto p : KElem s ts} -> 
            {auto pU : snd (index (kElemToFin p) ts) (Tree ts) = Unit} ->
              Tree ts
  E {ts} s {p} = MkTree (kElemToFin p) (rewrite pU in ())

namespace Variant
  public export
  Variant : Vect n (String, Type) -> Type
  Variant xs = Tree ((\(s, t) => (s, const t)) <$> xs)

namespace Enum
  public export
  Enum : Vect n String -> Type
  Enum xs = Tree ((\s => (s, const ())) <$> xs)


testEnum_E : Enum ["a", "b"]
testEnum_E = E "a"

