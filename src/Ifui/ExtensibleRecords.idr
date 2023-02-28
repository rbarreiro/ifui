module Ifui.ExtensibleRecords

import public Data.So
import public Decidable.Equality

namespace UList
  mutual
    public export
    data UList : Type -> Type where
      Nil : UList a
      (::) : DecEq a => (x : a) -> (l : UList a) -> {auto p : So (KeyNotInUList x l)} -> UList a

    public export
    KeyNotInUList : a -> UList a -> Bool
    KeyNotInUList x [] = 
      True
    KeyNotInUList x (y :: ys) = 
      case decEq x y of
           (Yes prf) => False
           (No contra) => KeyNotInUList x ys

  public export
  CanPrepend : a -> UList a ->  Type
  CanPrepend x l = So (KeyNotInUList x l)

  public export
  data Elem : a -> UList a -> Type where
    Here : {0 x : a} -> DecEq a => {auto p : CanPrepend x xs} -> Elem x ((::) x xs {p = p})
    There : {0 x : a} -> DecEq a => {auto p :  CanPrepend x xs} -> Elem y xs -> Elem y ((::) x xs {p = p})

  export
  calcUListElem : (s : a) -> (l : UList a) -> Maybe (Elem s l)
  calcUListElem s [] = 
    Nothing
  calcUListElem s (x :: l) =
    case decEq s x of
         (Yes prf) => 
            Just $ (rewrite prf in Here)
         (No contra) => 
            There <$> calcUListElem s l

  export
  Foldable UList where
    foldr f i [] = i
    foldr f i (x :: l) = f x (foldr f i l)

  testMakeUList : UList String
  testMakeUList = ["a", "b"]

namespace UKeyList
  mutual
    public export
    data UKeyList : Type -> Type -> Type where
      Nil : UKeyList a b
      (::) : (d : DecEq a) => (x : (a,b)) -> (l : UKeyList a b) -> {auto p : So (KeyNotInUKeyList (Builtin.fst x) l)} -> UKeyList a b

    public export
    KeyNotInUKeyList : a -> UKeyList a b -> Bool
    KeyNotInUKeyList x [] = True
    KeyNotInUKeyList x ((y, _) :: ys) = 
      case decEq x y of
           (Yes prf) => False
           (No contra) => KeyNotInUKeyList x ys

  public export
  CanPrepend :  (a, b) -> UKeyList a b -> Type
  CanPrepend x l = So (KeyNotInUKeyList (Builtin.fst x) l)

  public export
  CanPrependKey : a -> UKeyList a b -> Type
  CanPrependKey x l = So (KeyNotInUKeyList x l)

  public export
  data Elem : k -> v -> UKeyList k v -> Type where
    Here : {0 k : a} -> DecEq a => {auto p : CanPrependKey k xs} -> Elem k v ((::) (k, v) xs {p = p})
    There : {0 k : a} -> DecEq a => {auto p : CanPrepend y xs} -> Elem k v xs -> Elem k v ((::) y xs {p = p})

  public export
  data KElem : k -> UKeyList k v -> Type where
    KHere : {0 k : a} -> DecEq a => {auto p : CanPrependKey k xs} -> KElem k ((::) (k, v) xs {p = p})
    KThere : {0 k : a} -> DecEq a => {auto p : CanPrepend y xs} -> KElem k xs -> KElem k ((::) y xs {p = p})

  public export
  klookup : (ts : UKeyList a b)  -> (KElem k ts) -> b
  klookup ((k, v) :: xs) KHere = v
  klookup (y :: xs) (KThere x) = klookup xs x

  public export
  lookup' : (ts : UKeyList a b) -> (k : a) -> {auto p : KElem k ts} -> b
  lookup' ((_, v) :: _) k {p = KHere} = v
  lookup' (y :: xs) k {p = (KThere x)} = lookup' xs k {p = x}

  mutual
    public export
    mapValues : (a -> b) -> UKeyList k a -> UKeyList k b
    mapValues f [] = []
    mapValues f ((::) {p=prf}  (x, y) l) = (::) (x, f y) (mapValues f l) {p = mapValuesPrf x f l prf} 

    public export
    mapValuesPrf : (x : k) -> (a -> b) -> (l : UKeyList k a) -> So (KeyNotInUKeyList x l) -> So (KeyNotInUKeyList x (mapValues f l))
    mapValuesPrf x g [] y = y
    mapValuesPrf x g ((z, w) :: l) y with (decEq x z)
      mapValuesPrf x g ((z, w) :: l) y | Yes prf = y
      mapValuesPrf x g ((z, w) :: l) y | No contra = mapValuesPrf x g l y

  export
  Functor (UKeyList k) where
    map = mapValues

  export
  Foldable (UKeyList k) where
    foldr f i [] = i
    foldr f i (x :: l) = f (snd x) (foldr f i l)

  export
  calcCanPrependKey : (x : k) -> (xs : UKeyList k b) -> Maybe (CanPrependKey x xs)
  calcCanPrependKey x [] = Just Oh
  calcCanPrependKey x ((y, z) :: l) with (decEq x y)
    calcCanPrependKey x ((y, z) :: l) | Yes prf = Nothing
    calcCanPrependKey x ((y, z) :: l) | No contra = calcCanPrependKey x l

  export
  fromAllJust : UKeyList keys (Maybe b) -> Maybe (UKeyList keys b)
  fromAllJust [] = Just []
  fromAllJust ((::) {p=prf} (x, Nothing) l) = Nothing
  fromAllJust ((::) {p=prf} (x, (Just y)) l) = case fromAllJust l of
                                                      Nothing => Nothing 
                                                      (Just z) => case calcCanPrependKey x z of
                                                                      Nothing => Nothing
                                                                      (Just w) => Just $ (::) (x, y) z {p=w}

  public export
  length : UKeyList a b -> Nat
  length [] = Z
  length (x :: l) = S $ length l


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

public export
FieldList : Type
FieldList = UKeyList String Type

public export
data Record : FieldList -> Type where
  Nil : Record []
  (::) : DecEq String => {auto p : CanPrependKey s ts} -> Entry s t -> Record ts -> Record ((s,t) :: ts)

export
{s : String} -> Show t => Show (Entry s t) where
  show (MkEntry s x) = s ++ "^= " ++ show x

public export
get : (k : String) -> Record ts -> {auto p : KElem k ts} -> klookup ts p
get k ((MkEntry k x) :: _) {p = KHere} = x
get k (_ :: z) {p = (KThere y)} = get k z {p = y}

export
Eq (Record []) where
  (==) x y = True

export
{s : String} -> {p : CanPrependKey s ts} -> (Eq t, Eq (Record ts)) => Eq (Record ((s, t) :: ts)) where
  (==) ((MkEntry s x) :: z) ((MkEntry s y) :: w) = x == y && z == w

export
Show (Record []) where
  show x = "[]"

export
{s : String} -> {p : CanPrependKey s ts} -> (Show (Entry s t), Show (Record ts)) => Show (Record ((s, t) :: ts)) where
  show (x :: []) = "[" ++ show x ++ "]"
  show (x :: (y :: r)) = "[" ++ show x  ++ ","  ++ (let z = show (y :: r) in substr 1 (length z) z)

namespace StringEnum
  public export
  data StringEnum : UList String -> Type where
    SE : (s : String) -> {auto p : Elem s xs} -> StringEnum xs
  
  public export
  toStringEnum : {l : UList String}  -> String -> Maybe (StringEnum l)
  toStringEnum str = 
    do
      prf <- calcUListElem str l
      pure $ SE str

  public export
  Cast (StringEnum l) String where
    cast (SE s) = s


namespace Variant
  public export
  data Variant : FieldList -> Type where
    MkVariant : (s : String) -> t -> Elem s t ts -> Variant ts

  infixr 4 -=

  public export
  (-=) : (s : String) -> t -> {auto p : Elem s t ts} -> Variant ts
  (-=) x y {p} = MkVariant x y p 

  public export
  weakenVariant : {auto p : CanPrependKey s ts} -> Variant ts -> Variant ((s,t):: ts)
  weakenVariant (MkVariant k v y) = MkVariant k v (There y)
  
  export
  Show (Variant []) where
    show (MkVariant _ _ Here) impossible
    show (MkVariant _ _ (There later)) impossible
 
  export
  {s : String} -> {p : CanPrependKey s ts} -> (Show  t, Show (Variant ts)) => Show (Variant ((s, t) :: ts)) where
    show (MkVariant k v Here) = "(" ++ k ++ " -= " ++ show v ++ ")"
    show (MkVariant k v (There later)) = show (MkVariant k v later)

namespace Tree
  public export
  data Tree : UKeyList String (Type -> Type) -> Type where
    N : (s : String) -> {auto p : KElem s ts} -> ((klookup ts p) (Tree ts))  -> Tree ts


public export
AllI : (f : Type -> Type) -> UKeyList String Type -> Type
AllI f []        = ()
AllI f ((k,v) :: xs) = (f v, AllI f xs)

public export
AllI1 : (f : (Type -> Type) -> Type) -> UKeyList String (Type -> Type) -> Type
AllI1 f []        = ()
AllI1 f ((k,v) :: xs) = (f v, AllI1 f xs)

