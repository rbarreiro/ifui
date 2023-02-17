module Ifui.ExtensibleRecords

import public Data.So
import public Decidable.Equality

%hint
public export
strNotEq : {a : String} -> {b : String} -> {auto pNeq : So (a /= b)} -> Not (a = b)
strNotEq Refl {pNeq} = believe_me pNeq

%hint
public export
strPairNotEq : {a : (String, String)} -> {b : (String, String)} -> {auto pNeq : So (a /= b)} -> Not (a = b)
strPairNotEq Refl {pNeq} = believe_me pNeq

namespace UList
  mutual
    public export
    data UList : Type -> Type where
      Nil : UList a
      (::) : (x : a) -> (l : UList a) -> {auto p : UListCanPrepend x l} -> UList a

    public export
    data UListCanPrepend : (x : a) -> (l : UList a) -> Type where
      UListCanPrependNil : UListCanPrepend x Nil
      UListCanPrependCons : Not (k = tk) -> UListCanPrepend  v l -> UListCanPrepend k ((::) tk l {p})

  public export
  data Elem : k -> UList k -> Type where
    Here : {auto p : UListCanPrepend k xs} -> Elem k ((::) k xs {p = p})
    There : {auto p : UListCanPrepend y xs} -> Elem k xs -> Elem k ((::) y xs {p = p})

  export
  calcUListElem : DecEq a => (s : a) -> (l : UList a) -> Maybe (Elem s l)
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

namespace UKeyList
  mutual
    public export
    data UKeyList : Type -> Type -> Type where
      Nil : UKeyList a b
      (::) : (x : (a,b)) -> (l : UKeyList a b) -> {auto p : UKeyListCanPrepend x l} -> UKeyList a b

    public export
    data UKeyListCanPrepend : (x : (a,b)) -> (l : UKeyList a b) -> Type where
      UKeyListCanPrependNil : UKeyListCanPrepend x Nil
      UKeyListCanPrependCons : Not (k = tk) -> UKeyListCanPrepend (k, v) l -> UKeyListCanPrepend (k,v) ((::) (tk, tv) l {p})

  public export
  data Elem : k -> v -> UKeyList k v -> Type where
    Here : {auto p : UKeyListCanPrepend (k,v)  xs} -> Elem k v ((::) (k, v) xs {p = p})
    There : {auto p : UKeyListCanPrepend y  xs} -> Elem k v xs -> Elem k v ((::) y xs {p = p})

  public export
  data KElem : k -> UKeyList k v -> Type where
    KHere : {auto p : UKeyListCanPrepend (k, v)  xs} -> KElem k ((::) (k, v) xs {p = p})
    KThere : {auto p : UKeyListCanPrepend y  xs} -> KElem k xs -> KElem k ((::) y xs {p = p})

  public export
  klookup : (ts : UKeyList a b)  -> (KElem k ts) -> b
  klookup ((k, v) :: xs) KHere = v
  klookup (y :: xs) (KThere x) = klookup xs x

  public export
  lookup' : (ts : UKeyList a b) -> (k : a) -> {auto p : KElem k ts} -> b
  lookup' ((_, v) :: _) k {p = KHere} = v
  lookup' (y :: xs) k {p = (KThere x)} = lookup' xs k {p = x}

  export
  calcCanPrepend : DecEq keys  => (x : (keys, b)) -> (l : UKeyList keys b) -> Maybe (UKeyListCanPrepend x l)
  calcCanPrepend x [] = 
    Just UKeyListCanPrependNil
  calcCanPrepend (k,v) ((tk, tv) :: l) = 
    do
      pl <- calcCanPrepend (k,v) l
      case decEq k tk of
           (Yes prf) => Nothing
           (No contra) => Just $ UKeyListCanPrependCons contra pl

  mutual
    public export
    mapValues : (a -> b) -> UKeyList k a -> UKeyList k b
    mapValues f [] = []
    mapValues f ((::) {p=prf}  (x, y) l) = (::) (x, f y) (mapValues f l) {p = mapValuesCanPrepend prf} 

    public export
    mapValuesCanPrepend : UKeyListCanPrepend (x, y) l -> UKeyListCanPrepend (x, f y) (mapValues f l)
    mapValuesCanPrepend UKeyListCanPrependNil = 
      UKeyListCanPrependNil
    mapValuesCanPrepend (UKeyListCanPrependCons g z) =
      let aux = mapValuesCanPrepend z
      in UKeyListCanPrependCons g aux

  export
  Functor (UKeyList k) where
    map = mapValues

  export
  Foldable (UKeyList k) where
    foldr f i [] = i
    foldr f i (x :: l) = f (snd x) (foldr f i l)

  mutual
    export
    fromAllJust : DecEq keys => UKeyList keys (Maybe b) -> Maybe (UKeyList keys b)
    fromAllJust [] = Just []
    fromAllJust ((::) {p=prf} (x, Nothing) l) = Nothing
    fromAllJust ((::) {p=prf} (x, (Just y)) l) = case fromAllJust l of
                                                      Nothing => Nothing 
                                                      (Just z) => case calcCanPrepend (x, y) z of
                                                                       Nothing => Nothing
                                                                       (Just w) => Just $ (::) (x, y) z {p=w}

  public export
  length : UKeyList a b -> Nat
  length [] = Z
  length (x :: l) = S $ length l

--  export
--  calcUKeyListElem : (s : String) -> (l : UKeyList String b) -> Maybe ((x : b ** Elem s x l))
--  calcUKeyListElem s [] = 
--    Nothing
--  calcUKeyListElem s ((x, y) :: l) =
--    case decEq s x of
--         (Yes prf) => 
--            Just $ (y ** rewrite prf in Here)
--         (No contra) => 
--            (\(z**w) => (z ** There w)) <$> calcUKeyListElem s l



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
  (::) : {auto p : UKeyListCanPrepend (s, t) ts} -> Entry s t -> Record ts -> Record ((s,t) :: ts)

export
{s : String} -> Show t => Show (Entry s t) where
  show (MkEntry s x) = s ++ "^= " ++ show x

public export
interface HasValue (0 k : a) (0 t : b) (0 ts : UKeyList a b) | a, ts where
  hasValue : Elem k t ts


export
{0 k : a} -> {0 t : b} -> {0 ts : UKeyList a b} -> {p : UKeyListCanPrepend (k, t) ts} -> HasValue k t ((k,t)::ts) where
  hasValue = Here

export
{0 k : a} -> {0 t : b} -> {0 o : (a, b)} -> {0 ts : UKeyList a b} -> {p : UKeyListCanPrepend o ts} -> HasValue k t ts  => HasValue k t (o::ts) where
  hasValue = There $ hasValue {ts=ts}

getAux : Elem k t ts -> Record ts -> t
getAux Here ((MkEntry _ x) :: _) = x
getAux (There x) (_ :: z) = getAux x z

export
get : {0 t : Type} -> {0 ts : FieldList} -> (k : String) ->  HasValue k t ts => Record ts -> t
get k x = getAux (hasValue {k=k} {t=t} {ts=ts}) x

export
Eq (Record []) where
  (==) x y = True

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (Eq t, Eq (Record ts)) => Eq (Record ((s, t) :: ts)) where
  (==) ((MkEntry s x) :: z) ((MkEntry s y) :: w) = x == y && z == w

export
Show (Record []) where
  show x = "[]"

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (Show (Entry s t), Show (Record ts)) => Show (Record ((s, t) :: ts)) where
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
  weakenVariant : {auto p : UKeyListCanPrepend (s, t) ts} -> Variant ts -> Variant ((s,t):: ts)
  weakenVariant (MkVariant k v y) = MkVariant k v (There y)
  
  export
  Show (Variant []) where
    show (MkVariant _ _ Here) impossible
    show (MkVariant _ _ (There later)) impossible
 
  export
  {s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (Show  t, Show (Variant ts)) => Show (Variant ((s, t) :: ts)) where
    show (MkVariant k v Here) = "(" ++ k ++ " -= " ++ show v ++ ")"
    show (MkVariant k v (There later)) = show (MkVariant k v later)

namespace Tree
  public export
  data Tree : UKeyList String (Type -> Type) -> Type where
    N : (s : String) -> {auto p : KElem s ts} -> ((klookup ts p) (Tree ts))  -> Tree ts
