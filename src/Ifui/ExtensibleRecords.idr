module Ifui.ExtensibleRecords

import public Data.So

%hint
public export
strNotEq : {a : String} -> {b : String} -> {auto pNeq : So (a /= b)} -> Not (a = b)
strNotEq Refl {pNeq} = believe_me pNeq

%hint
public export
strPairNotEq : {a : (String, String)} -> {b : (String, String)} -> {auto pNeq : So (a /= b)} -> Not (a = b)
strPairNotEq Refl {pNeq} = believe_me pNeq

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
data Record : UKeyList String Type -> Type where
  Nil : Record []
  (::) : {auto p : UKeyListCanPrepend (s, t) ts} -> Entry s t -> Record ts -> Record ((s,t) :: ts)

export
{s : String} -> Show t => Show (Entry s t) where
  show (MkEntry s x) = s ++ "^= " ++ show x

public export
interface Get (k : String) (t : Type) (ts : UKeyList String Type) where
 get : Record ts -> t 

export
{k : String} -> {t : Type} -> {ts : UKeyList String Type} -> {p : UKeyListCanPrepend (k, t) ts} -> Get k t ((k,t)::ts) where
  get ((MkEntry s x) :: y) = x

export
{k : String} -> {t : Type} -> {o : (String, Type)} -> {ts : UKeyList String Type} -> {p : UKeyListCanPrepend o ts} -> Get k t ts  => Get k t (o::ts) where
  get (x :: y) = get {k=k} y

export
Show (Record []) where
  show x = "[]"

export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (Show (Entry s t), Show (Record ts)) => Show (Record ((s, t) :: ts)) where
  show (x :: []) = "[" ++ show x ++ "]"
  show (x :: (y :: r)) = "[" ++ show x  ++ ","  ++ (let z = show (y :: r) in substr 1 (length z) z)

public export
data Alt : UKeyList String Type -> Type where
  MkAlt : Entry s t -> Elem s t ts  -> Alt ts

public export
weakenAlt : {auto p : UKeyListCanPrepend (s, t) ts} -> Alt ts -> Alt ((s,t):: ts)
weakenAlt (MkAlt x y) = MkAlt x (There y)

export
Show (Alt []) where
  show (MkAlt _ Here) impossible
  show (MkAlt _ (There later)) impossible
 
export
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (Show (Entry s t), Show (Alt ts)) => Show (Alt ((s, t) :: ts)) where
  show (MkAlt x Here) = "(" ++ show x ++ ")"
  show (MkAlt x (There later)) = show (MkAlt x later)
