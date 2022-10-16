module Ifui.ExtensibleRecords

import public Data.List.Elem

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
data Record : List (String, Type) -> Type where
  Nil : Record []
  (::) : Entry s t -> Record ts -> Record ((s,t) :: ts)

export
{s : String} -> Show t => Show (Entry s t) where
  show (MkEntry s x) = s ++ "^= " ++ show x

export
Show (Record []) where
  show x = "[]"

export
{s : String} -> (Show (Entry s t), Show (Record ts)) => Show (Record ((s, t) :: ts)) where
  show (x :: []) = "[" ++ show x ++ "]"
  show (x :: (y :: r)) = "[" ++ show x  ++ ","  ++ (let z = show (y :: r) in substr 1 (length z) z)

public export
data Alt : List (String, Type) -> Type where
  MkAlt : Entry s t -> Elem (s, t) ts  -> Alt ts

public export
weakenAlt : Alt ts -> Alt ((s,t):: ts)
weakenAlt (MkAlt x y) = MkAlt x (There y)

export
Show (Alt []) where
  show (MkAlt _ Here) impossible
  show (MkAlt _ (There later)) impossible
 
export
{s : String} -> (Show (Entry s t), Show (Alt ts)) => Show (Alt ((s, t) :: ts)) where
  show (MkAlt x Here) = "(" ++ show x ++ ")"
  show (MkAlt x (There later)) = show (MkAlt x later)
