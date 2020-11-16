module Ifui.Changes

import Data.Vect

export
interface DChanges (a : Type) (f : a->a->Type) (g:a->Type) where
  applyDChanges : (x:a) -> (y:a) -> f x y -> g x -> g y

public export
data VectChanges : Type -> Nat -> Nat -> Type where
  setVect : (Vect m a) -> VectChanges a n m

public export
data StringChanges : (a:Type) -> a -> a -> Type where
  setString : String -> StringChanges a b c

export
DChanges a (\n,m => StringChanges a n m) (\n=>String)  where
  applyDChanges n m (setString str) x = str

export
DChanges Nat (\n,m => VectChanges b n m) (\n=>Vect n b)  where
  applyDChanges n m (setVect xs) x = xs
