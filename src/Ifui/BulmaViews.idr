module Ifui.BulmaViews

import Ifui
import Ifui.Bulma
import Ifui.PureExpressions


public export
interface View a where
  bulmaView : a -> Widget b


export
View String where
  bulmaView = text

export
Num a => Show a => View (List a) where
  bulmaView x = text $ show x

export
{s : String} -> View a  => View (Entry s a) where
  bulmaView x = labeled s (bulmaView x)

export
View a => View (List (String, a)) where
  bulmaView xs = div ((\(k,v) => labeled k (bulmaView v)) <$> xs)

export
{ts : Vect n (String, Type)} -> AllI View ts =>
                View (Record ts) where
  bulmaView {ts = []} x = neutral
  bulmaView {ts = ((y, z) :: xs)} (x :: w) = bulmaView x <+> bulmaView w

public export
interface View1 (0 f : Type -> Type) where
  bulmaView1 : (a -> Widget b) -> f a -> Widget b

export
View c => View1 (const c) where
  bulmaView1 cont x = bulmaView x



TreeHeadsView : Vect n (String, Type -> Type) -> Type -> Type -> Vect n (String, Type)
TreeHeadsView zs a b = (mapValues (\f => (a -> Widget b) -> f a -> Widget b) zs)

total
branchView : (ts : Vect n (String, Type -> Type)) -> (k : Fin n) -> index2 k (TreeHeadsView ts a b) -> 
                        (index2 k ts a) -> (a -> Widget b) -> Widget b
branchView [] FZ _ _ _ impossible
branchView [] (FS w) _ _ _ impossible
branchView ((w, v) :: xs) FZ f y cont = f cont y
branchView ((w, v) :: xs) (FS s) f y cont = branchView xs s f y cont

kview1 : {ts : Vect n (String, Type -> Type)} -> (i : AllI View1 ts) => (k : Fin n) -> index2 k (TreeHeadsView ts a b)
kview1 {ts = ((x, y) :: xs)} FZ = bulmaView1 {f = y} 
kview1 {ts = ((y, z) :: xs)} (FS x) = kview1 {ts = xs} x

treeView : {ts : Vect n (String, Type -> Type)} -> (i : AllI View1 ts) => Tree ts -> Widget b
treeView {ts} {i} (MkTree k x) =
  (text $ fst $ index k ts) <+> branchView ts k (kview1 k) x treeView


export
{ts : Vect n (String, Type -> Type)} -> AllI View1 ts =>
                View (Tree ts) where
  bulmaView x = treeView x


export
View PPTy where
  bulmaView = text . show

export
View PTy where
  bulmaView = text . show

export
View TreeNodeKind where
  bulmaView = text . show

export
View (Pexp c a) where
  bulmaView = text . show

export
(View a, {y : a} -> View (p y)) => View (DPair a p) where
  bulmaView (i ** j) = div [bulmaView i, bulmaView j]

