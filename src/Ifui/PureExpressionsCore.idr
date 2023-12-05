module Ifui.PureExpressionsCore

import public Decidable.Equality
import public Data.Vect

mutual
  public export
  data TreeNodeKind = NamedSubTrees
                    | Leaf PTy
                    | OneChild
                    | ValueAndOneChild PTy


  public export
  data PPTy = PPList
            | PPFun PPTy PPTy
  
  public export
  data PTy = PString
           | PBool
           | PUnit
           | PInt
           | PList PTy
           | PNat
           | PDouble
           | PTensor (List Nat) PTy
           | PTuple PTy PTy
           | PFun PTy PTy
           | PRecord (List (String, PTy))
           | PTree (List (String, TreeNodeKind))
           | PForall PPTy
           | PPDF

public export
TreeNodeKindPTy : TreeNodeKind -> PTy -> PTy
TreeNodeKindPTy NamedSubTrees = \x => PList (PTuple PString x)
TreeNodeKindPTy (Leaf t) = const t
TreeNodeKindPTy OneChild = \x => x
TreeNodeKindPTy (ValueAndOneChild t) = \x => PTuple t x

export
Uninhabited (PString = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PString) where
  uninhabited Refl impossible
export
Uninhabited (PString = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PString) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PString = PRecord x) where
  uninhabited Refl impossible
export
Uninhabited (PString = PTree x) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PRecord x) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PTree x) where
  uninhabited Refl impossible
export
Uninhabited ((PFun x y) = PRecord z) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PTree z) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PString) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PFun y z) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PTree y) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PString) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PFun y z) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PRecord y) where
  uninhabited Refl impossible
export
Uninhabited (NamedSubTrees = Leaf x) where
  uninhabited Refl impossible
export
Uninhabited (Leaf x = NamedSubTrees) where
  uninhabited Refl impossible
export
Uninhabited (PString = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PString = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PString) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PFun a y) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PRecord x) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PTree x) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PString) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PFun a y) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PRecord x) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PTree x) where
  uninhabited Refl impossible
export
Uninhabited (NamedSubTrees = OneChild) where
  uninhabited Refl impossible
export
Uninhabited (NamedSubTrees = ValueAndOneChild x) where
  uninhabited Refl impossible
export
Uninhabited (Leaf y = ValueAndOneChild x) where
  uninhabited Refl impossible
export
Uninhabited (Leaf y = OneChild) where
  uninhabited Refl impossible
export
Uninhabited (OneChild = NamedSubTrees) where
  uninhabited Refl impossible
export
Uninhabited (OneChild = Leaf x) where
  uninhabited Refl impossible
export
Uninhabited (OneChild = ValueAndOneChild x) where
  uninhabited Refl impossible
export
Uninhabited (ValueAndOneChild x = NamedSubTrees) where
  uninhabited Refl impossible
export
Uninhabited (ValueAndOneChild x = Leaf y) where
  uninhabited Refl impossible
export
Uninhabited (ValueAndOneChild x = OneChild) where
  uninhabited Refl impossible
export
Uninhabited (PString = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PString = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PString = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PString = PTensor x y) where
  uninhabited Refl impossible
export
Uninhabited (PString = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PTensor x y) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PList z) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PTensor z w) where
  uninhabited Refl impossible
export
Uninhabited (PFun x y = PTuple z w) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PList y) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PTensor y z) where
  uninhabited Refl impossible
export
Uninhabited (PRecord x = PTuple y z) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PList y) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PTensor y z) where
  uninhabited Refl impossible
export
Uninhabited (PTree x = PTuple y z) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PTensor x y) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PTensor x y) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PString) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PString) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PString) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PString) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PString) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PString = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PFun xx yy = PForall zz) where
  uninhabited Refl impossible
export
Uninhabited (PRecord xx = PForall yy) where
  uninhabited Refl impossible
export
Uninhabited (PTree xx = PForall yy) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PForall yy) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PForall xx) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PForall zz) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PForall zz) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PString) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PPList = PPFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PPFun x z = PPList) where
  uninhabited Refl impossible
export
Uninhabited (PString = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PFun xx yy = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PRecord xx = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PTree xx = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PUnit = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PInt = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PList xx = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PNat = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PDouble = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PTensor xx yy = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PTuple xx yy = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PForall xx = PPDF) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PString) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PUnit) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PInt) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PList x) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PNat) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PDouble) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PTensor ks x) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PTuple x y) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PFun x y) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PRecord xs) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PTree xs) where
  uninhabited Refl impossible
export
Uninhabited (PPDF = PForall x) where
  uninhabited Refl impossible

export
Biinjective PFun where
  biinjective Refl = (Refl, Refl)

export
Biinjective PTensor where
  biinjective Refl = (Refl, Refl)

export
Biinjective PTuple where
  biinjective Refl = (Refl, Refl)

export
Injective PRecord where
  injective Refl = Refl

export
Injective PTree where
  injective Refl = Refl

export
Injective Leaf where
  injective Refl = Refl

export
Injective PList where
  injective Refl = Refl

export
Injective ValueAndOneChild where
  injective Refl = Refl

export
Injective PForall where
  injective Refl = Refl

export
Biinjective PPFun where
  biinjective Refl = (Refl, Refl)

export
DecEq PPTy where
  decEq PPList PPList  = Yes Refl
  decEq PPList (PPFun x y)  = No absurd
  decEq (PPFun x z) PPList  = No absurd
  decEq (PPFun x z) (PPFun y w) = decEqCong2 (decEq x y) (decEq z w)

mutual
  export
  DecEq TreeNodeKind where
    decEq NamedSubTrees NamedSubTrees = Yes Refl
    decEq NamedSubTrees (Leaf x) = No absurd
    decEq (Leaf _) NamedSubTrees = No absurd
    decEq (Leaf x) (Leaf y) = decEqCong (decEq x y)
    decEq NamedSubTrees OneChild = No absurd
    decEq NamedSubTrees (ValueAndOneChild _) = No absurd
    decEq (Leaf _) OneChild = No absurd
    decEq (Leaf _) (ValueAndOneChild _) = No absurd
    decEq OneChild NamedSubTrees = No absurd
    decEq OneChild (Leaf x) = No absurd
    decEq OneChild OneChild = Yes Refl
    decEq OneChild (ValueAndOneChild x) = No absurd
    decEq (ValueAndOneChild _) NamedSubTrees = No absurd
    decEq (ValueAndOneChild _) (Leaf x) = No absurd
    decEq (ValueAndOneChild _) OneChild = No absurd
    decEq (ValueAndOneChild x) (ValueAndOneChild y) = decEqCong (decEq x y)


  export
  DecEq PTy where
    decEq PString PString = Yes Refl
    decEq PString PBool = No absurd
    decEq PString (PFun _ _) = No absurd
    decEq PBool PString = No absurd
    decEq PBool PBool = Yes Refl
    decEq PBool (PFun _ _) = No absurd
    decEq (PFun _ _) PString = No absurd
    decEq (PFun _ _) PBool = No absurd
    decEq (PFun x z) (PFun y w) = decEqCong2 (decEq x y) (decEq z w)
    decEq PString (PRecord _) = No absurd
    decEq PString (PTree _) = No absurd
    decEq PBool (PRecord _) = No absurd
    decEq PBool (PTree _) = No absurd
    decEq (PFun _ _) (PRecord _) = No absurd
    decEq (PFun _ _) (PTree _) = No absurd
    decEq (PRecord _) PString = No absurd
    decEq (PRecord _) PBool = No absurd
    decEq (PRecord _) (PFun x y) = No absurd
    decEq (PRecord xs) (PRecord ys) = decEqCong (decEq xs ys)
    decEq (PRecord _) (PTree xs) = No absurd
    decEq (PTree _) PString = No absurd
    decEq (PTree _) PBool = No absurd
    decEq (PTree _) (PFun _ _) = No absurd
    decEq (PTree _) (PRecord _) = No absurd
    decEq (PTree xs) (PTree ys) = decEqCong (decEq xs ys)
    decEq PString PUnit = No absurd
    decEq PString PInt = No absurd
    decEq PBool PUnit = No absurd
    decEq PBool PInt = No absurd
    decEq (PFun _ _) PUnit = No absurd
    decEq (PFun _ _) PInt = No absurd
    decEq (PRecord _) PUnit = No absurd
    decEq (PRecord _) PInt = No absurd
    decEq (PTree _) PUnit = No absurd
    decEq (PTree _) PInt = No absurd
    decEq PUnit PString = No absurd
    decEq PUnit PBool = No absurd
    decEq PUnit PUnit = Yes Refl
    decEq PUnit PInt = No absurd
    decEq PUnit (PFun x y) = No absurd
    decEq PUnit (PRecord xs) = No absurd
    decEq PUnit (PTree xs) = No absurd
    decEq PInt PString = No absurd
    decEq PInt PBool = No absurd
    decEq PInt PUnit = No absurd
    decEq PInt PInt = Yes Refl
    decEq PInt (PFun x y) = No absurd
    decEq PInt (PRecord xs) = No absurd
    decEq PInt (PTree xs) = No absurd
    decEq PString (PList _) = No absurd
    decEq PString PNat = No absurd
    decEq PString PDouble = No absurd
    decEq PString (PTensor _ _) = No absurd
    decEq PString (PTuple _ _) = No absurd
    decEq PBool (PList _) = No absurd
    decEq PBool PNat = No absurd
    decEq PBool PDouble = No absurd
    decEq PBool (PTensor _ _) = No absurd
    decEq PBool (PTuple _ _) = No absurd
    decEq (PFun _ _) (PList _) = No absurd
    decEq (PFun _ _) PNat = No absurd
    decEq (PFun _ _) PDouble = No absurd
    decEq (PFun _ _) (PTensor _ _) = No absurd
    decEq (PFun _ _) (PTuple _ _) = No absurd
    decEq (PRecord _) (PList _) = No absurd
    decEq (PRecord _) PNat = No absurd
    decEq (PRecord _) PDouble = No absurd
    decEq (PRecord _) (PTensor _ _) = No absurd
    decEq (PRecord _) (PTuple _ _) = No absurd
    decEq (PTree _) (PList _) = No absurd
    decEq (PTree _) PNat = No absurd
    decEq (PTree _) PDouble = No absurd
    decEq (PTree _) (PTensor _ _) = No absurd
    decEq (PTree _) (PTuple _ _) = No absurd
    decEq PUnit (PList _) = No absurd
    decEq PUnit PNat = No absurd
    decEq PUnit PDouble = No absurd
    decEq PUnit (PTensor _ _) = No absurd
    decEq PUnit (PTuple _ _) = No absurd
    decEq PInt (PList _) = No absurd
    decEq PInt PNat = No absurd
    decEq PInt PDouble = No absurd
    decEq PInt (PTensor _ _) = No absurd
    decEq PInt (PTuple _ _) = No absurd
    decEq (PList _) PString  = No absurd
    decEq (PList _) PBool  = No absurd
    decEq (PList _) PUnit  = No absurd
    decEq (PList _) PInt  = No absurd
    decEq (PList x) (PList y)  = decEqCong (decEq x y)
    decEq (PList _) PNat  = No absurd
    decEq (PList _) PDouble  = No absurd
    decEq (PList _) (PTensor ks x)  = No absurd
    decEq (PList _) (PTuple x y)  = No absurd
    decEq (PList _) (PFun x y)  = No absurd
    decEq (PList _) (PRecord xs)  = No absurd
    decEq (PList _) (PTree xs)  = No absurd
    decEq PNat PString  = No absurd
    decEq PNat PBool  = No absurd
    decEq PNat PUnit  = No absurd
    decEq PNat PInt  = No absurd
    decEq PNat (PList x)  = No absurd
    decEq PNat PNat  = Yes Refl
    decEq PNat PDouble  = No absurd
    decEq PNat (PTensor ks x)  = No absurd
    decEq PNat (PTuple x y)  = No absurd
    decEq PNat (PFun x y)  = No absurd
    decEq PNat (PRecord xs)  = No absurd
    decEq PNat (PTree xs)  = No absurd
    decEq PDouble PString  = No absurd
    decEq PDouble PBool  = No absurd
    decEq PDouble PUnit  = No absurd
    decEq PDouble PInt  = No absurd
    decEq PDouble (PList x)  = No absurd
    decEq PDouble PNat  = No absurd
    decEq PDouble PDouble  = Yes Refl
    decEq PDouble (PTensor ks x)  = No absurd
    decEq PDouble (PTuple x y)  = No absurd
    decEq PDouble (PFun x y)  = No absurd
    decEq PDouble (PRecord xs)  = No absurd
    decEq PDouble (PTree xs)  = No absurd
    decEq (PTensor _ _) PString  = No absurd
    decEq (PTensor _ _) PBool  = No absurd
    decEq (PTensor _ _) PUnit  = No absurd
    decEq (PTensor _ _) PInt  = No absurd
    decEq (PTensor _ _) (PList x)  = No absurd
    decEq (PTensor _ _) PNat  = No absurd
    decEq (PTensor _ _) PDouble  = No absurd
    decEq (PTensor x y) (PTensor xx yy)  = decEqCong2 (decEq x xx) (decEq y yy)
    decEq (PTensor _ _) (PTuple x y)  = No absurd
    decEq (PTensor _ _) (PFun x y)  = No absurd
    decEq (PTensor _ _) (PRecord xs)  = No absurd
    decEq (PTensor _ _) (PTree xs)  = No absurd
    decEq (PTuple _ _) PString  = No absurd
    decEq (PTuple _ _) PBool  = No absurd
    decEq (PTuple _ _) PUnit  = No absurd
    decEq (PTuple _ _) PInt  = No absurd
    decEq (PTuple _ _) (PList x)  = No absurd
    decEq (PTuple _ _) PNat  = No absurd
    decEq (PTuple _ _) PDouble  = No absurd
    decEq (PTuple _ _) (PTensor ks x)  = No absurd
    decEq (PTuple xx yy) (PTuple x y)  = decEqCong2 (decEq xx x) (decEq yy y)
    decEq (PTuple _ _) (PFun x y)  = No absurd
    decEq (PTuple _ _) (PRecord xs)  = No absurd
    decEq (PTuple _ _) (PTree xs)  = No absurd
    decEq PString (PForall _) = No absurd
    decEq PBool (PForall _) = No absurd
    decEq (PFun _ _) (PForall _) = No absurd
    decEq (PRecord _) (PForall _) = No absurd
    decEq (PTree _) (PForall _) = No absurd
    decEq PUnit (PForall _) = No absurd
    decEq PInt (PForall _) = No absurd
    decEq (PList _) (PForall _) = No absurd
    decEq PNat (PForall _) = No absurd
    decEq PDouble (PForall _) = No absurd
    decEq (PTensor _ _) (PForall _) = No absurd
    decEq (PTuple _ _) (PForall _) = No absurd
    decEq (PForall _) PString  = No absurd
    decEq (PForall _) PBool  = No absurd
    decEq (PForall _) PUnit  = No absurd
    decEq (PForall _) PInt  = No absurd
    decEq (PForall _) (PList x)  = No absurd
    decEq (PForall _) PNat  = No absurd
    decEq (PForall _) PDouble  = No absurd
    decEq (PForall _) (PTensor ks x)  = No absurd
    decEq (PForall _) (PTuple x y)  = No absurd
    decEq (PForall _) (PFun x y)  = No absurd
    decEq (PForall _) (PRecord xs)  = No absurd
    decEq (PForall _) (PTree xs)  = No absurd
    decEq (PForall x) (PForall y) = decEqCong (decEq x y)
    decEq PString PPDF = No absurd
    decEq PBool PPDF = No absurd
    decEq (PFun _ _) PPDF = No absurd
    decEq (PRecord _) PPDF = No absurd
    decEq (PTree _) PPDF = No absurd
    decEq PUnit PPDF = No absurd
    decEq PInt PPDF = No absurd
    decEq (PList _) PPDF = No absurd
    decEq PNat PPDF = No absurd
    decEq PDouble PPDF = No absurd
    decEq (PTensor _ _) PPDF = No absurd
    decEq (PTuple _ _) PPDF = No absurd
    decEq (PForall _) PPDF = No absurd
    decEq PPDF PString  = No absurd
    decEq PPDF PBool  = No absurd
    decEq PPDF PUnit  = No absurd
    decEq PPDF PInt  = No absurd
    decEq PPDF (PList x)  = No absurd
    decEq PPDF PNat  = No absurd
    decEq PPDF PDouble  = No absurd
    decEq PPDF (PTensor ks x)  = No absurd
    decEq PPDF (PTuple x y)  = No absurd
    decEq PPDF (PFun x y)  = No absurd
    decEq PPDF (PRecord xs)  = No absurd
    decEq PPDF (PTree xs)  = No absurd
    decEq PPDF (PForall x)  = No absurd
    decEq PPDF PPDF  = Yes Refl
