module Ifui.PureExpressions

import public Ifui.ExtensibleTypes
import Data.List.Elem
import Ifui.Json
import Decidable.Equality

mutual
  public export
  data TreeNodeKind = NamedSubTrees
                    | Leaf PTy

  public export
  data PTy = PString
           | PBool
           | PUnit
           | PInt
           | PFun PTy PTy
           | PRecord (List (String, PTy))
           | PTree (List (String, TreeNodeKind))

mutual
  public export
  TreeNodeKindType : TreeNodeKind -> Type -> Type
  TreeNodeKindType NamedSubTrees = \t => List (String, t)
  TreeNodeKindType (Leaf t) = const (PTyType t)

  public export
  PTyType : PTy -> Type
  PTyType PString = String
  PTyType PBool = Bool
  PTyType (PFun x y) = PTyType x -> PTyType y
  PTyType (PRecord xs) = Record (mapValues PTyType (Vect.fromList xs))
  PTyType (PTree xs) = Tree (mapValues TreeNodeKindType (Vect.fromList xs))
  PTyType PUnit = ()
  PTyType PInt = Int

public export
data Pexp : List (String, PTy) -> PTy -> Type where
    Var : (name : String) -> {auto p : KElem  name ctxt} -> Pexp ctxt (klookup ctxt p)
    Lambda : (arg : String) -> Pexp ((arg, a) :: ctxt) b ->  Pexp ctxt (PFun a  b)
    App : {a: PTy} -> Pexp ctxt (PFun a b) -> Pexp ctxt a -> Pexp ctxt b
    StringLit : String -> Pexp ctxt PString
    BoolLit : Bool -> Pexp ctxt PBool

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
Biinjective PFun where
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

mutual
  export
  DecEq TreeNodeKind where
    decEq NamedSubTrees NamedSubTrees = Yes Refl
    decEq NamedSubTrees (Leaf x) = No absurd
    decEq (Leaf _) NamedSubTrees = No absurd
    decEq (Leaf x) (Leaf y) = decEqCong (decEq x y)


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


mutual
  export
  JsonSerializable TreeNodeKind where
    toJson NamedSubTrees = JString "NamedSubTrees"
    toJson (Leaf x) = JArray [JString "Leaf", toJson x]
    
    fromJson (JString "NamedSubTrees") = Just NamedSubTrees 
    fromJson (JArray [JString "Leaf", x]) = Leaf <$> fromJson x
    fromJson _ = Nothing

  export
  JsonSerializable PTy where
    toJson PString = JString "PString"
    toJson PBool = JString "PBool"
    toJson (PFun x y) = JArray [JString "PFun", toJson x, toJson y]
    toJson (PRecord xs) = JArray [JString "PRecord", toJson (mapValues toJson xs)]
    toJson (PTree xs) = JArray [JString "PTree", toJson (mapValues toJson xs)]
    toJson PUnit = JString "PUnit"
    toJson PInt = JString "PInt"

    fromJson (JString "PString") = 
      Just PString
    fromJson (JString "PBool") = 
      Just PBool
    fromJson (JArray [JString "PFun", x, y]) = 
      Just $ PFun !(fromJson x) !(fromJson y)
    fromJson (JArray [JString "PRecord", xs]) =
      do
        xs_ <- the (List (String, JSON)) <$> fromJson xs
        xs__ <- sequence $ map (\(k,v) => (k,) <$> fromJson v) xs_
        pure $ PRecord xs__
    fromJson (JArray [JString "PTree", xs]) =
      do
        xs_ <- the (List (String, JSON)) <$> fromJson xs
        xs__ <- sequence $ map (\(k,v) => (k,) <$> fromJson v) xs_
        pure $ PTree xs__
    fromJson (JString "PUnit") = 
      Just PUnit
    fromJson (JString "PInt") = 
      Just PInt
    fromJson _ = Nothing 


elemToVar : (ctxt : List (String, PTy)) ->  Elem (n,a) ctxt -> (p : KElem n ctxt ** a = klookup ctxt p)
elemToVar ((n, a) :: xs) Here = (KHere ** Refl)
elemToVar ((y, z) :: xs) (There x) = let (g ** h) = elemToVar xs x in (KThere g ** h)

toJson_ : Pexp ctxt a -> JSON
toJson_ (Var name) = 
  JArray [JString "Var", JString name]
toJson_ (Lambda arg x) = 
  JArray [JString "Lambda", JString arg, toJson_ x]
toJson_ (App {a} x y) = 
  JArray [JString "App", toJson a, toJson_ x, toJson_ y]
toJson_ (StringLit str) = 
  JArray [JString "StringLit", JString str]
toJson_ (BoolLit x) = 
  JArray [JString "BoolLit", JBoolean x]

fromJson_ : (ctxt : List (String, PTy)) -> (a : PTy) -> JSON -> Maybe (Pexp ctxt a)
fromJson_ ctxt a (JArray [JString "Var", JString name]) = 
  case isElem (name, a) ctxt of
      (Yes prf) => 
          Just $ let (p ** h) = elemToVar ctxt prf in (rewrite h in  Var name {p = p})
      (No contra) => 
          Nothing
fromJson_ ctxt (PFun i j) (JArray [JString "Lambda", JString arg, body]) = 
  Just $ Lambda arg !(fromJson_ ((arg, i) :: ctxt) j body)
fromJson_ ctxt a (JArray [JString "App", i, f, x]) =
  do
    i_ <- the PTy <$> fromJson i
    f_ <- fromJson_ ctxt (PFun i_ a) f
    x_ <- fromJson_ ctxt i_ x
    pure $ App {a = i_} f_ x_
fromJson_ ctxt PString (JArray [JString "StringLit", JString s]) =
  Just $ StringLit s
fromJson_ ctxt PBool (JArray [JString "BoolLit", JBoolean b]) =
  Just $ BoolLit b
fromJson_ _ _ _ = 
  Nothing

export
{ctxt : List (String, PTy)} -> {a : PTy} -> JsonSerializable (Pexp ctxt a) where
  toJson = toJson
  fromJson = fromJson_ ctxt a

