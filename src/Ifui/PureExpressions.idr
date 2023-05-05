module Ifui.PureExpressions

import public Ifui.ExtensibleTypes
import Data.List.Elem
import Ifui.Json
import Decidable.Equality

public export
data PTy = PString
         | PBool
         | PFun PTy PTy

export
JsonSerializable PTy where
  toJson PString = JString "PString"
  toJson PBool = JString "PBool"
  toJson (PFun x y) = JArray [JString "PFun", toJson x, toJson y]

  fromJson (JString "PString") = Just PString
  fromJson (JString "PBool") = Just PBool
  fromJson (JArray [JString "PFun", x, y]) = Just $ PFun !(fromJson x) !(fromJson y)
  fromJson _ = Nothing 

export
Uninhabited (PString = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PBool = PString) where
  uninhabited Refl impossible
export
Uninhabited (PString = (PFun x y)) where
  uninhabited Refl impossible
export
Uninhabited ((PFun x y) = PString) where
  uninhabited Refl impossible
export
Uninhabited ((PFun x y) = PBool) where
  uninhabited Refl impossible
export
Uninhabited (PBool = (PFun x y)) where
  uninhabited Refl impossible

export
Biinjective PFun where
  biinjective Refl = (Refl, Refl)

export
DecEq PTy where
  decEq PString PString = Yes Refl
  decEq PString PBool = No absurd
  decEq PString (PFun x y) = No absurd
  decEq PBool PString = No absurd
  decEq PBool PBool = Yes Refl
  decEq PBool (PFun x y) = No absurd
  decEq (PFun x z) PString = No absurd
  decEq (PFun x z) PBool = No absurd
  decEq (PFun x z) (PFun y w) = decEqCong2 (decEq x y) (decEq z w)

public export
data Pexp : List (String, PTy) -> PTy -> Type where
    Var : (name : String) -> {auto p : KElem  name ctxt} -> Pexp ctxt (klookup ctxt p)
    Lambda : (arg : String) -> Pexp ((arg, a) :: ctxt) b ->  Pexp ctxt (PFun a  b)
    App : {a: PTy} -> Pexp ctxt (PFun a b) -> Pexp ctxt a -> Pexp ctxt b
    StringLit : String -> Pexp ctxt PString
    BoolLit : Bool -> Pexp ctxt PBool


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

