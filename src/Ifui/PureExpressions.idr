module Ifui.PureExpressions

import public Ifui.ExtensibleTypes
import public Ifui.MediaTypes
import public Ifui.PureExpressionsCore
import Data.List.Elem
import Ifui.Json


infixr 0 .> 

public export
(.>) : PTy -> PTy -> PTy
(.>) = PFun

public export
data PrimFn : PTy -> Type where
 StringEq : PrimFn (PString .> PString .> PBool)
 ListNil : PrimFn (PForall PPList)

public export
data Pexp : List (String, PTy) -> PTy -> Type where
    Var : (name : String) -> {auto p : KElem  name ctxt} -> Pexp ctxt (klookup ctxt p)
    Lambda : (arg : String) -> Pexp ((arg, a) :: ctxt) b ->  Pexp ctxt (PFun a  b)
    Let : {a : PTy} -> (arg : String) -> Pexp ctxt a -> Pexp ((arg, a) :: ctxt) b -> Pexp ctxt b
    App : {a: PTy} -> Pexp ctxt (PFun a b) -> Pexp ctxt a -> Pexp ctxt b
    StringLit : String -> Pexp ctxt PString
    BoolLit : Bool -> Pexp ctxt PBool
    TreeLit : {k : String} -> (p : KElem k ts) -> Pexp ctxt (TreeNodeKindPTy (klookup ts p) (PTree ts)) -> Pexp ctxt (PTree ts)
    PUnitLit : Pexp ctxt PUnit
    NatLit : String -> Pexp ctxt PNat
    Prim : PrimFn a -> Pexp ctxt a

mutual
  public export
  TreeNodeKindType : TreeNodeKind -> Type -> Type
  TreeNodeKindType NamedSubTrees = \x => List (String, x)
  TreeNodeKindType (Leaf t) = const (PTyType t)
  TreeNodeKindType OneChild = \x => x
  TreeNodeKindType (ValueAndOneChild t) = \x => (PTyType t, x)

  public export
  PTyType : PTy -> Type
  PTyType PString = String
  PTyType PBool = Bool
  PTyType (PFun x y) = Pexp [] (PFun x y)
  PTyType (PRecord xs) = Record (mapValues PTyType (listToVect xs))
  PTyType (PTree xs) = Tree (mapValues TreeNodeKindType (listToVect xs))
  PTyType PUnit = ()
  PTyType PInt = Int
  PTyType (PList t) = List (PTyType t)
  PTyType PNat = Nat
  PTyType PDouble = Double
  PTyType (PTensor dim t) = Tensor dim (PTyType t)
  PTyType (PTuple t1 t2) = (PTyType t1, PTyType t2)
  PTyType (PForall pt) = Pexp [] (PForall pt)
  PTyType PPDF = PDF

getVar : (vars : List (String, PTy)) -> (p : KElem name vars) -> Record' (mapValues f vars) -> f (klookup vars p)
getVar [] KHere _ impossible
getVar [] (KThere y) _ impossible
getVar ((name, v) :: xs) KHere (x :: y) = value x
getVar ((y, w) :: xs) (KThere z) (x :: v) = getVar xs z v

toExp : (a : PTy) -> PTyType a -> Pexp vars a
toExp PString x = StringLit x
toExp PBool x = BoolLit x
toExp PUnit x = PUnitLit
toExp PInt x = ?toExp_rhs_3
toExp (PList y) x = ?toExp_rhs_4
toExp PNat x = ?toExp_rhs_5
toExp PDouble x = ?toExp_rhs_6
toExp (PTensor ks y) x = ?toExp_rhs_7
toExp (PTuple y z) x = ?toExp_rhs_8
toExp (PFun y z) x = ?toExp_rhs_9
toExp (PRecord xs) x = ?toExp_rhs_10
toExp (PTree xs) x = ?toExp_rhs_11
toExp (PForall y) x = ?toExp_rhs_12
toExp PPDF x = ?toExp_rhs_13

inlineVars : (vars : List (String, PTy)) -> Record' (mapValues PTyType vars) -> Pexp vars a -> Pexp [] a
inlineVars [] x y = 
  y
inlineVars ((z, w) :: xs) ((MkEntry z x) :: v) y = 
  let y_ = Let z (toExp w x) y
  in inlineVars xs v y_

export
eval : {vars : List (String, PTy)} -> Record' (mapValues PTyType  vars) -> Pexp vars a -> PTyType a 
eval varValues (Var {p} _) = 
  getVar vars p varValues
eval varValues (Lambda arg x) = 
  inlineVars vars varValues  $ Lambda arg x
eval varValues (Let n v x) = ?arsd
eval varValues (App x y) = ?eval_rhs_2
eval varValues (StringLit str) = ?eval_rhs_3
eval varValues (BoolLit x) = ?eval_rhs_4
eval varValues (TreeLit p x) = ?eval_rhs_5
eval varValues PUnitLit = ?eval_rhs_6
eval varValues (NatLit str) = ?eval_rhs_7
eval varValues (Prim x) = ?eval_rhs_8


JsonSerializable PPTy where
  toJson x = ?arstrstd
  fromJson x = ?synyun

mutual
  export
  JsonSerializable TreeNodeKind where
    toJson NamedSubTrees = JString "NamedSubTrees"
    toJson (Leaf x) = JArray [JString "Leaf", toJson x]
    toJson OneChild = JString "OneChild"
    toJson (ValueAndOneChild x) = JArray [JString "ValueAndOneChild", toJson x] 

    
    fromJson (JString "NamedSubTrees") = Just NamedSubTrees 
    fromJson (JArray [JString "Leaf", x]) = Leaf <$> fromJson x
    fromJson (JString "OneChild") = Just OneChild
    fromJson (JArray [JString "ValueAndOneChild", x]) = ValueAndOneChild <$> fromJson x
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
    toJson (PList x) = JArray [JString "PList", toJson x]
    toJson PNat = JString "PNat"
    toJson PDouble = JString "PDouble"
    toJson (PTensor x y) = JArray [JString "PTensor", toJson x, toJson y]
    toJson (PTuple x y) = JArray [JString "PTuple", toJson x, toJson y]
    toJson (PForall t) = JArray [JString "PForall", toJson t]
    toJson PPDF = JString "PPDF"


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
    fromJson (JArray [JString "PList", x]) = 
      Just $ PList !(fromJson x)
    fromJson (JString "PNat") = 
      Just PString
    fromJson (JString "PDouble") = 
      Just PBool
    fromJson (JArray [JString "PTensor", x, y]) = 
      Just $ PTensor !(fromJson x) !(fromJson y)
    fromJson (JArray [JString "PTuple", x, y]) = 
      Just $ PTuple !(fromJson x) !(fromJson y)
    fromJson (JArray [JString "PForall", x]) = 
      Just $ PForall !(fromJson x)
    fromJson (JString "PPDF") = 
      Just PPDF
    fromJson _ = Nothing 

{a : PTy} -> JsonSerializable (PrimFn a) where
  toJson StringEq = 
    JString "StringEq"
  toJson ListNil = 
    JString "ListNil"

  fromJson (JString "StringEq") =
    case decEq a (PString .> PString .> PBool) of
         Yes prf => Just $ rewrite prf in StringEq
         No _ => Nothing
  fromJson (JString "ListNil") =
    case decEq a (PForall PPList) of
         Yes prf => Just $ rewrite prf in ListNil
         No _ => Nothing
  fromJson _ = 
    Nothing

export
JTuple PTy where
  tupleToJson x = [toJson x]
  
  tupleFromJson [x] = fromJson x
  tupleFromJson _ = Nothing

elemToVar : (ctxt : List (String, PTy)) ->  Elem (n,a) ctxt -> (p : KElem n ctxt ** a = klookup ctxt p)
elemToVar ((n, a) :: xs) Here = (KHere ** Refl)
elemToVar ((y, z) :: xs) (There x) = let (g ** h) = elemToVar xs x in (KThere g ** h)

toJson_ : {a : PTy} -> Pexp ctxt a -> JSON
toJson_ (Var name) = 
  JArray [JString "Var", JString name]
toJson_ (Lambda arg x) = 
  JArray [JString "Lambda", JString arg, toJson_ x]
toJson_ (Let n v x) = 
  JArray [JString "Let", JString n, toJson_ v, toJson_ x]
toJson_ (App {a} x y) = 
  JArray [JString "App", toJson a, toJson_ x, toJson_ y]
toJson_ (StringLit str) = 
  JArray [JString "StringLit", JString str]
toJson_ (BoolLit x) = 
  JArray [JString "BoolLit", JBoolean x]
toJson_ (TreeLit x y) = 
  JArray [JString "TreeLit", toJson $ kElemToNat x, toJson_ y]
toJson_ PUnitLit = 
  JString "PUnitLit"
toJson_ (NatLit x) = 
  JArray [JString "NatLit", toJson x]
toJson_ (Prim x) = 
  JArray [JString "Prim", toJson x]

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
fromJson_ ctxt (PTree ts) (JArray [JString "TreeLit", x, y]) =
  do
    n <- fromJson {a = Nat} x 
    (k ** p) <- indexKElem n ts
    v <- fromJson_ ctxt (TreeNodeKindPTy (klookup ts p) (PTree ts)) y
    pure $ TreeLit p v
fromJson_ ctxt PUnit (JString "PUnitLit") =
  Just $ PUnitLit
fromJson_ ctxt PNat (JArray [JString "NatLit", n]) =
  NatLit <$> fromJson n
fromJson_ ctxt a (JArray [JString "Prim", x]) =
  Prim <$> fromJson x
fromJson_ _ _ _ = 
  Nothing

export
{ctxt : List (String, PTy)} -> {a : PTy} -> JsonSerializable (Pexp ctxt a) where
  toJson = toJson
  fromJson = fromJson_ ctxt a

export
{ctxt : List (String, PTy)} -> {a : PTy} -> JTuple (Pexp ctxt a) where
  tupleToJson x = [toJson x]
  
  tupleFromJson [x] = fromJson x
  tupleFromJson _ = Nothing

mutual
  recPTypeTypeToJson : (xs : Vect n (String, PTy)) -> Record ((mapValues (\t => t -> JSON)) (mapValues PTyType xs))
  recPTypeTypeToJson [] = []
  recPTypeTypeToJson ((x, y) :: xs) = (MkEntry x (pTyTypeToJson y)) :: recPTypeTypeToJson xs

  export
  pTyTypeToJson : (t : PTy) -> (PTyType t) -> JSON
  pTyTypeToJson PString = toJson 
  pTyTypeToJson PBool = toJson
  pTyTypeToJson PUnit = toJson
  pTyTypeToJson PInt = toJson
  pTyTypeToJson (PFun x y) = toJson
  pTyTypeToJson (PRecord xs) = \y => JObject $ recordToJson (recPTypeTypeToJson $ listToVect xs) y
  pTyTypeToJson (PTree xs) = ?tTyTypeToJson_rhs_6
  pTyTypeToJson (PList x) = \w => JArray $ map (pTyTypeToJson x) w
  pTyTypeToJson PNat = ?harst
  pTyTypeToJson PDouble = ?hrstd
  pTyTypeToJson (PTensor _ _) = ?yuarlst
  pTyTypeToJson (PTuple _ _) = ?ysrdtulr
  pTyTypeToJson (PForall _) = ?ysrdtulrarst
  pTyTypeToJson PPDF = toJson

mutual
  recPTypeTypeFromJson : (xs : Vect n (String, PTy)) -> Record ((mapValues (\t => JSON -> Maybe t)) (mapValues PTyType xs))
  recPTypeTypeFromJson [] = []
  recPTypeTypeFromJson ((x, y) :: xs) = (MkEntry x (pTyTypeFromJson y)) :: recPTypeTypeFromJson xs

  export
  pTyTypeFromJson : (t : PTy) -> JSON -> (Maybe (PTyType t))
  pTyTypeFromJson PString = fromJson
  pTyTypeFromJson PBool = fromJson
  pTyTypeFromJson PUnit = fromJson
  pTyTypeFromJson PInt = fromJson
  pTyTypeFromJson (PFun x y) = fromJson
  pTyTypeFromJson (PRecord xs) = \y => case y of
                                            JObject ys => recordFromJson (recPTypeTypeFromJson $ listToVect xs) ys
                                            _ => Nothing
  pTyTypeFromJson (PTree (xs)) = ?tTyTypeFromJson_rhs_6
  pTyTypeFromJson (PList _) = ?arst
  pTyTypeFromJson PNat = ?stdyul
  pTyTypeFromJson PDouble = ?stid
  pTyTypeFromJson (PTensor _ _) = ?irtsuack
  pTyTypeFromJson (PTuple _ _) = ?cxnwu
  pTyTypeFromJson (PForall _) = ?cxnwuarst
  pTyTypeFromJson PPDF = fromJson

