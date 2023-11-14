module Ifui.ReadWidgetBulmaPureExp

import public Ifui.ReadWidgetBulmaCore
import public Ifui.PureExpressions
import Data.List

mutual
  export 
  ReadWidgetBulma TreeNodeKind where
    getReaderBulma x = 
      optionsReader {compact = True} 
        aux 
        [ ("NamedSubTrees", \() => pure NamedSubTrees)
        , ("Leaf", \() => Leaf <$> getReaderBulma {a = PTy} Nothing)
        , ("OneChild", \() => pure OneChild)
        , ("ValueAndOneChild", \() => ValueAndOneChild <$> getReaderBulma {a = PTy} Nothing)
        ]
        x
      where
        aux : TreeNodeKind -> (Fin 4, Reader TreeNodeKind)
        aux NamedSubTrees = (0, pure NamedSubTrees)
        aux (Leaf x) = (1, Leaf <$> getReaderBulma (Just x))
        aux OneChild = (2, pure OneChild)
        aux (ValueAndOneChild x) = (3, ValueAndOneChild <$> getReaderBulma (Just x))

  export 
  ReadWidgetBulma PTy where
    getReaderBulma x = 
      optionsReader {compact = True}
        aux 
        [ ("String", \() => pure PString)
        , ("Bool", \() => pure PBool)
        , ("Unit", \() => pure PUnit)
        , ("Int", \() => pure PInt)
        , ("Fun", \() => PFun <$> getReaderBulma Nothing  <*> getReaderBulma Nothing)
        , ("Record", \() => PRecord <$> stringValuePairsReaderCompact (getReaderBulma {a = PTy}) Nothing)
        , ("Tree", \() => PTree <$> stringValuePairsReaderCompact (getReaderBulma {a = TreeNodeKind}) Nothing)
        , ("List", \() => PList <$> getReaderBulma Nothing)
        , ("Nat", \() => pure PNat)
        , ("Double", \() => pure PDouble)
        , ("Tensor", \() => PTensor <$> getReaderBulma Nothing  <*> getReaderBulma Nothing)
        , ("Tuple", \() => PTuple <$> getReaderBulma Nothing  <*> getReaderBulma Nothing)
        , ("Forall", \() => ?arsyustd)
        , ("PDF", \() => pure PPDF)
        ] 
        x
      where
        aux : PTy -> (Fin 14, Reader PTy)
        aux PString = (0, pure PString)
        aux PBool = (1, pure PBool)
        aux PUnit = (2, pure PUnit)
        aux PInt = (3, pure PInt)
        aux (PFun y z) = (4, PFun <$> getReaderBulma (Just y)  <*> getReaderBulma (Just z))
        aux (PRecord xs) = (5, PRecord <$> stringValuePairsReaderCompact (getReaderBulma {a = PTy}) (Just xs))
        aux (PTree xs) = (6, PTree <$> stringValuePairsReaderCompact (getReaderBulma {a = TreeNodeKind}) (Just xs))
        aux (PList x) = (7, PList <$> getReaderBulma (Just x))
        aux PNat = (8, pure PNat)
        aux PDouble = (9, pure PDouble)
        aux (PTensor y z) = (10, PTensor <$> getReaderBulma (Just y)  <*> getReaderBulma (Just z))
        aux (PTuple y z) = (11, PTuple <$> getReaderBulma (Just y)  <*> getReaderBulma (Just z))
        aux (PForall x) = ?arstuyl
        aux PPDF = (13, pure PPDF)


varExprs_ : (ctxt : List (String, PTy)) -> List (t : PTy ** (k : String ** p : KElem k ctxt ** t = klookup ctxt p))
varExprs_ [] = []
varExprs_ ((z, w) :: xs) = 
  let ys = varExprs_ xs
      ys_ = [ (t ** k ** KThere p ** pEq) | ( t ** k ** p ** pEq) <- ys]
  in (w ** z ** KHere ** Refl) :: ys_ 

varExprs : (ctxt : List (String, PTy)) -> List (String, (t : PTy ** Pexp ctxt t))
varExprs ctxt = 
  let xs = varExprs_ ctxt
  in [ (k, (t ** (rewrite pEq in Var k {p=p}))) | ( t ** k ** p ** pEq) <- xs]

primExprs : List (String, (t : PTy ** Pexp ctxt t))
primExprs = 
  [
    ("StringEq", ((PString .> PString .> PBool) ** Prim StringEq))
  ]

baseExprs : (ctxt : List (String, PTy)) -> List (String, (t : PTy ** Pexp ctxt t))
baseExprs ctxt = varExprs ctxt ++ primExprs

getReaderByName : String -> Vect n (String, () -> Reader (Pexp ctxt t)) -> Maybe (Fin n, () -> Reader (Pexp ctxt t))
getReaderByName str xs = 
  case findIndex ((str==) . fst) xs of
     Nothing => Nothing  
     Just i => Just (i, snd $ index i xs)

isSomeReturnType : PTy -> PTy -> Bool
isSomeReturnType x y = 
  case decEq x y of
       Yes _ => True
       No _ => case y of
                    PFun z w => isSomeReturnType x w
                    _ => False

mutual
  readerFromExp_ : (ctxt : List (String, PTy)) -> (t : PTy) -> (te : PTy) -> String -> 
                      (Pexp ctxt te) -> Maybe (() -> Reader (Pexp ctxt t))
  readerFromExp_ ctxt t te n z = 
    case decEq t te of
         No _ => 
            case te of
                 PFun x y => 
                     do
                       let argReader = getReaderBulma_Pexp ctxt x Nothing
                       fnReader <- readerFromExp_ ctxt (x .> t) te n z
                       pure $ const $ App <$> fnReader () <*> argReader
                 _ => 
                     Nothing
         Yes prf => 
            Just $ \() => transformReader (\x => text n) (pure $ rewrite prf in z)


  readerFromExp : (ctxt : List (String, PTy)) -> (t : PTy) -> (te : PTy) -> 
                      String -> (Pexp ctxt te) -> Maybe (() -> Reader (Pexp ctxt t))
  readerFromExp ctxt t te n x = 
    if isSomeReturnType t te then readerFromExp_ ctxt t te n x
                             else Nothing

  varAndPrimReaders : (ctxt : List (String, PTy)) -> (t : PTy) -> List (String, () ->  Reader (Pexp ctxt t))
  varAndPrimReaders ctxt t =
    let base = baseExprs ctxt
    in catMaybes (map (\(n,(te ** e)) => (n,) <$> readerFromExp ctxt t te n e)  base)


  TreeConsArgs : (ctxt : List (String, PTy)) -> (ts : List (String, TreeNodeKind)) -> Type
  TreeConsArgs ctxt ts = (k : String ** p : KElem k ts ** Pexp ctxt (TreeNodeKindPTy (klookup ts p) (PTree ts)))

  treeLitReader : (ctxt : List (String, PTy)) -> (ts : List (String, TreeNodeKind)) -> 
                      Maybe (TreeConsArgs ctxt ts) -> Reader (TreeConsArgs ctxt ts)
  treeLitReader ctxt ts =
    optionsReader
      aux
      ((\(k ** p) => (k, \() => (\x => (k ** p ** x)) <$> getReaderBulma_Pexp ctxt (TreeNodeKindPTy (klookup ts p) (PTree ts)) Nothing)) <$> kElemVect ts)
    where
      aux : TreeConsArgs ctxt ts -> (Fin (length ts), Reader (TreeConsArgs ctxt ts))
      aux (k ** p ** v) = (kElemToFin p, (\x => (k ** p ** x)) <$> getReaderBulma_Pexp ctxt (TreeNodeKindPTy (klookup ts p) (PTree ts)) (Just v))

  litReaders : (ctxt : List (String, PTy)) -> (t : PTy) -> List (String, () -> Reader (Pexp ctxt t))
  litReaders ctxt PString = ?litReaders_rhs_0
  litReaders ctxt PBool = ?litReaders_rhs_1
  litReaders ctxt PUnit =
    [("Literal", \() => MkReader (const $ text "()") (Just PUnitLit))]
  litReaders ctxt PInt = ?litReaders_rhs_3
  litReaders ctxt (PList x) = ?litReaders_rhs_4
  litReaders ctxt PNat = 
    [("Literal", \() => NatLit <$> getReaderBulma Nothing)]
  litReaders ctxt PDouble = ?litReaders_rhs_6
  litReaders ctxt (PTensor ks x) = ?litReaders_rhs_7
  litReaders ctxt (PTuple x y) = ?litReaders_rhs_8
  litReaders ctxt (PFun x y) = ?litReaders_rhs_9
  litReaders ctxt (PRecord xs) = ?litReaders_rhs_10
  litReaders ctxt (PTree xs) = 
    [("Literal", \() => (\(k ** p ** v) => TreeLit p v)  <$> treeLitReader ctxt xs Nothing)]
  litReaders ctxt (PForall x) = ?litReaders_rhs_12
  litReaders ctxt PPDF = []

  expReaders : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (String, () ->  Reader (Pexp ctxt t)))
  expReaders ctxt t =
    let readers = varAndPrimReaders ctxt t ++ litReaders ctxt t
    in (length readers ** listToVect readers)

  pexpW : Vect n (String, () -> Reader (Pexp ctxt t)) ->
                  Maybe (Fin n, Reader (Pexp ctxt t)) -> 
                    Bool -> Widget (Reader (Pexp ctxt t))
  pexpW options x check = 
    case x of
         Nothing => do
           let warn = if check then warning "Missing"
                               else neutral
           k <- div [selectBulma (map fst options) Nothing, warn]
           let (o, ar) = index k options
           let ar_ = ar ()
           pure $ MkReader (pexpW options $ Just (k, ar_)) (getValue ar_)
         Just (opt, reader) =>
           do
             let or = selectBulma (map fst options) (Just opt)
             res <- (Left <$> or) <+> (Right <$> getWidget reader check)
             case res of 
                  Left k => 
                     let (o, ar) = index k options
                         ar_ = ar ()
                     in pure $ MkReader (pexpW options $ Just (k, ar_)) (getValue ar_)
                  Right y => 
                     pure $ MkReader (pexpW options $ Just (opt, y)) (getValue y)

  getReaderBulma_Pexp : (ctxt : List (String, PTy)) -> (t : PTy) -> Maybe (Pexp ctxt t) -> Reader (Pexp ctxt t)
  getReaderBulma_Pexp ctxt PUnit _ =
    MkReader (const $ text "()") (Just PUnitLit)
  getReaderBulma_Pexp ctxt t Nothing = 
    let (n ** zs) = expReaders ctxt t
    in MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt (klookup ctxt p) (Just (Var name)) =
    let (n ** zs) = expReaders ctxt (klookup ctxt p)
    in case getReaderByName name zs of
            Just (i, r) => let r_ = r () in MkReader (pexpW zs (Just (i, r_))) (getValue r_)
            Nothing => MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt (PFun a b) (Just (Lambda arg x)) = 
    let (n ** zs) = expReaders ctxt (PFun a b)
    in ?getReaderBulma__rhs_3
  getReaderBulma_Pexp ctxt t (Just (Let n v x)) = ?getReaderBulma__rhsrstrst
  getReaderBulma_Pexp ctxt t (Just (App x y)) = ?getReaderBulma__rhs_4
  getReaderBulma_Pexp ctxt PString (Just (StringLit str)) = ?getReaderBulma__rhs_5
  getReaderBulma_Pexp ctxt PBool (Just (BoolLit x)) = ?getReaderBulma__rhs_6
  getReaderBulma_Pexp ctxt (PTree ts) (Just (TreeLit {k} x y)) = 
    let (n ** zs) = expReaders ctxt (PTree ts)
    in case getReaderByName "Literal" zs of
            Just (i, _) =>  MkReader (pexpW zs (Just (i, (\(_ ** p ** v) => TreeLit p v) <$> treeLitReader ctxt ts (Just  (k ** x ** y))))) (Just (TreeLit x y))
            Nothing => MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt PNat (Just (NatLit x)) = 
    let (n ** zs) = expReaders ctxt PNat
    in case getReaderByName "Literal" zs of
            Just (i, _) => MkReader (pexpW zs (Just (i, NatLit <$> getReaderBulma (Just x)))) (Just $ NatLit x)
            Nothing => MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt t (Just (Prim x)) = ?getReaderBulma__rhs_7

export
{ctxt : List (String, PTy)} -> {a : PTy} -> ReadWidgetBulma (Pexp ctxt a) where
  getReaderBulma = getReaderBulma_Pexp ctxt a
    
export
{s : String } -> ReadWidgetBulma (Entry s PTy) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]

export
{ctxt : List (String, PTy)} -> {a : PTy} -> {s : String } -> ReadWidgetBulma (Entry s (Pexp ctxt a)) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget b -> Widget b
      f x = fieldsSection s [x]


mutual
  recReaders : (xs : Vect n (String, PTy)) -> Record (RecordEntryGetReader (mapValues PTyType xs))
  recReaders [] = []
  recReaders ((x, y) :: xs) =
    let f = transformReader (\w => fieldsSection x [w])
        
        z : Maybe (Entry x (PTyType y)) -> Reader (Entry x (PTyType y))
        z Nothing = MkEntry x <$> (f $ pTyTypeReader y Nothing)
        z (Just v) = MkEntry x <$> (f $ pTyTypeReader y (Just $ value v))
    in (MkEntry x z) :: recReaders xs

  export
  pTyTypeReader : (t : PTy) -> Maybe (PTyType t) -> Reader (PTyType t)
  pTyTypeReader PString = getReaderBulma 
  pTyTypeReader PBool = getReaderBulma
  pTyTypeReader PUnit = getReaderBulma
  pTyTypeReader PInt = ?pTyTypeReader_rhs_3
  pTyTypeReader (PFun x y) = getReaderBulma
  pTyTypeReader (PRecord xs) = recordGetReaderBulma (recReaders $ listToVect xs)
  pTyTypeReader (PTree xs) = ?pTyTypeReader_rhs_6
  pTyTypeReader (PList _) = ?sytdul
  pTyTypeReader PNat = ?stydustnd
  pTyTypeReader PDouble = ?yulrst
  pTyTypeReader (PTensor _ _) = ?uyaw
  pTyTypeReader (PTuple x y) = \w => case w of
                                          Nothing => (,) <$> pTyTypeReader x Nothing <*> pTyTypeReader y Nothing
                                          Just (a, b) => (,) <$> pTyTypeReader x (Just a) <*> pTyTypeReader y (Just b)
  pTyTypeReader (PForall _) = ?yrsutyrujarst
  pTyTypeReader PPDF = ?yrsutyrujarstarst


