module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
import Data.List
import Data.Vect
import public Ifui.ExtensibleTypes
import public Ifui.Bulma
import Ifui.PureExpressions
import Decidable.Equality

export
data Reader a = MkReader (Bool -> Widget (Reader a)) (Maybe a)

export
getValue : Reader a -> Maybe a
getValue (MkReader x y) = y

export
getWidget : Reader a -> Bool -> Widget (Reader a)
getWidget (MkReader x y) = x

export
Functor Reader where
  map f (MkReader x y) = MkReader (\check => (\z => f <$> z) <$> x check) (f <$> y)

export
Applicative Reader where
  pure x = MkReader (const neutral) (Just x)

  (<*>) x y = 
    let w : Reader (a -> b) -> Reader a -> Bool -> Widget (Reader b)
        w r s filled =
          do
            res <- (Left <$> getWidget r filled) <+> (Right <$> getWidget s filled)
            case res of
                 Left k =>
                    pure $ MkReader (\f => w k s f) (getValue k <*> getValue s)
                 Right k =>
                    pure $ MkReader (\f => w r k f) (getValue r <*> getValue k)
    in MkReader 
        (w x y)
        (getValue x <*> getValue y)

bindW : (a -> Reader b) -> Reader a -> Maybe (Reader b) -> Bool -> Widget (Reader b)
bindW f xx Nothing filled = 
  do
    res <- getWidget xx filled
    case getValue res of
         Nothing =>
           assert_total $ pure $ MkReader (bindW f res Nothing)  Nothing
         Just v =>
           let fr = f v
           in assert_total $ pure $ MkReader (bindW f res (Just fr)) (getValue fr)
bindW f xx (Just ff) filled = 
  do
    res <- (Left <$> getWidget xx filled) <+> (Right <$> getWidget ff filled)
    case res of
         Left z =>
            case getValue z of
                 Nothing =>
                   assert_total $ pure $ MkReader (bindW f z Nothing)  Nothing
                 Just v =>
                   let fr = f v
                   in assert_total $ pure $ MkReader (bindW f z (Just fr)) (getValue fr)
         Right z =>
            assert_total $ pure $ MkReader (bindW f xx (Just z)) (getValue z)

export
Monad Reader where
  x >>= f = 
    MkReader (bindW f x (f <$> getValue x)) (join $ (getValue . f) <$> getValue x)

export
readerBind' : Reader a -> (a -> Reader b) -> Maybe (Reader b) -> Reader b
readerBind' x f y = 
  MkReader (bindW f x y) (join $ getValue <$> y)

export
transformReader : (Widget (Reader a) -> Widget (Reader a)) -> Reader a -> Reader a
transformReader f (MkReader x y) = 
  MkReader (\check => f $ transformReader f <$> x check) y

public export
interface ReadWidgetBulma a where
  getReaderBulma : Maybe a -> Reader a

public export
interface ReadWidgetBulma1 (0 f : Type -> Type) where
  getReaderBulma1 : (Maybe a -> Reader a) -> Maybe (f a) -> Reader (f a)

export
ReadWidgetBulma () where
  getReaderBulma x = 
    MkReader (const neutral) (Just ())

warning : String -> Widget a
warning s = fasIconText {styleOptions = [hasTextWarning]}  "exclamation-triangle" s

export
ReadWidgetBulma String where
  getReaderBulma x = 
    MkReader (w (isJust x) $ fromMaybe "" x) x
    where
      w : Bool -> String -> Bool -> Widget (Reader String)
      w filled s check = do
        let warn = if not filled && check 
                         then warning "Missing"
                         else neutral
        s_ <- div [textInputBulma s, warn]
        pure $ MkReader (w True s_)  (Just s_)

export
ReadWidgetBulma Double where
  getReaderBulma x = 
    MkReader (w x) x
    where
      w : Maybe Double -> Bool -> Widget (Reader Double)
      w s check = do
        s_ <- numberInputBulma s
        pure $ MkReader (w s_) s_

export
ReadWidgetBulma Nat where
  getReaderBulma x = 
    MkReader (w x) x
    where
      w : Maybe Nat -> Bool -> Widget (Reader Nat)
      w s check = do
        s_ <- (\w => cast <$> w) <$> numberInputBulma (cast <$> s)
        pure $ MkReader (w s_) s_

export
ReadWidgetBulma Bool where
  getReaderBulma x = 
    MkReader (w x) x
    where
      f2b : Fin 2 -> Bool
      f2b 0 = False
      f2b 1 = True

      b2f : Bool -> Fin 2
      b2f False = 0
      b2f True = 1

      w : Maybe Bool -> Bool -> Widget (Reader Bool)
      w s check = do
        k <- selectBulma ["False", "True"] (b2f <$> s)
        let s_ = Just $ f2b k
        pure $ MkReader (w s_) s_

export
{s : String} -> ReadWidgetBulma (Entry s String) where
  getReaderBulma x = 
    MkReader (w (isJust x) $ fromMaybe "" (value <$>x)) x
    where
      w : Bool -> String -> Bool -> Widget (Reader (Entry s String))
      w filled z check = do
        let warn = if not filled && check 
                         then warning "Missing"
                         else neutral
        z_ <- div [textInputBulma {label = Just s} z, warn]
        pure $ MkReader (w True z_) (Just $  MkEntry s z_)
    
export
{s : String} -> ReadWidgetBulma (Entry s Double) where
  getReaderBulma x = 
    MkReader (w $ value <$>x) x
    where
      w : Maybe Double -> Bool -> Widget (Reader (Entry s Double))
      w z check = do
        z_ <- numberInputBulma {label = Just s} z
        pure $ MkReader (w z_) (MkEntry s <$> z_)

export
{s : String } -> ReadWidgetBulma (Entry s Bool) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]

public export
RecordEntryGetReader : Vect n (String, Type) -> Vect n (String, Type)
RecordEntryGetReader zs = mapValuesWithKey (\s,t => Maybe (Entry s t) -> Reader (Entry s t)) zs

export
recordGetReaderBulma : {zs : Vect n (String, Type)} -> 
                         Record (RecordEntryGetReader zs) -> 
                           Maybe (Record zs) -> Reader (Record zs)
recordGetReaderBulma {zs = []} x y = 
  pure []
recordGetReaderBulma {zs = ((z, w) :: xs)} ((MkEntry z x) :: v) Nothing = 
  (::) <$> x Nothing <*> recordGetReaderBulma {zs = xs} v Nothing
recordGetReaderBulma {zs = ((z, w) :: xs)} ((MkEntry z x) :: v) (Just (y :: s)) = 
  (::) <$> x (Just y) <*> recordGetReaderBulma {zs = xs} v (Just s)

getReaderPartsRecord :(zs : Vect n (String, Type)) -> Record (mapValuesWithKey (\s, t => ReadWidgetBulma (Entry s t))  zs) -> 
                         Record (RecordEntryGetReader zs)
getReaderPartsRecord [] x = []
getReaderPartsRecord ((y, z) :: xs) ((MkEntry y x) :: w) = MkEntry y getReaderBulma :: getReaderPartsRecord xs w

export
{zs : Vect n (String, Type)} -> 
       (i : Record  (mapValuesWithKey (\s, t => ReadWidgetBulma (Entry s t))  zs)) => 
                ReadWidgetBulma (Record zs) where 
  getReaderBulma x = recordGetReaderBulma {zs = zs} (getReaderPartsRecord zs i) x


public export
TreeHeadsGetReader : Vect n (String, Type -> Type) -> Type -> Vect n (String, Type)
TreeHeadsGetReader zs a = (mapValues (\f => ((Maybe a -> Reader a) -> Maybe (f a) -> Reader (f a))) zs)

getReaderTreeHead : (zs : Vect n (String, Type -> Type)) -> (k : Fin n) -> 
                   index2 k (TreeHeadsGetReader zs a) -> Maybe ((index2 k zs) a) -> (Maybe a -> Reader a)  -> Reader ((index2 k zs) a)
getReaderTreeHead [] FZ _ _ _ impossible
getReaderTreeHead [] (FS z) _ _ _ impossible
getReaderTreeHead ((z, w) :: xs) FZ x y f = x f y
getReaderTreeHead ((z, w) :: xs) (FS v) x y f = getReaderTreeHead xs v x y f

export
treeGetReaderBulma : {zs : Vect n (String, Type -> Type)} -> 
                      Record (TreeHeadsGetReader zs (Tree zs)) -> Maybe (Tree zs) -> Reader (Tree zs)
treeGetReaderBulma x y =
  let w : Maybe (Fin n, Reader (Tree zs)) -> Bool -> Widget (Reader (Tree zs))
      w Nothing check = 
         do
           let warn = if check then warning "Missing"
                               else neutral
           k <- div [selectBulma (map fst zs) Nothing, warn]
           let r = valueIndex k x
           let ar = MkTree k <$> getReaderTreeHead zs k r Nothing (treeGetReaderBulma {zs = zs} x)
           pure $ MkReader (w $ Just (k, ar)) (getValue ar)
      w (Just (opt, reader)) check = 
         do
           let or = selectBulma (map fst zs) (Just opt)
           res <- (Left <$> or) <+> (Right <$> getWidget reader check)
           case res of 
                Left k => 
                   let r = valueIndex k x
                       ar = MkTree k <$> getReaderTreeHead zs k r Nothing (treeGetReaderBulma {zs = zs} x)
                   in pure $ MkReader (w $ Just (k, ar)) (getValue ar)
                Right y => pure $ MkReader (w $ Just (opt, y)) (getValue y)

      start : Maybe (Fin n, Reader (Tree zs))
      start = case y of
                   Nothing => 
                      Nothing
                   (Just (MkTree k z)) => 
                      let r = valueIndex k x
                      in Just (k, MkTree k <$> getReaderTreeHead zs k r (Just z) (treeGetReaderBulma {zs = zs} x))
      
  in MkReader (\check => w start check) y

getReaderPartsTree : (zs : Vect n (String, Type -> Type)) -> Record (mapValues (\f => ReadWidgetBulma1 f)  zs) -> 
                         Record (TreeHeadsGetReader zs a)
getReaderPartsTree [] x = []
getReaderPartsTree ((y, z) :: xs) ((MkEntry y x) :: w) =  (MkEntry y getReaderBulma1) :: getReaderPartsTree xs w

export
{zs : Vect n (String, Type -> Type)} ->
       (i : Record  (mapValues (\f => ReadWidgetBulma1 f)  zs)) => 
                ReadWidgetBulma (Tree zs) where 
  getReaderBulma x = treeGetReaderBulma {zs = zs} (getReaderPartsTree zs i) x

export
ReadWidgetBulma b => ReadWidgetBulma1 (const b) where
  getReaderBulma1 cont x = getReaderBulma x

export
ReadWidgetBulma b => ReadWidgetBulma1 (\x => (b, x)) where
  getReaderBulma1 cont Nothing = (,) <$> getReaderBulma Nothing  <*> cont Nothing
  getReaderBulma1 cont (Just (x, y)) = (,) <$> getReaderBulma (Just x) <*> cont (Just y)


fromAllJust : List (Maybe a) -> Maybe (List a)
fromAllJust [] = Just []
fromAllJust (Nothing :: xs) = Nothing
fromAllJust ((Just x) :: xs) = [| (Just x) :: fromAllJust xs |]

mapIndexed : (Nat -> a -> b) -> List a -> List b
mapIndexed f xs =
  case length xs of
       0 => []
       S k => zipWith f [0..k] xs

data ListReaderEvent a = AddValue
                       | ChangeValueList Nat (Reader a)
export
ReadWidgetBulma1 List where
  getReaderBulma1 cont x =
    let add : Widget (ListReaderEvent a)
        add = fasIconText {onclick = (Just AddValue)} "plus" "Add"

        renderItem : Bool -> Nat -> Reader a -> Widget (ListReaderEvent a)
        renderItem check i r =
          ChangeValueList i <$> getWidget r check

        w : List (Reader a) -> Bool -> Widget (Reader (List a))
        w itemsReaders check = 
          do
            res <- div ((mapIndexed (renderItem check) itemsReaders) ++ [add]) 
            let newReaders = the (List (Reader a)) $  case res of 
                               AddValue =>
                                  itemsReaders ++ [cont Nothing]
                               ChangeValueList i valnew => 
                                  take i itemsReaders ++ [valnew] ++ drop (i+1) itemsReaders
            pure $ MkReader (w newReaders) (fromAllJust $ map  getValue newReaders)
        startReaders : List  (Reader a)
        startReaders = case x of
                  Nothing => []
                  Just x => map (\v => cont (Just v)) x
    in MkReader (w startReaders) x

export
ReadWidgetBulma a => ReadWidgetBulma (List a) where
  getReaderBulma x = getReaderBulma1 getReaderBulma x

data KeyListReaderEvent a = ChangeKey String String
                          | AddEmptyKey
                          | ChangeValueKeyList String (Reader a)
export
ReadWidgetBulma1 (\w => List (String, w)) where
  getReaderBulma1 cont x =
    let add : Widget (KeyListReaderEvent a)
        add = fasIconText {onclick = (Just AddEmptyKey)} "plus" "Add"

        renderItem : Bool -> (String, Reader a) -> Widget (KeyListReaderEvent a)
        renderItem check (s, r) =
          div [ChangeKey s <$> textInputBulma {label = Just "Entry Key"} s ,ChangeValueKeyList s <$> getWidget r check]

        w : List (String, Reader a) -> Bool -> Widget (Reader (List (String, a)))
        w itemsReaders check = 
          do
            res <- div ((map (renderItem check) itemsReaders) ++ [add]) 
            let newReaders = the (List (String, Reader a)) $  case res of 
                               ChangeKey sold snew => 
                                  map (\(k,v) => if k == sold then (snew, v) else (k,v)) itemsReaders
                               AddEmptyKey =>
                                  itemsReaders ++ [("", cont Nothing)]
                               ChangeValueKeyList s valnew => 
                                  map (\(k,v) => if k == s then (k, valnew) else (k,v)) itemsReaders
            pure $ MkReader (w newReaders) (fromAllJust $ map (\(k,v) => (k,)  <$> getValue v) newReaders)
        startReaders : List (String, Reader a)
        startReaders = case x of
                  Nothing => []
                  Just x => map (\(k,v) => (k, cont (Just v))) x
    in MkReader (w startReaders) x

export
{s : String } -> ReadWidgetBulma (Record ts) => ReadWidgetBulma (Entry s (Record ts)) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]

export
{s : String } -> ReadWidgetBulma (Tree ts) => ReadWidgetBulma (Entry s (Tree ts)) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]


optionsReader : {default False compact : Bool} -> 
                   (a -> (Fin n, Reader a)) -> Vect n (String, () -> Reader a) -> Maybe a -> Reader a
optionsReader init options s0 =
  let w : Maybe (Fin n, Reader a) -> Bool -> Widget (Reader a)
      w Nothing check = 
         do
           let warn = if check then warning "Missing"
                               else neutral
           k <- div [selectBulma (map fst options) Nothing, warn]
           let (o, ar_) = index k options
           let ar = ar_ ()
           pure $ MkReader (w $ Just (k, ar)) (getValue ar)
      w (Just (opt, reader)) check = 
         do
           let or = selectBulma (map fst options) (Just opt)
           res <- (Left <$> or) <+> (Right <$> getWidget reader check)
           case res of 
                Left k => 
                   let (o, ar_) = index k options
                       ar = ar_ ()
                   in pure $ MkReader (w $ Just (k, ar)) (getValue ar)
                Right y => 
                   pure $ MkReader (w $ Just (opt, y)) (getValue y)

      start : Maybe (Fin n, Reader a)
      start = case s0 of
                   Nothing => 
                      Nothing
                   (Just i) => 
                      Just $ init i
      
  in MkReader (\check => w start check) s0

selectReader : Vect n String -> Maybe (Fin n ) -> Reader (Fin n)
selectReader xs x =
    MkReader (w x) x
    where
      w : Maybe (Fin n) -> Bool -> Widget (Reader (Fin n))
      w s check = do
        let warn = if isNothing s && check 
                         then warning "Missing"
                         else neutral
        s_ <- div [selectBulma xs s, warn]
        pure $ MkReader (w $ Just s_)  (Just s_)

stringValuePairsReaderCompact : (Maybe a -> Reader a)  -> Maybe (List (String, a)) -> Reader (List (String, a))
stringValuePairsReaderCompact cont x =
  let add : Widget (KeyListReaderEvent a)
      add = fasIcon {onclick = (Just AddEmptyKey)} "plus"

      renderItem : Bool -> (String, Reader a) -> Widget (KeyListReaderEvent a)
      renderItem check (s, r) =
        div [ChangeKey s <$> textInputBulma {label = Just "Entry Key"} s ,ChangeValueKeyList s <$> getWidget r check]

      w : List (String, Reader a) -> Bool -> Widget (Reader (List (String, a)))
      w itemsReaders check = 
        do
          res <- div ((map (renderItem check) itemsReaders) ++ [add]) 
          let newReaders = the (List (String, Reader a)) $  case res of 
                             ChangeKey sold snew => 
                                map (\(k,v) => if k == sold then (snew, v) else (k,v)) itemsReaders
                             AddEmptyKey =>
                                itemsReaders ++ [("", cont Nothing)]
                             ChangeValueKeyList s valnew => 
                                map (\(k,v) => if k == s then (k, valnew) else (k,v)) itemsReaders
          pure $ MkReader (w newReaders) (fromAllJust $ map (\(k,v) => (k,)  <$> getValue v) newReaders)
      startReaders : List (String, Reader a)
      startReaders = case x of
                Nothing => []
                Just x => map (\(k,v) => (k, cont (Just v))) x
  in MkReader (w startReaders) x

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
        ] 
        x
      where
        aux : PTy -> (Fin 13, Reader PTy)
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
  readerFromExp_ : (ctxt : List (String, PTy)) -> (t : PTy) -> (te : PTy) -> String -> (Pexp ctxt te) -> Maybe (() -> Reader (Pexp ctxt t))
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

  expReaders : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (String, () ->  Reader (Pexp ctxt t)))
  expReaders ctxt t =
    let readers = varAndPrimReaders ctxt t ++ litReaders ctxt t
    in (length readers ** fromList readers)

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
  pTyTypeReader (PRecord xs) = recordGetReaderBulma (recReaders $ Vect.fromList xs)
  pTyTypeReader (PTree xs) = ?pTyTypeReader_rhs_6
  pTyTypeReader (PList _) = ?sytdul
  pTyTypeReader PNat = ?stydustnd
  pTyTypeReader PDouble = ?yulrst
  pTyTypeReader (PTensor _ _) = ?uyaw
  pTyTypeReader (PTuple _ _) = ?yrsutyruj
  pTyTypeReader (PForall _) = ?yrsutyrujarst


export
readerForm : {default Nothing startVal : Maybe a} -> (Maybe a -> Reader a)  -> Widget (Maybe a)
readerForm reader = 
  loopState 
    (False, reader startVal)
    (\(check, x) => do
             res <- formBulma (\z => getWidget z check) x
             case res of
                  Nothing => 
                     pure $ Right Nothing
                  Just r =>
                     case getValue r of
                          Nothing => pure $ Left (True ,r)
                          Just v =>  pure $ Right $ Just v
    )

export
getFormBulma : ReadWidgetBulma a => {default Nothing startVal : Maybe a} -> Widget (Maybe a)
getFormBulma = 
  readerForm getReaderBulma 

test : Widget (Maybe (Tree [("Record", \w => List (String, w)), ("String", const ()), ("HowMany", \w => (String, w))]))
test = getFormBulma

test2 : Widget (Maybe (Record [("id", String),("spec", PTy)]))
test2 = getFormBulma


