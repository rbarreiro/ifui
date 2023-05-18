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

data KeyListReaderEvent a = ChangeKey String String
                          | AddEmptyKey
                          | ChangeValue String (Reader a)


fromAllJust : List (Maybe a) -> Maybe (List a)
fromAllJust [] = Just []
fromAllJust (Nothing :: xs) = Nothing
fromAllJust ((Just x) :: xs) = [| (Just x) :: fromAllJust xs |]

export
ReadWidgetBulma1 (\w => List (String, w)) where
  getReaderBulma1 cont x =
    let add : Widget (KeyListReaderEvent a)
        add = fasIconText {onclick = (Just AddEmptyKey)} "plus" "Add"

        renderItem : Bool -> (String, Reader a) -> Widget (KeyListReaderEvent a)
        renderItem check (s, r) =
          div [ChangeKey s <$> textInputBulma {label = Just "Entry Key"} s ,ChangeValue s <$> getWidget r check]

        w : List (String, Reader a) -> Bool -> Widget (Reader (List (String, a)))
        w itemsReaders check = 
          do
            res <- div ((map (renderItem check) itemsReaders) ++ [add]) 
            let newReaders = the (List (String, Reader a)) $  case res of 
                               ChangeKey sold snew => 
                                  map (\(k,v) => if k == sold then (snew, v) else (k,v)) itemsReaders
                               AddEmptyKey =>
                                  itemsReaders ++ [("", cont Nothing)]
                               ChangeValue s valnew => 
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

optionsReader : {default False compact : Bool} -> (a -> (Fin n, Reader a)) -> Vect n (String, () -> Reader a) -> Maybe a -> Reader a
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


stringValuePairsReaderCompact : (Maybe a -> Reader a)  -> Maybe (List (String, a)) -> Reader (List (String, a))
stringValuePairsReaderCompact cont x =
  let add : Widget (KeyListReaderEvent a)
      add = fasIcon {onclick = (Just AddEmptyKey)} "plus"

      renderItem : Bool -> (String, Reader a) -> Widget (KeyListReaderEvent a)
      renderItem check (s, r) =
        div [ChangeKey s <$> textInputBulma {label = Just "Entry Key"} s ,ChangeValue s <$> getWidget r check]

      w : List (String, Reader a) -> Bool -> Widget (Reader (List (String, a)))
      w itemsReaders check = 
        do
          res <- div ((map (renderItem check) itemsReaders) ++ [add]) 
          let newReaders = the (List (String, Reader a)) $  case res of 
                             ChangeKey sold snew => 
                                map (\(k,v) => if k == sold then (snew, v) else (k,v)) itemsReaders
                             AddEmptyKey =>
                                itemsReaders ++ [("", cont Nothing)]
                             ChangeValue s valnew => 
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
        ] 
        x
      where
        aux : PTy -> (Fin 7, Reader PTy)
        aux PString = (0, pure PString)
        aux PBool = (1, pure PBool)
        aux PUnit = (2, pure PUnit)
        aux PInt = (3, pure PInt)
        aux (PFun y z) = (4, PFun <$> getReaderBulma (Just y)  <*> getReaderBulma (Just z))
        aux (PRecord xs) = (5, PRecord <$> stringValuePairsReaderCompact (getReaderBulma {a = PTy}) (Just xs))
        aux (PTree xs) = (6, PTree <$> stringValuePairsReaderCompact (getReaderBulma {a = TreeNodeKind}) (Just xs))


varExprs_ : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (k : String ** p : KElem k ctxt ** t = klookup ctxt p))
varExprs_ [] t = (0 ** [])
varExprs_ ((z, w) :: xs) t = 
  let (n ** ys) = varExprs_ xs t
      ys_ = [ (k ** KThere p ** pEq) | (k ** p ** pEq) <- ys]
  in case decEq t w of
          No _ => (n ** ys_)
          Yes prf => (S n ** (z ** KHere ** prf) :: ys_)

varExprs : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (String, Pexp ctxt t))
varExprs ctxt t = 
  let (n ** xs) = varExprs_ ctxt t
  in (n ** [(rewrite pEq in (k, Var k {p = p})) | (k ** p **  pEq) <- xs] )

varAndPrimExprs : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (String, Pexp ctxt t))
varAndPrimExprs ctxt t = 
  varExprs ctxt t

getReaderByName : String -> Vect n (String, Reader (Pexp ctxt t)) -> Maybe (Fin n, Reader (Pexp ctxt t))
getReaderByName str xs = 
  case findIndex ((str==) . fst) xs of
     Nothing => Nothing  
     Just i => Just (i, snd $ index i xs)

mutual

  varAndPrimReaders : (ctxt : List (String, PTy)) -> (t : PTy) -> (n : Nat ** Vect n (String, Reader (Pexp ctxt t)))

  pexpW : Vect n (String, Reader (Pexp ctxt t)) ->
                  Maybe (Fin n, Reader (Pexp ctxt t)) -> 
                    Bool -> Widget (Reader (Pexp ctxt t))
  pexpW options x check = 
    case x of
         Nothing => do
           let warn = if check then warning "Missing"
                               else neutral
           k <- div [selectBulma (map fst options) Nothing, warn]
           let (o, ar) = index k options
           pure $ MkReader (pexpW options $ Just (k, ar)) (getValue ar)
         Just (opt, reader) =>
           do
             let or = selectBulma (map fst options) (Just opt)
             res <- (Left <$> or) <+> (Right <$> getWidget reader check)
             case res of 
                  Left k => 
                     let (o, ar) = index k options
                     in pure $ MkReader (pexpW options $ Just (k, ar)) (getValue ar)
                  Right y => 
                     pure $ MkReader (pexpW options $ Just (opt, y)) (getValue y)

  getReaderBulma_Pexp : (ctxt : List (String, PTy)) -> (t : PTy) -> Maybe (Pexp ctxt t) -> Reader (Pexp ctxt t)
  getReaderBulma_Pexp ctxt t Nothing = 
    let (n ** zs) = varAndPrimReaders ctxt t
    in MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt (klookup ctxt p) (Just (Var name)) =
    let (n ** zs) = varAndPrimReaders ctxt (klookup ctxt p)
    in case getReaderByName name zs of
            Just (i, r) => MkReader (pexpW zs (Just (i, r))) (getValue r)
            Nothing => MkReader (pexpW zs Nothing) Nothing
  getReaderBulma_Pexp ctxt (PFun a b) (Just (Lambda arg x)) = 
    let (n ** zs) = varAndPrimReaders ctxt (PFun a b)
    in ?getReaderBulma__rhs_3
  getReaderBulma_Pexp ctxt t (Just (App x y)) = ?getReaderBulma__rhs_4
  getReaderBulma_Pexp ctxt PString (Just (StringLit str)) = ?getReaderBulma__rhs_5
  getReaderBulma_Pexp ctxt PBool (Just (BoolLit x)) = ?getReaderBulma__rhs_6
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

test : Widget (Maybe (Tree [("Record", \w => List (String, w)), ("String", const ())]))
test = getFormBulma

test2 : Widget (Maybe (Record [("id", String),("spec", PTy)]))
test2 = getFormBulma
