module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
import Data.Vect
import public Ifui.ExtensibleRecords
import public Ifui.Bulma

export
data Reader a = MkReader (Widget (Reader a)) (Maybe a)

export
getValue : Reader a -> Maybe a
getValue (MkReader x y) = y

export
getWidget : Reader a -> Widget (Reader a)
getWidget (MkReader x y) = x

export
Functor Reader where
  map f (MkReader x y) = MkReader ((\z => f <$> z) <$> x) (f <$> y)

export
transformReader : (Widget (Reader a) -> Widget (Reader a)) -> Reader a -> Reader a
transformReader f (MkReader x y) = 
  MkReader (f $ transformReader f <$> x ) y

public export
interface ReadWidgetBulma a where
  getReaderBulma : Maybe a -> Reader a

public export
interface ReadWidgetBulma1 (0 f : Type -> Type) where
  getReaderBulma1 : (Maybe a -> Reader a) -> Maybe (f a) -> Reader (f a)

export
ReadWidgetBulma () where
  getReaderBulma x = 
    MkReader neutral (Just ())

export
ReadWidgetBulma String where
  getReaderBulma x = 
    MkReader (w $ fromMaybe "" x) x
    where
      w : String -> Widget (Reader String)
      w s = do
        s_ <- textInputBulma s
        pure $ MkReader (w s_)  (Just s_)

export
ReadWidgetBulma Double where
  getReaderBulma x = 
    MkReader (w x) x
    where
      w : Maybe Double -> Widget (Reader Double)
      w s = do
        s_ <- numberInputBulma s
        pure $ MkReader (w s_) s_

export
{s : String} -> ReadWidgetBulma (Entry s String) where
  getReaderBulma x = 
    MkReader (w $ fromMaybe "" (value <$>x)) x
    where
      w : String -> Widget (Reader (Entry s String))
      w z = do
        z_ <- textInputBulma {label = Just s} z
        pure $ MkReader (w z_) (Just $  MkEntry s z_)
    
export
{s : String} -> ReadWidgetBulma (Entry s Double) where
  getReaderBulma x = 
    MkReader (w $ value <$>x) x
    where
      w : Maybe Double -> Widget (Reader (Entry s Double))
      w z = do
        z_ <- numberInputBulma {label = Just s} z
        pure $ MkReader (w z_) (MkEntry s <$> z_)


-- starts : (rs : UKeyList String Type) -> AllIG (\s, t => ReadWidgetBulma (Entry s t)) rs => 
--             Maybe (Record rs) -> Record (mapValuesWithKey (\i, j => Reader (Entry i j)) rs)
-- starts [] x = 
--   []
-- starts ((::) {p} (y, z) ls) Nothing = 
--   (::) {p = mapValuesWithKeyPrf y ls p} 
--        (y ^= getReaderBulma Nothing) 
--        (starts ls Nothing)
-- starts ((::) {p} (y, z) ls) (Just (x :: xs)) = 
--   (::) {p = mapValuesWithKeyPrf y ls p} 
--        (y ^= getReaderBulma (Just x)) 
--        (starts ls (Just xs))
-- 
-- 
-- 
-- renderReaders : (zs : FieldList) -> Record (mapValuesWithKey (\i, j => Reader (Entry i j)) zs) -> 
--                     Widget (Variant (mapValuesWithKey (\i, j => Reader (Entry i j)) zs))
-- renderReaders [] [] = 
--   neutral
-- renderReaders ((::) {p} (y, z) ls) (x :: w) =
--   ?h1 <+> ((\w => weakenVariant {p = ?h} w) <$> renderReaders ls w)
-- 
-- export
-- {zs : FieldList} -> AllIG (\s, t => ReadWidgetBulma (Entry s t)) zs => ReadWidgetBulma (Record zs) where
--   getReaderBulma {zs} x =
--     let w : Record (mapValuesWithKey (\i, j => Reader (Entry i j)) zs) -> Widget (Reader (Record zs))
--         w x = 
--           do
--             ?htrsdstd
-- 
--     in MkReader (w (starts zs x)) x

export
ReadWidgetBulma (Record []) where
  getReaderBulma x = MkReader neutral (Just Nil)

export
{p : CanPrependKey s ts} -> (ReadWidgetBulma (Entry s t), ReadWidgetBulma (Record ts)) => ReadWidgetBulma (Record ((s,t) :: ts)) where
  getReaderBulma x = 
    MkReader (w (getReaderBulma ((\(z::zs) => z) <$> x)) (getReaderBulma ((\(z::zs) => zs) <$> x)))  x
    where 
      w : Reader (Entry s t) -> Reader (Record ts) -> Widget (Reader (Record ((s, t) :: ts)))
      w t r = do
        res <- (Left <$> getWidget t) <+> (Right <$> getWidget r)
        case res of
             (Left y) => pure $ MkReader (w y r) ((::) {p=p} <$> getValue y <*> getValue r)
             (Right y) => pure $ MkReader (w t y) ((::) {p=p} <$> getValue t <*> getValue y)



listElemToFin : Elem x y xs -> Fin (UKeyList.length xs)
listElemToFin Here = FZ
listElemToFin (There y) = FS $ listElemToFin y

getVariantIdx : {0 ts : FieldList} ->  Variant ts -> Fin (UKeyList.length ts)
getVariantIdx (MkVariant _ _ y) = listElemToFin y

data VariantOptions : FieldList -> Type where
  MkVariantOptions : {0 ts : FieldList} -> Vect (UKeyList.length ts) String -> Vect (UKeyList.length ts) (Reader (Variant ts)) -> VariantOptions ts

export
interface VariantReader (0 ts : FieldList) where
  getVariantOptionReader : Variant ts -> Reader (Variant ts)
  getOptions : VariantOptions ts

export
VariantReader [] where
  getVariantOptionReader (MkVariant _ _ Here) impossible
  getVariantOptionReader (MkVariant _ _ (There later)) impossible
  getOptions = MkVariantOptions [] []

export
{s : String} -> {p : CanPrependKey s ts} -> (VariantReader ts, ReadWidgetBulma t) => VariantReader ((s, t) :: ts) where
  getVariantOptionReader (MkVariant s x Here) = (\w => MkVariant s w Here) <$> getReaderBulma (Just x)
  getVariantOptionReader (MkVariant s x (There later)) = weakenVariant <$> (the (Reader (Variant ts)) $ getVariantOptionReader (MkVariant s x later) )

  getOptions =
    let MkVariantOptions options readers = the (VariantOptions ts) getOptions
    in  MkVariantOptions (s :: options) 
                     (((\w => MkVariant s w Here)  <$> (the (Reader t) $ getReaderBulma Nothing)) :: ((map weakenVariant) <$> readers))

export
{0 ts : FieldList} -> VariantReader ts => ReadWidgetBulma (Variant ts) where
  getReaderBulma x = 
    let MkVariantOptions options readers = the (VariantOptions ts) getOptions
        w : Maybe (Fin (UKeyList.length ts)) -> Maybe (Reader (Variant ts)) -> Widget (Reader (Variant ts))
        w Nothing _ =
          do
            y <- selectBulma options Nothing
            pure $ MkReader (w (Just y) Nothing) Nothing
        w (Just opt) altreader = 
          do
            let or = selectBulma options (Just opt)
            let ar = getWidget $ fromMaybe (index opt readers) altreader 
            res <- (Left <$> or) <+> (Right <$> ar)
            case res of 
                 Left y => pure $ MkReader (w (Just y) Nothing) Nothing
                 Right y => pure $ MkReader (w (Just opt) (Just y)) (getValue y)
    in MkReader (w (getVariantIdx <$> x) (getVariantOptionReader <$> x)) x
        
data TreeOptions : UKeyList String (Type -> Type) -> Type where
  MkTreeOptions : {0 ts : UKeyList String (Type -> Type)} -> Vect (UKeyList.length ts) String -> Vect (UKeyList.length ts) (k : String ** KElem k ts) -> TreeOptions ts

interface TreeReader (0 ts : UKeyList String (Type -> Type)) where
  branchValueReader : (Maybe a -> Reader a) -> (pElem : KElem k ts) -> Maybe ((klookup ts pElem) a) -> Reader ((klookup ts pElem) a) 
  treeOptions : TreeOptions ts

export
TreeReader [] where
  branchValueReader _ KHere _ impossible
  branchValueReader _ (KThere y) _ impossible
  treeOptions = MkTreeOptions [] [] 

export
{s : String} -> {p : CanPrependKey s ts} -> (TreeReader ts, ReadWidgetBulma1 f) => TreeReader ((s, f) :: ts) where
  branchValueReader cont KHere x = 
    getReaderBulma1 cont x
  branchValueReader cont (KThere y) x = 
    branchValueReader cont y x

  treeOptions = 
    let MkTreeOptions o p = treeOptions {ts=ts}
    in MkTreeOptions (s :: o) ((s ** KHere) :: ((\(k ** prf) => (k ** KThere prf) ) <$> p))


export
{0 ts : UKeyList String (Type -> Type)} -> TreeReader ts => ReadWidgetBulma (Tree ts) where
  getReaderBulma x =
    let MkTreeOptions options prfs = treeOptions {ts=ts}

        getEmptyBranchReader : Fin (UKeyList.length ts) -> Reader (Tree ts)
        getEmptyBranchReader x =
          let (k **prf) = index x prfs
          in N k {p=prf} <$> branchValueReader (getReaderBulma {a = Tree ts}) prf Nothing

        w : Maybe (Fin (UKeyList.length ts)) -> (Maybe (Reader (Tree ts))) -> Widget (Reader (Tree ts))
        w Nothing _ = 
          do
            y <- selectBulma options Nothing
            pure $ MkReader (w (Just y) Nothing) Nothing
        w (Just opt) prevReader = 
          do
            let or = selectBulma options (Just opt)
            let ar = getWidget $ fromMaybe (getEmptyBranchReader opt) prevReader 
            res <- (Left <$> or) <+> (Right <$> ar)
            case res of 
                 Left y => pure $ MkReader (w (Just y) Nothing) Nothing
                 Right y => pure $ MkReader (w (Just opt) (Just y)) (getValue y)

        rootIdx : Tree ts -> Maybe (Fin (UKeyList.length ts))
        rootIdx (N s _) = findIndex (==s)  options

        startValueReader : Tree ts -> Reader (Tree ts)
        startValueReader (N s {p} x) =  N s {p=p} <$> branchValueReader (getReaderBulma {a = Tree ts}) p (Just x)
    in MkReader (w (join $ rootIdx <$> x) (startValueReader <$> x)) x

export
ReadWidgetBulma b => ReadWidgetBulma1 (const b) where
  getReaderBulma1 cont x = getReaderBulma x


data UKeyListReaderEvent a = ChangeKey String String
                           | AddEmptyKey
                           | ChangeValue String (Reader a)


makeNameNotRepeated : String -> (l : UKeyList String a) -> (k : String ** CanPrependKey k l)
makeNameNotRepeated str l = 
  case calcCanPrependKey str l of
       Nothing => makeNameNotRepeated (str ++ "_")  l
       (Just x) => (str ** x)

toUKeyList : List (String, a) -> UKeyList String a
toUKeyList [] = []
toUKeyList ((x, y) :: xs) = 
  let xs_      = toUKeyList xs
      (k ** p) = makeNameNotRepeated x xs_
  in (::) {p = p} (k, y) xs_

export
ReadWidgetBulma1 (UKeyList String) where
  getReaderBulma1 cont x =
    let add : Widget (UKeyListReaderEvent a)
        add = fasIconText {onclick = (Just AddEmptyKey)} "plus" "Add"

        renderItem : (String, Reader a) -> Widget (UKeyListReaderEvent a)
        renderItem (s, r) =
          div [ChangeKey s <$> textInputBulma {label = Just "Entry Key"} s ,ChangeValue s <$> getWidget r]

        w : UKeyList String (Reader a) -> Widget (Reader (UKeyList String a))
        w itemsReaders = 
          do
            let pairs = toListPairs itemsReaders
            res <- div ( add :: (map renderItem pairs)) 
            let newReaders = case res of 
                               ChangeKey sold snew => 
                                  toUKeyList $ map (\(k,v) => if k == sold then (snew, v) else (k,v))  pairs
                               AddEmptyKey => 
                                  let (k ** p) = makeNameNotRepeated "" itemsReaders
                                  in (::) {p = p} (k, cont Nothing) itemsReaders
                               ChangeValue s valnew => 
                                  mapValuesWithKey (\k, v => if s == k then valnew else v) itemsReaders 
            pure $ MkReader (w newReaders) (fromAllJust $ mapValues getValue newReaders )

        startReaders : UKeyList String (Reader a)
        startReaders = case x of
                  Nothing => []
                  Just x => mapValues (cont . Just) x
    in MkReader (w startReaders) x

export
{s : String } -> ReadWidgetBulma (Variant ts) => ReadWidgetBulma (Entry s (Variant ts)) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]

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

export
getFormBulma : ReadWidgetBulma a => {default Nothing startVal : Maybe a} -> Widget (Maybe a)
getFormBulma = 
  loopState 
    (getReaderBulma startVal)
    (\x => do
             res <- formBulma getWidget x
             case res of
                  Nothing => 
                     pure $ Right Nothing
                  Just r =>
                     case getValue r of
                          Nothing => pure $ Left r
                          Just v =>  pure $ Right $ Just v
    )

test : Widget (Maybe (Tree [("Record", UKeyList String), ("String", const ())]))
test = getFormBulma

