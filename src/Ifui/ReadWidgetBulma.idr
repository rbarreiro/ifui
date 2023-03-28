module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
import Data.Vect
import public Ifui.ExtensibleTypes
import public Ifui.Bulma

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
recordGetReaderBulma : {zs : Vect n (String, Type)} -> 
                         Record (mapValuesWithKey (\s,t => Maybe (Entry s t) -> Reader (Entry s t)) zs) -> 
                           Maybe (Record zs) -> Reader (Record zs)
recordGetReaderBulma {zs = []} x y = 
  pure []
recordGetReaderBulma {zs = ((z, w) :: xs)} ((MkEntry z x) :: v) Nothing = 
  (::) <$> x Nothing <*> recordGetReaderBulma {zs = xs} v Nothing
recordGetReaderBulma {zs = ((z, w) :: xs)} ((MkEntry z x) :: v) (Just (y :: s)) = 
  (::) <$> x (Just y) <*> recordGetReaderBulma {zs = xs} v (Just s)

getReaderPartsRecord :(zs : Vect n (String, Type)) -> Record (mapValuesWithKey (\s, t => ReadWidgetBulma (Entry s t))  zs) -> 
                         Record (mapValuesWithKey (\s,t => Maybe (Entry s t) -> Reader (Entry s t)) zs)
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
                         Record (TreeHeadsGetReader zs (Tree zs))
getReaderPartsTree [] x = []
getReaderPartsTree ((y, z) :: xs) ((MkEntry y x) :: w) = ?hhh -- :: getReaderPartsTree xs w

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
            res <- div ( add :: (map (renderItem check) itemsReaders)) 
            let newReaders = the (List (String, Reader a)) $  case res of 
                               ChangeKey sold snew => 
                                  map (\(k,v) => if k == sold then (snew, v) else (k,v)) itemsReaders
                               AddEmptyKey =>
                                  ("", cont Nothing) :: itemsReaders
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

export
getFormBulma : ReadWidgetBulma a => {default Nothing startVal : Maybe a} -> Widget (Maybe a)
getFormBulma = 
  loopState 
    (False, getReaderBulma startVal)
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

test : Widget (Maybe (Tree [("Record", \w => List (String, w)), ("String", const ())]))
test = getFormBulma

