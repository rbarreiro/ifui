module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
import Data.Vect
import public Ifui.ExtensibleRecords
import Ifui.Bulma

export
data Reader a = MkReader (Widget (Reader a)) (Maybe a)

export
getValue : Reader a -> Maybe a
getValue (MkReader x y) = y

export
getWidget : Reader a -> Widget (Reader a)
getWidget (MkReader x y) = x

Functor Reader where
  map f (MkReader x y) = MkReader ((\z => f <$> z) <$> x) (f <$> y)

export
transformReader : (Widget (Reader a) -> Widget (Reader a)) -> Reader a -> Reader a
transformReader f (MkReader x y) = 
  MkReader (f $ transformReader f <$> x ) y

data VariantOptions : FieldList -> Type where
  MkVariantOptions : {0 ts : FieldList} -> Vect (UKeyList.length ts) String -> Vect (UKeyList.length ts) (Reader (Variant ts)) -> VariantOptions ts

public export
interface ReadWidgetBulma a where
  getReaderBulma : Maybe a -> Reader a

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

export
ReadWidgetBulma (Record []) where
  getReaderBulma x = MkReader neutral (Just Nil)

export
{p : UKeyListCanPrepend (s, t) ts} -> (ReadWidgetBulma (Entry s t), ReadWidgetBulma (Record ts)) => ReadWidgetBulma (Record ((s,t) :: ts)) where
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
{s : String} -> {p : UKeyListCanPrepend (s, t) ts} -> (VariantReader ts, ReadWidgetBulma t) => VariantReader ((s, t) :: ts) where
  getVariantOptionReader (MkVariant s x Here) = (\w => MkVariant s w Here) <$> getReaderBulma (Just x)
  getVariantOptionReader (MkVariant s x (There later)) = weakenVariant <$> (the (Reader (Variant ts)) $ getVariantOptionReader (MkVariant s x later) )

  getOptions =
    let MkVariantOptions options readers = the (VariantOptions ts) getOptions
    in  MkVariantOptions (s :: options) 
                     (((\w => MkVariant s w Here)  <$> (the (Reader t) $ getReaderBulma Nothing)) :: ((map weakenVariant) <$> readers))

export
{0 ts : FieldList} -> {p : UKeyListCanPrepend (s, t) ts} -> VariantReader ((s, t) ::ts) => ReadWidgetBulma (Variant ((s,t) :: ts)) where
  getReaderBulma x = 
    MkReader (w (fromMaybe FZ $ getVariantIdx <$> x) (getVariantOptionReader <$> x)) x
    where
      w : Fin (S (UKeyList.length ts)) -> Maybe (Reader (Variant ((s, t) ::ts))) -> Widget (Reader (Variant ((s, t) ::ts)))
      w opt altreader = 
        do
          let MkVariantOptions options readers = the (VariantOptions ((s, t) ::ts)) getOptions
          let or = selectBulma options opt
          let ar = getWidget $ fromMaybe (index opt readers) altreader 
          res <- (Left <$> or) <+> (Right <$> ar)
          case res of 
               Left y => pure $ MkReader (w y Nothing) Nothing
               Right y => pure $ MkReader (w opt (Just y)) (getValue y)
        
export
{s : String } -> ReadWidgetBulma (Variant ts) => ReadWidgetBulma (Entry s (Variant ts)) where
  getReaderBulma x = 
    MkEntry s <$> (transformReader f $ getReaderBulma (value <$> x))
    where
      f : Widget a -> Widget a
      f x = fieldsSection s [x]

export
getFormBulma : ReadWidgetBulma a => {default Nothing startVal : Maybe a} -> Widget (Maybe a)
getFormBulma = 
  getRes <$> formBulma getWidget (the (Reader a) $ getReaderBulma startVal)
  where
    getRes : Maybe (Reader a) -> Maybe a
    getRes Nothing = Nothing
    getRes (Just x) = getValue x
