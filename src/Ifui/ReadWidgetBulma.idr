module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
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

data AltOptions : Vect n (String, Type) -> Type where
  MkAltOptions : {0 ts : Vect n (String, Type)} -> Vect n String -> Vect n (Reader (Alt ts)) -> AltOptions ts

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
(ReadWidgetBulma (Entry s t), ReadWidgetBulma (Record ts)) => ReadWidgetBulma (Record ((s,t) :: ts)) where
  getReaderBulma x = 
    MkReader (w (getReaderBulma ((\(z::zs) => z) <$> x)) (getReaderBulma ((\(z::zs) => zs) <$> x)))  x
    where 
      w : Reader (Entry s t) -> Reader (Record ts) -> Widget (Reader (Record ((s, t) :: ts)))
      w t r = do
        res <- (Left <$> getWidget t) <+> (Right <$> getWidget r)
        case res of
             (Left y) => pure $ MkReader (w y r) [|getValue y :: getValue r |]
             (Right y) => pure $ MkReader (w t y) [|getValue t :: getValue y |]



getAltIdx : {0 ts : Vect n (String, Type)} ->  Alt ts -> Fin n
getAltIdx (MkAlt x y) = elemToFin y

export
interface AltReader (0 ts : Vect n (String, Type)) where
  getAltOptionReader : Alt ts -> Reader (Alt ts)
  getOptions : AltOptions ts

export
AltReader [] where
  getAltOptionReader (MkAlt _ Here) impossible
  getAltOptionReader (MkAlt _ (There later)) impossible
  getOptions = MkAltOptions [] []

export
{s : String} -> (AltReader ts, ReadWidgetBulma t) => AltReader ((s, t) :: ts) where
  getAltOptionReader (MkAlt x Here) = (\w => MkAlt (MkEntry s w) Here) <$> getReaderBulma (Just $ value x)
  getAltOptionReader (MkAlt x (There later)) = weakenAlt <$> (the (Reader (Alt ts)) $ getAltOptionReader (MkAlt x later) )

  getOptions =
    let MkAltOptions options readers = the (AltOptions ts) getOptions
    in  MkAltOptions (s :: options) 
                     (((\w => MkAlt (MkEntry s w) Here)  <$> (the (Reader t) $ getReaderBulma Nothing)) :: ((map weakenAlt) <$> readers))

export
{0 n : Nat} -> {0 ts : Vect (S n) (String,Type)} -> AltReader ts => ReadWidgetBulma (Alt (ts)) where
  getReaderBulma x = 
    MkReader (w (fromMaybe FZ $ getAltIdx <$> x) (getAltOptionReader <$> x)) x
    where
      w : Fin (S n) -> Maybe (Reader (Alt ts)) -> Widget (Reader (Alt ts))
      w opt altreader = 
        do
          let MkAltOptions options readers = the (AltOptions ts) getOptions
          let or = selectBulma options opt
          let ar = getWidget $ fromMaybe (index opt readers) altreader 
          res <- (Left <$> or) <+> (Right <$> ar)
          case res of 
               Left y => pure $ MkReader (w y Nothing) Nothing
               Right y => pure $ MkReader (w opt (Just y)) (getValue y)
        
export
{s : String } -> ReadWidgetBulma (Alt ts) => ReadWidgetBulma (Entry s (Alt ts)) where
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
