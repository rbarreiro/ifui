module Ifui.ReadWidgetBulma

import Ifui
import Data.Maybe
import Ifui.ExtensibleRecords
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
        s_ <- textInputBulma (s)
        pure $ MkReader (w s_)  (Just s_)

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
ReadWidgetBulma (Record []) where
  getReaderBulma x = MkReader neutral x

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

interface AltReader (0 ts : Vect n (String, Type)) where
--  getEmptyAltOptionReader : Fin n -> Reader (Alt {n = n}  ts)
  getAltOptionReader : Alt ts -> Reader (Alt ts)
  getOptions : AltOptions ts

AltReader [] where
  getAltOptionReader (MkAlt _ Here) impossible
  getAltOptionReader (MkAlt _ (There later)) impossible
  getOptions = MkAltOptions [] []

{s : String} -> (AltReader ts, ReadWidgetBulma t) => AltReader ((s, t) :: ts) where
  getAltOptionReader (MkAlt x Here) = ?h2_2 <$> getReaderBulma (Just $ value x)
  getAltOptionReader (MkAlt x (There later)) = ?h2_3 (the (Reader (Alt ts)) $ getAltOptionReader ?hhhhh )

  getOptions = ?h3
--    let MkAltOptions options = the (AltOptions ts) getOptions
--    in MkAltOptions (s :: options)

export
{0 n : Nat} -> {0 ts : Vect n (String,Type)} -> AltReader ts => ReadWidgetBulma (Alt (ts)) where
  getReaderBulma x = 
    MkReader (w (getAltIdx <$> x) (getAltOptionReader <$> x)) x
    where
      w : Maybe (Fin n) -> Maybe (Reader (Alt ts)) -> Widget (Reader (Alt ts))
      w opt altreader = 
        do
          let MkAltOptions options readers = the (AltOptions ts) getOptions
          let or = selectBulma options opt
          let ar = fromMaybe neutral $ getWidget <$> altreader 
          res <- (Left <$> or) <+> (Right <$> ar)
          case res of 
               Left y => pure $ MkReader (w (Just y) (Just $ index y readers)) Nothing
               Right y => pure $ MkReader (w opt (Just y)) (getValue y)
        
