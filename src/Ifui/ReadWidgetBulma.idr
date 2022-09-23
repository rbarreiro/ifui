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


data AltOptions : Vect n (String, Type) -> Type where
  MkAltOptions : Vect (length z) String -> AltOptions z

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


interface AltReader (0 ts : Vect n (String, Type)) where
  getAltReader : (x : Fin n) -> Maybe (Builtin.snd (Vect.index x ts)) -> Reader (Builtin.snd (Vect.index x ts))  
  getOptions : AltOptions ts


MaybeAltOptionReaderTy : Maybe (Fin n) -> Vect n (String, Type) -> Type
MaybeAltOptionReaderTy Nothing xs = Reader ()
MaybeAltOptionReaderTy (Just x) xs = Reader (snd (index x xs))

--getReaderBulmaAlt : {0 ts: Vect n (String, Type)} -> AltReader ts =>
--                           (x : Maybe (Fin n)) -> MaybeAltOptionTy x ts -> Widget (Alt ts)
--getReaderBulmaAlt Nothing y = 
--  do
--    let MkAltOptions options = the (AltOptions ts) getOptions
--    x <- selectBulma options Nothing
--    getReaderBulmaAlt (Just (the (Fin n) (rewrite sym (lengthCorrect ts) in x))) Nothing
--getReaderBulmaAlt (Just x) y = ?getReaderBulmaAlt_rhs_2
--
--

getAltIdx : {0 ts : Vect n (String, Type)} ->  Alt ts -> Fin n
getAltIdx (MkAlt x y) = elemToFin y

altReaderStart : AltReader ts => (x : Maybe (Alt ts)) -> MaybeAltOptionReaderTy (ReadWidgetBulma.getAltIdx <$> x) ts
altReaderStart Nothing = MkReader neutral Nothing
altReaderStart (Just (MkAlt x Here)) = ?hhhh_2
altReaderStart (Just (MkAlt x (There later))) = ?hhhh_3

export
{0 n : Nat} -> {0 ts : Vect n (String,Type)} -> AltReader ts => ReadWidgetBulma (Alt (ts)) where
  getReaderBulma x = 
    MkReader (w (getAltIdx <$> x) (altReaderStart x)) x
    where
      w :(y : Maybe (Fin n)) -> MaybeAltOptionReaderTy y ts -> Widget (Reader (Alt ts))
      w Nothing zz = ?h_1 
      w (Just y) zz = ?h_2
       -- do
       -- let MkAltOptions options = the (AltOptions ts) getOptions
       -- ?h
        
