module Ifui.ReadWidget

import Ifui
import Data.Maybe
import Ifui.ExtensibleRecords

public export
Reader : Type -> Type
Reader a = (Maybe a -> Widget (Maybe a))

public export
interface ReadWidget a  where
  getReader : Reader a

export
ReadWidget String where
  getReader x = input [value $ fromMaybe "" x, Just <$> onChange] 

export
{s : String} -> {t : Type} -> ReadWidget t => ReadWidget (Entry s t) where
  getReader x = 
    let rt = the (Reader t) getReader
        ti = case x of
                  Nothing => Nothing
                  (Just (MkEntry _ z)) => Just z
    in span [] [text s, (\w => MkEntry s <$> w) <$> rt ti]

export
ReadWidget (Record []) where
  getReader x = neutral

readerPair : Reader a -> Reader b -> Reader (a, b)
readerPair f g x = ?readerPair_rhs

export
(ReadWidget (Entry s t), ReadWidget (Record ts)) => ReadWidget (Record ((s,t) :: ts)) where
  getReader Nothing = ?h_1
  getReader (Just x) = ?h_2
