module Ifui.ReadWidget

import Ifui
import Data.Maybe
import Ifui.ExtensibleRecords

public export
Reader : Type -> Type
Reader a = (Maybe a -> Widget a)

public export
interface ReadWidget a  where
  getReader : Reader a

export
ReadWidget String where
  getReader x = textInput [value $ fromMaybe "" x, onChange] 

export
ReadWidget Double where
  getReader x = cast <$> getReader (the (Maybe String) $ cast <$> x)

export
{s : String} -> {t : Type} -> ReadWidget t => ReadWidget (Entry s t) where
  getReader x = 
    let rt = the (Reader t) getReader
        ti = case x of
                  Nothing => Nothing
                  (Just (MkEntry _ z)) => Just z
    in span [] [text s, (\w => MkEntry s w) <$> rt ti]

export
ReadWidget (Record []) where
  getReader x = neutral


export
(ReadWidget (Entry s t), ReadWidget (Record ts)) => ReadWidget (Record ((s,t) :: ts)) where
  getReader Nothing = [|(getReader $ Nothing) :: (getReader $ Nothing)|]
  getReader (Just (x :: y)) = [|(getReader $ Just  x) :: (getReader $ Just y)|] 
