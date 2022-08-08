module Ifui.ReadWidget

import Ifui
import Data.Maybe
import Ifui.ExtensibleRecords

Reader : Type -> Type
Reader a = (Maybe a -> Widget (Maybe a))

interface ReadWidget a  where
  getReader : Reader a

ReadWidget String where
  getReader x = input [value $ fromMaybe "" x, Just <$> onChange] 

{s : String} -> {t : Type} -> ReadWidget t => ReadWidget (Entry s t) where
  getReader x = 
    let rt = the (Reader t) getReader
        ti = case x of
                  Nothing => Nothing
                  (Just (MkEntry _ z)) => Just z
    in span [] [text s, (\w => MkEntry s <$> w) <$> rt ti]

--ReadWidget (Record []) where
--  getReader x = empty


--(HasForm t, HasForm (Record ts)) => HasForm (Ifui.ExtensibleRecords.Record ((s,t) :: ts)) where
--  getForm x = 
--    span [] [
--      ?h
--    ]
