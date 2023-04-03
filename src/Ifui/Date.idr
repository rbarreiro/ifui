module Ifui.Date

export
data Date = MkDate String

export
Show Date where
  show (MkDate x) = x

export
readISODate : String -> Maybe Date
readISODate x = Just $ MkDate x


%foreign "javascript:lambda: () => (new Date()).toISOString()"
prim__currentDate : () -> PrimIO String
export
currentDate : IO Date
currentDate = MkDate <$> (primIO $ prim__currentDate ())
