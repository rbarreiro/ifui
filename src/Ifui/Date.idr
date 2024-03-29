module Ifui.Date

export
data Date = MkDate String

export
Show Date where
  show (MkDate x) = x

export
Eq Date where
  (==) (MkDate x) (MkDate y) = x == y

export
Ord Date where
  compare (MkDate x) (MkDate y) = compare x y

export
readISODate : String -> Maybe Date
readISODate x = Just $ MkDate x


%foreign "javascript:lambda: () => (new Date()).toISOString()"
prim__currentDate : () -> PrimIO String
export
currentDate : IO Date
currentDate = MkDate <$> (primIO $ prim__currentDate ())

%foreign "javascript:lambda: () => Date.now()"
prim__millisSinceEpoch : () -> PrimIO Int
export
millisSinceEpoch : IO Int
millisSinceEpoch = primIO $ prim__millisSinceEpoch ()
