module Ifui.Changes

public export
data StringChanges = setString String

public export
interface Changes a b where
  applyChanges : a -> b -> a

export
Changes String StringChanges where
  applyChanges x (setString y) = y
