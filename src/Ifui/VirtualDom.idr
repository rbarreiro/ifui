module Ifui.VirtualDom

import Ifui.Dom

public export
data VAttribute = StringAttr String

public export
data VNode = VNodeText String | VNodeNode String (List VAttribute) (List VNode)

data RunningNode = MkRunningNode

export
createEmptyRunningNode : String -> IO RunningNode
