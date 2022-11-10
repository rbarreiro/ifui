module Api

import IfuiServer

public export
ApiServices : List (String, ServiceKind)
ApiServices = [("getText", RPC () String)]

export
todoApi : Server ApiServices
todoApi =
  [ serviceRPC "getText" (\() => pure "ola")
  ]

