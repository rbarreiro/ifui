module Main

import IfuiServer
import IfuiServer.RethinkDB
import Api

main : IO ()
main =
  do
    _ <- serveStatic 6401 "www"
    _ <- (connect' "todo_rethinkdb" 28015  Schema).run $ 
            \db => startWsServerWithAuth 6402 (todoApi db)
    pure ()

