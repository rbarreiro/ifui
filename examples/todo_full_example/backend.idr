module Main

import IfuiServer
import IfuiServer.MongoDB
import Api

main : IO ()
main =
  do
    _ <- serveStatic 6401 "www"
    _ <- (createMongoClient Schema "mongodb://todo_mongodb:27017").run $ 
            \mongo => startWsServer 6402 (todoApi mongo)
    pure ()

