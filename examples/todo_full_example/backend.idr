module Main

import IfuiServer
import Api

main : IO ()
main =
  do
    serveStatic 6011 "www"
    startWsServer 6012 todoApi

