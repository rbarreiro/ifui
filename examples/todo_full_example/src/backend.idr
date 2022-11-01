module Main

import IfuiServer

main : IO ()
main =
  do
    serveStatic 6011 "www"

