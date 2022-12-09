module Api

import IfuiServer
import IfuiServer.RethinkDB

public export
DBTy : UKeyList (String, String) (List (String, Type))
DBTy = [("todoApp", "todoItem"), [("desc", String)]]

public export
Schema : ServerSchema DBTy
Schema = [MkTableSchema "todoApp" "todoItem"  [("desc", String)]]

public export
ApiServices : List (String, ServiceKind)
ApiServices = [ ("todoList", RPC () (List String))
              , ("createTodo", RPC String ())
              ]

export
todoApi : RethinkServer DBTy -> Server ApiServices
todoApi r =
  let todoItem = GetTable "todoApp" "todoItem"
  in [MkRPC "todoList" (\() => map (get "desc") <$> (run' r (ReadTable todoItem) >>= toArray'))
     , MkRPC "createTodo" (\x => do _ <- run' r (Insert todoItem [["desc" ^= x]]); pure ())
     ]
--   let todoApp = getDB "todoApp" mongo
--       todoItem = getCollection "todoItem" todoApp 
--       todoListFind : Promise (List (Record [("desc", String)]))
--       todoListFind = toArray todoItem
--   in [ MkRPC "todoList" (\() => map (get "desc") <$> todoListFind)
--      , MkRPC "createTodo" (\x => do _ <- insert todoItem ["desc" ^= x]; pure ())
--      ]
--   where

