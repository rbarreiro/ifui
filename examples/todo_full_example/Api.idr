module Api

import IfuiServer
import IfuiServer.MongoDB

public export
DBTy : List (String, String, List (String, Type))
DBTy = [("todoApp", "todoItem", [("desc", String)])]

public export
Schema : ClientSchema DBTy
Schema = [MkCollectionSchema "todoApp" "todoItem"  [("desc", String)]]

public export
ApiServices : List (String, ServiceKind)
ApiServices = [ ("todoList", RPC () (List String))
              , ("createTodo", RPC String ())
              ]

export
todoApi : MongoClient DBTy -> Server ApiServices
todoApi mongo  =
  let todoApp = getDB "todoApp" mongo
      todoItem = getCollection "todoItem" todoApp 
      todoListFind : Promise (List (Record [("desc", String)]))
      todoListFind =  find todoItem (\w => TrueLit)
  in [ MkRPC "todoList" (\() => map (get "desc") <$> todoListFind)
     , MkRPC "createTodo" (\x => do _ <- insertOne todoItem ["desc" ^= x]; pure ())
     ]
  where

