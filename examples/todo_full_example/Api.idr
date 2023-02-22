module Api

import IfuiServer
import IfuiServer.RethinkDB


public export
Schema : ServerSchema 
Schema = [MkTableSchema "todoApp" "todoItem" String  [("desc", String)]]

public export
DBTy : UKeyList (String, String) FieldList
DBTy = ServerSchemaTy Schema

public export
ApiServices : UKeyList String ServiceKind
ApiServices = [ ("todoList", StreamService () (Change String))
              , ("createTodo", RPC String ())
              ]

logedInTodoApi : RethinkServer DBTy -> Server ApiServices
logedInTodoApi r =
  let todoItem = GetTable "todoApp" "todoItem"
      descsQuery : Query DBTy [] (Cursor String)
      descsQuery = ReadTable todoItem |> MapCursor <| GetField "desc"
      descList : IOStream (Change String)
      descList= getChanges' r $ GetChanges {includeInitial=True} descsQuery
      in [MkStreamService "todoList" (\() => descList )
     , MkRPC "createTodo" (\x => do _ <- run' r (Insert todoItem (Lit [["desc" ^= x]])); pure ())
     ]

public export
RoleServer : Bool -> UKeyList String ServiceKind
RoleServer False = []
RoleServer True = ApiServices

public export
Login : Type
Login = Record [("login", String), ("password", String)]

export
todoApi : RethinkServer DBTy -> ServerWithAuth Login Bool RoleServer
todoApi r =
  MkServerWithAuth 
    (\x => pure $ (the String $ get "login" x) == get "password" x)
    (\x =>
          case x of
            False => []
            True => logedInTodoApi r
    )
    
