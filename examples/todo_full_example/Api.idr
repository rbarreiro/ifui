module Api

import IfuiServer
import IfuiServer.RethinkDB
import public Schema


public export
DBTy : ServerSpec
DBTy = ServerSchemaSpec Schema

public export
ApiServices : ServiceKind
ApiServices = GroupService [ ("todoList", StreamService () (Change String))
                           , ("createTodo", RPC String ())
                           ]

logedInTodoApi : RethinkServer DBTy -> Service ApiServices
logedInTodoApi r =
  let todoItem = GetTable "todoApp" "todoItem"
      descsQuery : Query DBTy [] (Cursor String)
      descsQuery = ReadTable todoItem |> Map <| GetField "desc"
      descList : IOStream (Change String)
      descList= getChanges' r $ GetChanges True descsQuery
      in MkGroupService  [ "todoList" ^= MkStreamService (\() => descList )
                         , "createTodo" ^= MkRPC (\x => do _ <- run' r (Insert todoItem (Lit [["desc" ^= x]])); pure ())
                         ]

public export
RoleServer : Bool -> ServiceKind
RoleServer False = EmptyService
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
            False => MkEmptyService
            True => logedInTodoApi r
    )
    
