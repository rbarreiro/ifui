module IfuiServer.RethinkDB

import public Ifui.ExtensibleRecords
import Data.List
import Data.Maybe
import public Data.List.Elem
import public IfuiServer.Promise
import public Ifui.JSValue

public export
data HasVar : String -> Type -> List (String, Type) -> Type where
  Here : HasVar s t ((s, t) :: xs)
  There : HasVar s t xs -> {auto p : Not (s = s_)}  -> HasVar s t ((s_, t_) :: xs)

data Table : UKeyList String Type -> Type where

data Cursor : Type -> Type where
  MkCursor : AnyPtr -> Cursor a

public export
data Expr : UKeyList (String, String) (UKeyList String Type) -> List (String, Type) -> Type -> Type where
    Var : (name : String) -> HasVar name t ctxt  -> Expr db ctxt t
    Lambda : (arg : String) -> (a : Type)  -> Expr db ((arg, a) :: ctxt) b ->  Expr db ctxt (a -> b)
    App : Expr db ctxt (a -> b) ->  Expr db ctxt a ->  Expr db ctxt b
    GetTable : (d : String) -> (t : String) -> {auto 0 p : Elem (d, t) ts db} -> Expr db ctxt (Table ts)
    ReadTable : Expr db ctxt (Table ts) -> Expr db ctxt (Cursor (Record ts))
    Insert :  Expr db ctxt (Table ts) -> Expr db ctxt (List (Record ts)) -> Expr db ctxt (Record [])
    Lit : HasJSValue a => a -> Expr db ctxt a
    StrEq : Expr db ctxt String -> Expr db ctxt String -> Expr db ctxt Bool
    GetField : (key : String) ->  {auto 0 p : Elem key t fields}  -> Expr db ctxt (Record fields) -> Expr db ctxt t

public export
data StorableType = SString
                  | SRecord (UKeyList String StorableType)

public export
InterpST : StorableType -> Type
InterpST SString = String
InterpST (SRecord ts) = Record (mapValues InterpST ts)

public export
data TableSchema : String -> String -> UKeyList String Type -> Type where
  MkTableSchema : (d : String) -> (c : String) -> (ts : UKeyList String StorableType) -> TableSchema d c (mapValues InterpST ts)

public export
data ServerSchema : UKeyList (String, String) (UKeyList String Type) -> Type where
  Nil : ServerSchema [] 
  (::) : TableSchema db tbl ts -> ServerSchema rs -> {auto p : UKeyListCanPrepend ((db, tbl), ts) rs} -> ServerSchema ( (::) ((db, tbl), ts) rs {p=p})
  
export
data RethinkServer : UKeyList (String, String) (UKeyList String Type) -> Type where
  MkRethinkServer : AnyPtr -> RethinkServer a

%foreign "node:lambda: (cursor, callback)  => cursor.toArray((err, res) => callback(err ? err + '' : '')(res)())"
prim__toArray : AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()

export
toArray : HasJSValue a => Cursor a -> Promise (Either String (List a))
toArray (MkCursor x) = 
  MkPromise $ \w => do
    primIO $ prim__toArray x (\err, result => toPrim $ if err == "" then case the (Maybe (List a)) (fromPtr result) of
                                                                            Nothing =>  w $ Left "Find: error reading values \{ptrToString result}"
                                                                            (Just x) => w $ Right x
                                                                    else w $ Left err
                             )
    pure $ MkPromiseHandler (pure ())

export
toArray' : HasJSValue a => Cursor a -> Promise (List a)
toArray' x = onErrPrint $ toArray x

%foreign "node:lambda: err  => err ? err + '' : ''"
prim__errToStr : AnyPtr -> String

%foreign "node:lambda: () => require('rethinkdb')"
prim__r : PrimIO AnyPtr

%foreign "node:lambda: (q, conn, callback)  => q.run(conn, (err, res) => callback(err ? err + '' : '')(res)())"
prim__run : AnyPtr -> AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()

runPtr : AnyPtr -> AnyPtr -> Promise (Either String AnyPtr)
runPtr query conn = 
  MkPromise $ \w => do
     primIO $ prim__run query conn $ \err, result => toPrim $ if err == "" then w $ Right result
                                                                           else w $ Left err
     pure $ MkPromiseHandler $ pure ()
 
%foreign "node:lambda: (r, db, tbl) => r.expr([r.branch(r.dbList().contains(db), r.expr({}), r.dbCreate(db)), r.branch(r.db(db).tableList().contains(tbl), r.expr({}), r.db(db).tableCreate(tbl))])"
prim__table_if_not_exists : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, db, tbl) => r.db(db).table(tbl)"
prim__read_table : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, db, tbl) => r.db('ifui_meta').table('table_versions').insert([{'id': [db, tbl], 'version': 1}])"
prim__insert_table_version : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (host, port, callback)  => {const r = require('rethinkdb'); r.connect({host:host, port: port}, (err, conn) => callback(err)(conn)())}"
prim__connect : String -> Int -> (AnyPtr -> AnyPtr -> PrimIO ()) -> PrimIO ()

getDbTableNames : TableSchema d t xs -> List String
getDbTableNames (MkTableSchema d t ts) = [d, t]

migrateTable : AnyPtr -> TableSchema d t xs -> Int -> AnyPtr -> Promise (Maybe String)
migrateTable r (MkTableSchema d t ts) 0 conn =
  do
    let create = prim__table_if_not_exists r d t
    let createV = prim__insert_table_version r d t
    Right _ <- runPtr create conn | Left x => pure $ Just x
    Right _ <- runPtr createV conn | Left x => pure $ Just x
    pure Nothing
migrateTable r (MkTableSchema d t ts) 1 conn =
  pure Nothing
migrateTable r x v conn =
  pure $ Just "Unexpected version \{show v} for table \{show (getDbTableNames x)}"


getVersion : TableSchema d t xs -> List (Record [("id", List String), ("version", Int)]) -> Int
getVersion x versions = 
  fromMaybe 0 $ get {k="version"} <$> find (\w => getDbTableNames x == get {k="id"} w) versions

migrateServer : AnyPtr -> ServerSchema ts -> List (Record [("id", List String), ("version", Int)]) -> AnyPtr -> Promise (Maybe String)
migrateServer r [] versions conn = 
  pure Nothing
migrateServer r (x :: y) versions conn =
  do
    Nothing <- migrateTable r x (getVersion x versions) conn | Just e => pure $ Just e
    migrateServer r y versions conn

doMigration : ServerSchema ts -> AnyPtr -> Promise (Maybe String)
doMigration s conn = 
  do
    r <- primIO $ prim__r
    let addTableVers = prim__table_if_not_exists r "ifui_meta" "table_versions"
    Right _ <- runPtr addTableVers conn | Left x => pure $ Just x
    let readTblVers = prim__read_table r "ifui_meta" "table_versions"
    Right tableVersionsCursorPtr <- runPtr readTblVers conn | Left x => pure $ Just x
    let tableVersionsCursor = the (Cursor (Record [("id", List String), ("version", Int)])) $ MkCursor tableVersionsCursorPtr
    Right versions <- toArray tableVersionsCursor | Left x => pure $ Just x
    migrateServer r s versions conn



export
connect :  String -> Int -> ServerSchema ts -> Promise (Either String (RethinkServer ts))
connect host port x = 
  MkPromise $ \w =>
    do
      primIO $ prim__connect host port (\e, conn => toPrim $
                                                  do
                                                    let err = prim__errToStr e 
                                                    if err == "" then do 
                                                                        _ <- (doMigration x conn).run $ \z => case z of
                                                                                                                   Nothing => w $ Right $ MkRethinkServer conn
                                                                                                                   Just y => w $ Left y
                                                                        pure () --w $ Right $ MkRethinkServer conn
                                                                 else w $ Left err
                                       )
      pure $ MkPromiseHandler (pure ())

export
connect' :  String -> Int -> ServerSchema ts -> Promise (RethinkServer ts)
connect' host port x = onErrPrint $ connect host port x

public export
data VarStack : List (String, Type) -> Type where
  Empty : VarStack []
  AddVar : {0 a : Type} -> (s : String) -> AnyPtr  -> VarStack ts -> VarStack ((s, a) :: ts)

getVar : HasVar name a ctxt -> VarStack ctxt -> AnyPtr
getVar Here (AddVar name x y) = 
  x
getVar (There x) (AddVar s_ y z) = 
  getVar x z

fnToPtr : (AnyPtr -> AnyPtr) -> AnyPtr
fnToPtr x = believe_me x

%foreign "node:lambda: (r, f, x) => r.do(x, f)"
prim__app : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (r, d, t) => r.db(d).table(t)"
prim__getTable : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, x, y) => r.eq(x,y)"
prim__req : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (f, x) => f(x)"
prim__rget : AnyPtr -> String -> AnyPtr

%foreign "node:lambda: (r, x) => r.expr(x)"
prim__expr : AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (t, xs) => t.insert(xs)"
prim__insert : AnyPtr -> AnyPtr -> AnyPtr

compileExpr : AnyPtr -> VarStack ctxt ->  Expr db ctxt r -> AnyPtr 
compileExpr r vars (Var name x) = getVar x vars
compileExpr r vars (Lambda arg a x) = fnToPtr $ \w => compileExpr r (AddVar arg w vars) x
compileExpr r vars (App x y) = prim__app r (compileExpr r vars x) (compileExpr r vars y)
compileExpr r vars (GetTable d t) = prim__getTable r d t
compileExpr r vars (ReadTable x) = compileExpr r vars x
compileExpr r vars (Insert t xs) = prim__insert (compileExpr r vars t) (compileExpr r vars xs)
compileExpr r vars (Lit x) = prim__expr r $ jsv2ptr $ toJS x
compileExpr r vars (StrEq x y) = prim__req r (compileExpr r vars x) (compileExpr r vars y)
compileExpr r vars (GetField key x) = prim__rget (compileExpr r vars x) key


export
run : HasJSValue a => RethinkServer ts -> Expr ts [] a -> Promise (Either String a)
run (MkRethinkServer s) e = 
  do
    r <- primIO prim__r
    let query = compileExpr r Empty e
    z <- runPtr query s
    case z of 
         Left err => pure $ Left err
         Right ptr =>  case the (Maybe a) (fromPtr ptr) of
                            Nothing => pure $ Left "Find: error reading values \{ptrToString ptr}"
                            Just x => pure $ Right x

export
run' : HasJSValue a => RethinkServer ts -> Expr ts [] a -> Promise a
run' x y = onErrPrint $ run x y

export
HasJSValue (Cursor a) where
  toJS (MkCursor x) = MkJSValue x
  fromJS (MkJSValue x) = MkCursor x
  checkPtr x = True
