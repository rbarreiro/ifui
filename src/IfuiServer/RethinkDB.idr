module IfuiServer.RethinkDB

import public Ifui.ExtensibleTypes
import Data.List
import Data.Maybe
import public Data.List.Elem
import public IfuiServer.Promise
import public IfuiServer.IOStream
import public Ifui.Services
import public Ifui.Json
import public Ifui.Date
import Data.IORef
import IfuiServer.Server
import Data.List.Elem

public export
data HasVar : String -> Type -> List (String, Type) -> Type where
  Here : HasVar s t ((s, t) :: xs)
  There : HasVar s t xs -> HasVar s t ((s_, t_) :: xs)

export
data Table : List (String, Type) -> Type where

export
data Cursor : Type -> Type where

export
data Changes : Type -> Type where

public export
ServerSpec : Type
ServerSpec = List ((String, String), List (String, Type))

public export
interface HasParts (0 a : Type) (0 b : Type) where
  replacePartsNulls : AnyPtr -> AnyPtr -> AnyPtr

public export
data Query : ServerSpec -> List (String, Type) -> Type -> Type where
    Var : (name : String) -> HasVar name t ctxt  -> Query db ctxt t
    Lambda : (arg : String) -> (a : Type)  -> Query db ((arg, a) :: ctxt) b ->  Query db ctxt (a -> b)
    App : Query db ctxt (a -> b) -> Query db ctxt a -> Query db ctxt b
    GetTable : (d : String) -> (t : String) -> {auto p : KElem (d, t) db} -> Query db ctxt (Table (lookup db p))
    ReadTable : Query db ctxt (Table ts) -> Query db ctxt (Cursor (Record' ts))
    Between : HasParts a b => {default True leftBoundClosed : Bool} -> {default False rightBoundClosed : Bool} ->
                   Query db ctxt (Table ts) -> {auto 0 p : Elem ("id", a) ts} -> 
                     Query db ctxt b -> Query db ctxt b  -> Query db ctxt (Cursor (Record' ts))
    GetChanges : {default False includeInitial : Bool} -> (e : Query db ctxt (Cursor t)) -> Query db ctxt (Changes t)
    Insert' : Query db ctxt (Table ts) -> Query db ctxt (List (Record' ts)) -> 
                 Query db ctxt (Record [("first_error", Maybe String)])
    Insert : Query db ctxt (Table (("id", String) :: ts)) -> 
                Query db ctxt (List (Record' ts)) -> Query db ctxt (Record [("first_error", Maybe String)])
    Lit : JsonSerializable a => a -> Query db ctxt a
    StrEq : Query db ctxt (String -> String -> Bool)
    GetField : (key : String) -> {auto p : Vect.KElem key fields} -> Query db ctxt (Record fields -> Vect.lookup fields p)
    MapCursor : Query db ctxt ((a -> b) -> Cursor a -> Cursor b)


infixr 0 |>
infixl 1 <|

export
(<|) : Query db ctxt (a -> b) -> Query db ctxt a -> Query db ctxt b
(<|) = App

export
(|>) : Query db ctxt a -> Query db ctxt (a -> b) -> Query db ctxt b
(|>) x f = App f x

public export
interface KeyType a where

export
KeyType Int where
export
KeyType String where
export
(KeyType a, KeyType b) => KeyType (a, b) where

public export
data TableSchema : Type where
  MkTableSchema : (d : String) -> (n : String) -> (idTy : Type) -> 
                          (ts : List (String, Type)) -> {auto 0  prf : So (UniqueKeys (("id", idTy) :: ts))} -> 
                              (KeyType idTy, JsonSerializable (Record' (("id", idTy) :: ts))) =>
                                     TableSchema

public export
TableSchemaFields : TableSchema -> List (String, Type)
TableSchemaFields (MkTableSchema _ _ idTy ts) = ("id", idTy) :: ts

public export
TableSchemaDB : TableSchema -> String
TableSchemaDB (MkTableSchema d _ _ _) = d

public export
TableSchemaName : TableSchema -> String
TableSchemaName (MkTableSchema _ n _ _) = n

mutual
  public export
  data ServerSchema : Type where
    Nil : ServerSchema
    (::) : TableSchema -> ServerSchema -> ServerSchema

  public export
  ServerSchemaSpec : ServerSchema -> ServerSpec
  ServerSchemaSpec [] = []
  ServerSchemaSpec (t :: s) = ((TableSchemaDB t, TableSchemaName t), TableSchemaFields t) :: (ServerSchemaSpec s)
 
test : ServerSchema
test = [MkTableSchema "todoApp" "todoItem" Int [("desc", String)]]

export
data RethinkServer : ServerSpec -> Type where
  MkRethinkServer : AnyPtr -> RethinkServer a

%foreign "node:lambda: (cursor, callback)  => cursor.toArray((err, res) => callback(err ? err + '' : '')(res)())"
prim__toArray : AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()


toArrayPtr : AnyPtr -> Promise (Either String AnyPtr)
toArrayPtr cursor = 
  MkPromise $ \w => do
    primIO$ prim__toArray cursor $  \err, result => toPrim $ if err == "" then w $ Right result
                                                                          else w $ Left err
    pure $ MkPromiseHandler $ pure ()


%foreign "node:lambda: err  => err ? err + '' : ''"
prim__errToStr : AnyPtr -> String

%foreign "node:lambda: () => require('rethinkdb')"
prim__r : () -> AnyPtr

%foreign "node:lambda: (q, conn, callback)  => q.run(conn, (err, res) => callback(err ? err + '' : '')(res)())"
prim__run : AnyPtr -> AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()

runPtr : AnyPtr -> AnyPtr -> Promise (Either String AnyPtr)
runPtr query conn = 
  MkPromise $ \w => do
     primIO $ prim__run query conn $ \err, result => toPrim $ if err == "" then w $ Right result
                                                                           else w $ Left err
     pure $ MkPromiseHandler $ pure ()
 
readResultPtr : JsonSerializable a => Lazy String -> AnyPtr -> Either String a
readResultPtr contextForError ptr = 
  case fromPtr ptr of
    Nothing => Left "Error in \{contextForError} trying to read value \{prim__json_stringify ptr}"
    (Just x) => Right x

%foreign "node:lambda: (r, db, tbl) => r.expr([r.branch(r.dbList().contains(db), r.expr({}), r.dbCreate(db)), r.branch(r.db(db).tableList().contains(tbl), r.expr({}), r.db(db).tableCreate(tbl))])"
prim__table_if_not_exists : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, db, tbl) => r.db(db).table(tbl)"
prim__read_table : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, db, tbl) => r.db(db).table(tbl).wait()"
prim__wait_table : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (r, db, tbl) => r.db('ifui_meta').table('table_versions').insert([{'id': [db, tbl], 'version': 1}])"
prim__insert_table_version : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: (host, port, callback)  => {const r = require('rethinkdb'); r.connect({host:host, port: port}, (err, conn) => callback(err)(conn)())}"
prim__connect : String -> Int -> (AnyPtr -> AnyPtr -> PrimIO ()) -> PrimIO ()

getDbTableNames : TableSchema -> List String
getDbTableNames (MkTableSchema d t i ts) = [d, t]

migrateTable : AnyPtr -> TableSchema -> Int -> AnyPtr -> Promise (Maybe String)
migrateTable r (MkTableSchema d t i ts) 0 conn =
  do
    let create = prim__table_if_not_exists r d t
    let createV = prim__insert_table_version r d t
    Right _ <- runPtr create conn | Left x => pure $ Just x
    Right _ <- runPtr createV conn | Left x => pure $ Just x
    pure Nothing
migrateTable r (MkTableSchema d t i ts) 1 conn =
  pure Nothing
migrateTable r x v conn =
  pure $ Just "Unexpected version \{show v} for table \{show (getDbTableNames x)}"


getVersion : TableSchema -> List (Record [("id", List String), ("version", Int)]) -> Int
getVersion x versions = 
  fromMaybe 0 $ (\x => get "version" x) <$> find (\w => getDbTableNames x == get "id" w) versions

migrateServer : AnyPtr -> ServerSchema -> List (Record [("id", List String), ("version", Int)]) -> AnyPtr -> Promise (Maybe String)
migrateServer r [] versions conn = 
  pure Nothing
migrateServer r (x :: y) versions conn =
  do
    Nothing <- migrateTable r x (getVersion x versions) conn | Just e => pure $ Just e
    migrateServer r y versions conn

doMigration : ServerSchema -> AnyPtr -> Promise (Maybe String)
doMigration s conn = 
  do
    let r =  prim__r ()
    let addTableVers = prim__table_if_not_exists r "ifui_meta" "table_versions"
    Right _ <- runPtr addTableVers conn | Left x => pure $ Just x
    let waitTblVers = prim__wait_table r "ifui_meta" "table_versions"
    let readTblVers = prim__read_table r "ifui_meta" "table_versions"
    Right _ <- runPtr waitTblVers conn | Left x => pure $ Just x
    Right tableVersionsCursor <- runPtr readTblVers conn | Left x => pure $ Just x
    Right versionsPtr <- toArrayPtr tableVersionsCursor | Left x => pure $ Just x
    case readResultPtr "doMigration read versions Ptr" versionsPtr of
         Right versions => migrateServer r s versions conn
         Left err => pure $ Just err

export
connect :  String -> Int -> (s : ServerSchema) -> Promise (Either String (RethinkServer (ServerSchemaSpec s)))
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
connect' :  String -> Int -> (s : ServerSchema) -> Promise (RethinkServer (ServerSchemaSpec s))
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

%foreign "node:lambda: (f, x) => f(x)"
prim__app : AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (r, d, t) => r.db(d).table(t)"
prim__getTable : AnyPtr -> String -> String -> AnyPtr

%foreign "node:lambda: r => (x => (y => r.eq(x,y)))"
prim__req : AnyPtr -> AnyPtr

%foreign "node:lambda: k => (x => x(k))"
prim__rget : String -> AnyPtr

%foreign "node:lambda: (r, x) => r.expr(x)"
prim__expr : AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (r, t, xs) => t.insert(xs).do(x => x.merge(r.expr({'first_error': x('first_error').default(null)})))"
prim__insert : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (t, options) => t.changes(options)"
prim__changes : AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: r => (f => (x => r.map(x, f)))"
prim__rmap : AnyPtr -> AnyPtr

%foreign "node:lambda: (r, tbl, left, right, options) => tbl.between(left, right, options)"
prim__rbetween : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr 

%foreign "node:lambda: (x, replacement) => x.default(replacement)"
prim__rdefault : AnyPtr -> AnyPtr -> AnyPtr 

%foreign "node:lambda: r => r.minval"
prim__rminval : AnyPtr -> AnyPtr 

%foreign "node:lambda: r => r.maxval"
prim__rmaxval : AnyPtr -> AnyPtr

%foreign "node:lambda: x => x.count()"
prim__rcount : AnyPtr -> AnyPtr

%foreign "node:lambda: (r, test, thenValue, elseValue) => r.branch(test, thenValue, elseValue)"
prim__rifThenElse : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr 


export
HasParts String (Maybe String) where
  replacePartsNulls replacement x = prim__rdefault x replacement

%foreign "node:lambda (rfst, rsnd, x) => {const r = require('rethinkdb'); r.branch(x.count().eq(2), r.expr([]).prepend(rsnd(x[1])).prepend(rfst(x[0])), rsnd(x.slice(1)).prepend(rfst(x[0])))}"
prim__replacePartsNullsTuple : (AnyPtr -> AnyPtr) -> (AnyPtr -> AnyPtr) -> AnyPtr -> AnyPtr 

export
(HasParts a a', HasParts b b') => HasParts (a, b) (a', b') where
  replacePartsNulls replacement x = 
    prim__replacePartsNullsTuple 
      (replacePartsNulls {a = a} {b = a'} replacement) 
      (replacePartsNulls {a = b} {b = b'} replacement) 
      x

compileQuery : AnyPtr -> VarStack ctxt ->  Query db ctxt r -> AnyPtr 
compileQuery r vars (Var name x) = 
  getVar x vars
compileQuery r vars (Lambda arg a x) = 
  fnToPtr $ \w => compileQuery r (AddVar arg w vars) x
compileQuery r vars (App f x) = 
  prim__app (compileQuery r vars f) (compileQuery r vars x)
compileQuery r vars (GetTable d t) = 
  prim__getTable r d t
compileQuery r vars (ReadTable x) = 
  compileQuery r vars x
compileQuery r vars (Between {a} {b} {leftBoundClosed} {rightBoundClosed} tbl x y) = 
  prim__rbetween
    r
    (compileQuery r vars tbl) 
    (replacePartsNulls {a = a} {b = b} (prim__rminval r) $ compileQuery r vars x) 
    (replacePartsNulls {a = a} {b = b} (prim__rmaxval r) $ compileQuery r vars y) 
    (json2ptr $ JObject [("leftBound", JString $ if leftBoundClosed then "closed" else "open")
                        , ("rightBound", JString $ if rightBoundClosed then "closed" else "open")
                        ]
    )
compileQuery r vars (GetChanges x {includeInitial}) = 
  prim__changes (compileQuery r vars x) (json2ptr $ JObject [("includeInitial", JBoolean includeInitial)])
compileQuery r vars (Insert t xs) = 
  prim__insert r (compileQuery r vars t) (compileQuery r vars xs)
compileQuery r vars (Insert' t xs) = 
  prim__insert r (compileQuery r vars t) (compileQuery r vars xs)
compileQuery r vars (Lit x) = 
  prim__expr r $ toPtr x
compileQuery r vars StrEq = 
  prim__req r 
compileQuery r vars (GetField key) = 
  prim__rget key
compileQuery r vars MapCursor =
  prim__rmap r


%foreign "javascript:lambda: x=> x+''"
prim__toString : AnyPtr -> String

export
debugShowQuery : Query db [] t -> String
debugShowQuery x =
  let r = prim__r ()
      e = compileQuery r Empty x
  in prim__toString e

export
run : JsonSerializable a => RethinkServer ts -> Query ts [] a -> Promise (Either String a)
run (MkRethinkServer s) e = 
  do
    let r = prim__r ()
    let query = compileQuery r Empty e
    z <- runPtr query s
    case z of 
         Left err => pure $ Left err
         Right ptr => pure $ readResultPtr "run \{debugShowQuery e}"  ptr

export
run' : JsonSerializable a => RethinkServer ts -> Query ts [] a -> Promise a
run' x y = onErrPrint $ run x y

export
toArray : JsonSerializable a => RethinkServer ts -> Query ts [] (Cursor a) -> Promise (Either String (List a))
toArray (MkRethinkServer s) e = 
  do
    let r = prim__r ()
    let query = compileQuery r Empty e
    Right z <- runPtr query s | Left e => pure $ Left e
    Right res <- toArrayPtr z | Left e => pure $ Left e
    pure $ readResultPtr "toArray \{debugShowQuery e}" res

export
toArray' : JsonSerializable a => RethinkServer ts -> Query ts [] (Cursor a) -> Promise (List a)
toArray' s e = onErrPrint $ toArray s e

%foreign "node:lambda: (cursor, callback)  => cursor.each((err, res) => callback(err ? err + '' : '')(res)())"
prim__each : AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()

%foreign "node:lambda: (cursor, callback)  => cursor.close((err) => callback(err ? err + '' : '')())"
prim__close : AnyPtr -> (String -> PrimIO ()) -> PrimIO ()

%foreign "javascript:lambda: (k,x) => {if(x.hasOwnProperty(k)){return x} else {const res = {...x}; res[k]=null; return res}} "
prim__add_null_key : String -> AnyPtr -> AnyPtr


export
getChanges : JsonSerializable (Change a) => RethinkServer ts -> Query ts [] (Changes a) -> IOStream (Either String (Change a))
getChanges (MkRethinkServer conn) e =
   MkIOStream $ \w => do
    let r = prim__r ()
    cursor <- newIORef $ the (Maybe AnyPtr) Nothing
    canceled <- newIORef False
    let query = compileQuery r Empty e
    primIO $ prim__run query conn $ 
      \err, result => toPrim $ if err == "" then 
                                             if !(readIORef canceled) 
                                                then primIO $ prim__close result (\x => toPrim $ pure ())
                                                else primIO $ prim__each result $ \err_, r => 
                                                   if err_ == "" then toPrim $ w $ readResultPtr "getChanges \{debugShowQuery e}" (prim__add_null_key "old_val" r)
                                                                 else toPrim $ w $ Left err
                                             else w $ Left err
    pure $ MkStreamHandler $ do 
      writeIORef canceled True
      case !(readIORef cursor) of 
           Nothing => pure ()
           Just c => primIO $ prim__close c (\x => toPrim $ pure ())           

export
getChanges' : JsonSerializable (Change a) => RethinkServer ts -> Query ts [] (Changes a) -> IOStream (Change a)
getChanges' s e = onErrPrint $ getChanges s e

export
insert1' : JsonSerializable (List (Record' ts)) =>  RethinkServer db ->  Query db [] (Table ts) -> Record' ts -> Promise (Maybe String)
insert1' r t x =
  do
    res <- run r (Insert' t (Lit [x]))
    case res of
         (Left y) => pure $ Just y
         (Right y) => pure $ get "first_error" y
