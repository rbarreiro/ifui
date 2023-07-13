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
interface QueryTuple (0 a : Type) (0 b : Type) where
  tcons : AnyPtr 
  tfst : AnyPtr
  tsnd : AnyPtr

public export
interface QueryMaybe (0 a : Type) where
  isNothing : AnyPtr -> AnyPtr
  unwrapJust : AnyPtr -> AnyPtr
  nothing : AnyPtr
  wrap : AnyPtr -> AnyPtr

public export
interface QueryEq (0 a : Type) where
  req : AnyPtr

public export
interface QueryNum (0 a : Type) where
  radd : AnyPtr
  rmul : AnyPtr

public export
interface QuerySequence (0 f : Type -> Type) where


public export
interface QueryFiniteSequence (0 f : Type -> Type) where

mutual
  public export
  data Query : ServerSpec -> List (String, Type) -> Type -> Type where
      Var : (name : String) -> {auto p : KElem  name ctxt}  -> Query db ctxt (klookup ctxt p)
      Lambda : (arg : String) -> Query db ((arg, a) :: ctxt) b ->  Query db ctxt (a -> b)
      App : Query db ctxt (a -> b) -> Query db ctxt a -> Query db ctxt b
      GetTable : (d : String) -> (t : String) -> {auto p : KElem (d, t) db} -> Query db ctxt (Table (klookup db p))
      ReadTable : Query db ctxt (Table ts) -> Query db ctxt (Cursor (Record' ts))
      Between : HasParts a b => 
                   {default True leftBoundClosed : Bool} -> {default False rightBoundClosed : Bool} ->
                     Query db ctxt (Table (("id", a) :: ts)) ->  
                       Query db ctxt b -> Query db ctxt b  -> Query db ctxt (Cursor (Record' (("id", a) :: ts)))
      GetChanges : QueryMaybe a  => Bool -> Query db ctxt (Cursor a) -> Query db ctxt (Changes a)
      Insert' : Query db ctxt (Table ts) -> Query db ctxt (List (Record' ts)) -> 
                   Query db ctxt (Record [("first_error", Maybe String)])
      Insert : Query db ctxt (Table (("id", String) :: ts)) -> 
                  Query db ctxt (List (Record' ts)) -> Query db ctxt (Record [("first_error", Maybe String)])
      UpdateOne : Query db ctxt (Table (("id", a) :: ts)) -> Query db ctxt a ->
                    Update (Record' (("id", a) :: ts)) (Record' ts) -> Query db ctxt (Record [("first_error", Maybe String)])
      Lit : JsonSerializable a => a -> Query db ctxt a
      Eq : QueryEq a => Query db ctxt (a -> a -> Bool)
      GetField : (key : String) -> {auto p : Vect.KElem key fields} -> Query db ctxt (Record fields -> Vect.klookup fields p)
      Map : QuerySequence f => Query db ctxt ((a -> b) -> f a -> f b)
      ConcatMapList : QueryFiniteSequence g => Query db ctxt ((a -> g b) -> List a -> List b)
      GenerateUUID : Query db ctxt String
      Now : Query db ctxt Date
      ListPrepend : Query db ctxt (a -> List a -> List a)
      ListAppend : Query db ctxt (a -> List a -> List a)
      RecordPrependKey : (k : String) -> Query db ctxt a -> Query db ctxt (Record fields -> Record ((k, a) :: fields))
      TCons : QueryTuple a b =>  Query db ctxt (a -> b -> (a, b))
      TFst : QueryTuple a b =>  Query db ctxt ((a, b) -> a)
      TSnd : QueryTuple a b =>  Query db ctxt ((a, b) -> b)
      Slice : Query db ctxt (Int-> Maybe Int -> List a -> List a)
      Add : QueryNum a => Query db ctxt (a -> a -> a)
      Mul : QueryNum a => Query db ctxt (a -> a -> a)
      MatchMaybe : QueryMaybe a => Query db ctxt (b -> (a -> b) -> Maybe a -> b)
      Nothing : QueryMaybe a => Query db ctxt (Maybe a)
      Just : QueryMaybe a => Query db ctxt (a -> Maybe a)
      Relax : Query db ctxt a -> Query db ((k,b) :: ctxt) a
      EmptyList : Query db ctxt (List a)
      Nth : QueryMaybe a => QueryFiniteSequence f => Query db ctxt (Int -> f a -> Maybe a)
      CatMaybes : (QueryMaybe a, QuerySequence f) => Query db ctxt (f (Maybe a) -> f a)
      OrderBy : QuerySequence f => Query db ctxt ((a -> b) -> f a -> f a)
      Get : Query db ctxt (Table (("id", a) :: ts)) -> Query db ctxt (a -> Maybe (Record' (("id", a) :: ts)))

  public export
  data AtomicProof : Query db ctxt t -> Type where
    APApp : AtomicProof f -> AtomicProof x -> AtomicProof (App f x)
    APListAppend : AtomicProof ListAppend
    APListPrepend : AtomicProof ListPrepend
    APLit : JsonSerializable t => (x : t) -> AtomicProof (Lit x)
    APPNow : AtomicProof Now
    APVar : {auto p : List.KElem name ctxt} -> AtomicProof (Var {ctxt=ctxt} {p=p} name)
    APEmptyList : AtomicProof EmptyList
    APGetField : {auto p : Vect.KElem key fields} -> AtomicProof (GetField {p = p} key)

  namespace Update

    public export
    data FieldUpdates : Type -> Vect n (String, Type) -> Type where
      Nil : FieldUpdates a xs
      (::) : {k : String} -> {p : Elem (k, b) xs} -> Entry k (Update a b)  -> FieldUpdates a xs -> FieldUpdates a xs

    public export
    data Update : Type -> Type -> Type where
      UpdateValue : (q : Query d [("row", a)] b) -> {auto 0 p : AtomicProof q} -> Update a b
      UpdateFields : FieldUpdates a xs -> Update a (Record xs)


public export
getTableFields : (ts : ServerSpec) -> (d : String) -> (t : String) -> {auto p : KElem (d, t) ts} -> List (String, Type)
getTableFields ts d t = klookup' ts (d, t)

public export
getTableTy : (ts : ServerSpec) -> (d : String) -> (t : String) -> {auto p : KElem (d, t) ts} -> Type
getTableTy ts d t = Record' $ getTableFields ts d t



infixr 0 ^^ 
infixl 1 |>
infixl 2 <|

infixl 3 |.
infixl 3 .|

export
(<|) : Query db ctxt (a -> b) -> Query db ctxt a -> Query db ctxt b
(<|) = App

export
(|>) : Query db ctxt a -> Query db ctxt (a -> b) -> Query db ctxt b
(|>) x f = App f x

export
(|.) : Query db ctxt (a -> b) -> Query db ctxt (b -> c)  -> Query db ctxt (a -> c)
(|.) f g = Lambda "x" (Var "x" |> Relax f |> Relax g)

export
(.|) : Query db ctxt (b -> c)  -> Query db ctxt (a -> b)  -> Query db ctxt (a -> c)
(.|) f g = g |. f

public export
(^^) : QueryTuple a b  => Query db ctxt a -> Query db ctxt b -> Query db ctxt (a, b)
(^^) x y = TCons <| x <| y

export
FromString (Query db ctxt String) where
  fromString = Lit

export
(QueryNum a, JsonSerializable a, Num a) => Num (Query db ctxt a) where
  (+) x y = Add <| x <| y
  (*) x y = Mul <|x <| y
  fromInteger x = Lit $ fromInteger x


namespace QueryList
  public export
  Nil : Query db ctxt (List a)
  Nil = EmptyList
  
  public export
  (::) : Query db ctxt a -> Query db ctxt (List a) -> Query db ctxt (List a)
  (::) x y = ListPrepend <| x <| y

namespace QueryRecord
  public export
  Nil : Query db ctxt (Record [])
  Nil = Lit []

  public export
  (::) :{s : String} -> Entry s (Query db ctxt a) -> Query db ctxt (Record ts) -> Query db ctxt (Record ((s, a) :: ts))
  (::) {s} (MkEntry s x) y = RecordPrependKey s x <| y

export
QuerySequence Cursor where
export
QuerySequence List where
export
QuerySequence Changes where

export
QueryFiniteSequence Cursor where
export
QueryFiniteSequence List where

namespace Query
  export
  drop : Query db ctxt (Int -> List a -> List a)
  drop = Lambda "x" (Slice <| Var "x" <| Lit Nothing)

  export
  mapMaybe : QueryMaybe a => QueryMaybe b => Query db ctxt ((a -> b) -> Maybe a -> Maybe b)
  mapMaybe = Lambda "f" (Lambda "x" (MatchMaybe <| Nothing <| (Var "f" |. Just) <| Var "x"))

testQueryList : Query [] [] (List (Record [("a", String), ("b", String)]))
testQueryList = [[("a" ^= "1"), ("b" ^= "2")]]

public export
interface KeyType a where

export
KeyType Int where
export
KeyType String where
export
(KeyType a, KeyType b) => KeyType (a, b) where
export
KeyType a => KeyType (List a) where

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
  MkRethinkServer : Bool -> AnyPtr -> RethinkServer a

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
connect : {default False debug : Bool} -> String -> Int -> (s : ServerSchema) -> Promise (Either String (RethinkServer (ServerSchemaSpec s)))
connect host port x = 
  MkPromise $ \w =>
    do
      primIO $ prim__connect host port (\e, conn => toPrim $
                                                  do
                                                    let err = prim__errToStr e 
                                                    if err == "" then do 
                                                                        _ <- (doMigration x conn).run $ \z => case z of
                                                                                                                   Nothing => w $ Right $ MkRethinkServer debug conn
                                                                                                                   Just y => w $ Left y
                                                                        pure ()
                                                                 else w $ Left err
                                       )
      pure $ MkPromiseHandler (pure ())

export
connect' : {default False debug : Bool} -> String -> Int -> (s : ServerSchema) -> Promise (RethinkServer (ServerSchemaSpec s))
connect' host port x = onErrThrow $ connect {debug = debug} host port x

public export
data VarStack : List (String, Type) -> Type where
  Empty : VarStack []
  AddVar : {0 a : Type} -> (s : String) -> AnyPtr  -> VarStack ts -> VarStack ((s, a) :: ts)

getVar : KElem name ctxt -> VarStack ctxt -> AnyPtr
getVar KHere (AddVar name x y) = x
getVar (KThere x) (AddVar s y z) = getVar x z

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

%foreign "node:lambda: (r, nothing, wrap, t, options) => t.changes(options).map(x => r.expr({type: x('type'), old_val: r.branch(x('type').eq('add').or(x('type').eq('initial')),nothing, wrap(x('old_val'))), new_val: r.branch(x('type').eq('remove').or(x('type').eq('uninitial')),nothing, wrap(x('new_val'))) }))"
prim__changes : AnyPtr -> AnyPtr -> (AnyPtr -> AnyPtr) -> AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: r => (f => (x => r.map(x, f)))"
prim__rmap : AnyPtr -> AnyPtr

%foreign "node:lambda: () => (f => (x => x.concatMap(f)))"
prim__rconcatMap : () -> AnyPtr

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

%foreign "node:lambda: r => r.uuid()"
prim__ruuid : AnyPtr -> AnyPtr 

%foreign "node:lambda: r => r.now()"
prim__rnow : AnyPtr -> AnyPtr 

%foreign "node:lambda: () => (x => (y => y.prepend(x)))"
prim__rprepend : () -> AnyPtr

%foreign "node:lambda: () => (x => (y => y.append(x)))"
prim__rappend : () -> AnyPtr

%foreign "node:lambda: (k, v) => (x => x.merge({[k]: v}))"
prim__rmerge : String -> AnyPtr -> AnyPtr

%foreign "node:lambda: r => (start => (end => (x => r.branch(end.eq(null), x.slice(start), x.slice(start,end)))))"
prim__rslice : AnyPtr -> AnyPtr

%foreign "node:lambda: r => (x => (y => r.add(x,y)))"
prim__radd : AnyPtr -> AnyPtr

%foreign "node:lambda: r => (x => (y => r.mul(x,y)))"
prim__rmul : AnyPtr -> AnyPtr

%foreign "node:lambda: (r, isNothing, unwrap) => (ifNothing => (ifJust => (x => r.branch(isNothing(x), ifNothing, ifJust(unwrap(x))))))"
prim__rMatchMaybe : AnyPtr -> (AnyPtr -> AnyPtr) -> (AnyPtr -> AnyPtr) -> AnyPtr

%foreign "node:lambda: x => x.eq(null)"
prim__risNull : AnyPtr -> AnyPtr 

%foreign "node:lambda: r => r.expr(null)"
prim__rnull : AnyPtr -> AnyPtr 

%foreign "node:lambda: (r, x) => r.expr([x])"
prim__rsingletonList : AnyPtr -> AnyPtr -> AnyPtr 

%foreign "node:lambda: x => x(0)"
prim__rfst : AnyPtr -> AnyPtr 

%foreign "node:lambda: r => r.expr([])"
prim__remptyList : AnyPtr -> AnyPtr

%foreign "node:lambda: (r, nothing, wrap) => (k => (lst => r.branch( lst.count().gt(r.branch(k.ge(0), k, k.mul(-1).add(-1))), wrap(lst(k)), nothing )  ))"
prim__rnth : AnyPtr -> AnyPtr -> (AnyPtr -> AnyPtr) -> AnyPtr

%foreign "node:lambda: (isNothing, unwrap) => (lst => lst.filter(x => isNothing(x).not()).map(unwrap) )"
prim__rcatMaybes : (AnyPtr -> AnyPtr) -> (AnyPtr -> AnyPtr) -> AnyPtr

%foreign "node:lambda: x => x"
prim__rid : AnyPtr -> AnyPtr 

%foreign "node:lambda: f => f "
prim__fnToPtr : (AnyPtr -> AnyPtr) -> AnyPtr

%foreign "node:lambda: f => (x => x.orderBy(f))"
prim__rorderBy : () -> AnyPtr

%foreign "node:lambda: tbl => (x => tbl.get(x))"
prim__rGet : AnyPtr -> AnyPtr

export
HasParts String (Maybe String) where
  replacePartsNulls replacement x = prim__rdefault x replacement

%foreign "node:lambda: (r, rfst, rsnd, x) => r.branch(x.count().eq(2), r.expr([rfst(x(0)), rsnd(x(1))]), rsnd(x.slice(1)).prepend(rfst(x(0))))"
prim__replacePartsNullsTuple : AnyPtr -> (AnyPtr -> AnyPtr) -> (AnyPtr -> AnyPtr) -> AnyPtr -> AnyPtr 

export
(HasParts a a', HasParts b b') => HasParts (a, b) (a', b') where
  replacePartsNulls replacement x = 
    prim__replacePartsNullsTuple 
      (prim__r ())
      (replacePartsNulls {a = a} {b = a'} replacement) 
      (replacePartsNulls {a = b} {b = b'} replacement) 
      x

%foreign "node:lambda: (r) => (x => (y => r.expr([x,y]))) "
prim__rmakeListPair : AnyPtr -> AnyPtr

%foreign "node:lambda: () => (x => x(0)) "
prim__rlist0 : () -> AnyPtr

%foreign "node:lambda: () => (x => x(1)) "
prim__rlist1 : () -> AnyPtr

%foreign "node:lambda: () => (x => x.slice(1)) "
prim__rrest : () -> AnyPtr

export
QueryTuple a String where
  tcons = prim__rmakeListPair (prim__r ())
  tfst = prim__rlist0 ()
  tsnd = prim__rlist1 ()

export
QueryTuple a (b, c) where
  tcons = prim__rprepend ()
  tfst = prim__rlist0 ()
  tsnd = prim__rrest ()

export
QueryNum Int where
  radd = prim__radd (prim__r ())
  rmul = prim__rmul (prim__r ())

export
QueryMaybe JSON where
  isNothing = prim__risNull
  unwrapJust = prim__rfst
  nothing = prim__rnull (prim__r ())
  wrap = prim__rsingletonList (prim__r ())

export
QueryMaybe a => QueryMaybe (Maybe a) where
  isNothing = prim__risNull
  unwrapJust = prim__rfst
  nothing = prim__rnull (prim__r ())
  wrap = prim__rsingletonList (prim__r ())

export
QueryMaybe Int where
  isNothing = prim__risNull
  unwrapJust = prim__rid
  nothing = prim__rnull (prim__r ())
  wrap = prim__rid

export
QueryMaybe (a, b) where
  isNothing = prim__risNull
  unwrapJust = prim__rid
  nothing = prim__rnull (prim__r ())
  wrap = prim__rid

export
QueryMaybe String where
  isNothing = prim__risNull
  unwrapJust = prim__rid
  nothing = prim__rnull (prim__r ())
  wrap = prim__rid

export
QueryMaybe (Record rs) where
  isNothing = prim__risNull
  unwrapJust = prim__rid
  nothing = prim__rnull (prim__r ())
  wrap = prim__rid

%foreign "node:lambda: (r, f) => (x => r.literal(f(x)))"
prim__updateValue : AnyPtr -> AnyPtr -> AnyPtr 

%foreign "node:lambda: (t, x, u) => t.get(x).update(u) "
prim__updateOne : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr 

mutual

  compileFieldUpdates : AnyPtr -> FieldUpdates a xs  -> List (String, AnyPtr)
  compileFieldUpdates r [] = []
  compileFieldUpdates r ((MkEntry k x) :: rs) = (k, compileUpdate r x) :: compileFieldUpdates r rs

  compileUpdate : AnyPtr -> Update a b -> AnyPtr
  compileUpdate r (UpdateValue q) = prim__updateValue r (compileQuery r Empty (Lambda "row" q))
  compileUpdate r (UpdateFields upds) = let zs = compileFieldUpdates r upds in mkJsObj zs

  compileQuery : AnyPtr -> VarStack ctxt ->  Query db ctxt r -> AnyPtr 
  compileQuery r vars (Var name {p}) = 
    getVar p vars
  compileQuery r vars (Lambda arg x) = 
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
  compileQuery r vars (GetChanges {a} includeInitial x ) = 
    prim__changes 
      r 
      (nothing {a})  
      (wrap {a})
      (compileQuery r vars x) 
      (json2ptr $ JObject [("includeInitial", JBoolean includeInitial), ("includeTypes", JBoolean True)])
  compileQuery r vars (Insert t xs) = 
    prim__insert r (compileQuery r vars t) (compileQuery r vars xs)
  compileQuery r vars (Insert' t xs) = 
    prim__insert r (compileQuery r vars t) (compileQuery r vars xs)
  compileQuery r vars (UpdateOne t x u) =
    prim__updateOne (compileQuery r vars t) (compileQuery r vars x) (compileUpdate r u)
  compileQuery r vars (Lit x) = 
    prim__expr r $ toPtr x
  compileQuery r vars (Eq {a}) = 
    req {a}
  compileQuery r vars (GetField key) = 
    prim__rget key
  compileQuery r vars Map =
    prim__rmap r
  compileQuery r vars ConcatMapList =
    prim__rconcatMap ()
  compileQuery r vars GenerateUUID =
    prim__ruuid r
  compileQuery r vars Now =
    prim__rnow r
  compileQuery r vars ListPrepend =
    prim__rprepend ()
  compileQuery r vars ListAppend =
    prim__rappend ()
  compileQuery r vars (RecordPrependKey k v) =
    prim__rmerge k (compileQuery r vars v)
  compileQuery r vars (TCons {a} {b}) =
    tcons {a} {b}
  compileQuery r vars (TFst {a} {b}) =
    tfst {a} {b}
  compileQuery r vars (TSnd {a} {b}) =
    tsnd {a} {b}
  compileQuery r vars Slice =
    prim__rslice r
  compileQuery r vars (Add {a}) =
    radd {a}
  compileQuery r vars (Mul {a}) =
    rmul {a}  
  compileQuery r vars (MatchMaybe {a}) =
    prim__rMatchMaybe r (isNothing {a}) (unwrapJust {a})
  compileQuery r vars (Nothing {a}) =
    (nothing {a})
  compileQuery r vars (Just {a}) =
    prim__fnToPtr $ (wrap {a})
  compileQuery r (AddVar k y z) (Relax x) = 
    compileQuery r z x
  compileQuery r vars EmptyList =
    prim__remptyList r
  compileQuery r vars (Nth {a}) =
    prim__rnth r (nothing {a}) (wrap {a})
  compileQuery r vars (CatMaybes {a}) =
    prim__rcatMaybes (isNothing {a}) (unwrapJust {a})
  compileQuery r vars OrderBy =
    prim__rorderBy ()
  compileQuery r vars (Get x) = 
    prim__rGet $ compileQuery r vars x

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
run (MkRethinkServer debug s) e = 
  do
    let r = prim__r ()
    let query = compileQuery r Empty e
    z <- runPtr query s
    case z of 
         Left err => 
           pure $ Left err
         Right ptr => do
           if debug then putStrLn "Debug: run \{debugShowQuery e}" else pure ()
           pure $ readResultPtr "run \{debugShowQuery e}"  ptr

export
run' : JsonSerializable a => RethinkServer ts -> Query ts [] a -> Promise a
run' x y = onErrPrint $ run x y

export
toArray : JsonSerializable a => RethinkServer ts -> Query ts [] (Cursor a) -> Promise (Either String (List a))
toArray (MkRethinkServer debug s) e = 
  do
    let r = prim__r ()
    let query = compileQuery r Empty e
    Right z <- runPtr query s | Left e => pure $ Left e
    Right res <- toArrayPtr z | Left e => pure $ Left e
    if debug then putStrLn "Debug: toArray \{debugShowQuery e}" else pure ()
    pure $ readResultPtr "toArray \{debugShowQuery e}" res

export
toArray' : JsonSerializable a => RethinkServer ts -> Query ts [] (Cursor a) -> Promise (List a)
toArray' s e = onErrPrint $ toArray s e

%foreign "node:lambda: (cursor, callback)  => cursor.each((err, res) => callback(err ? err + '' : '')(res)())"
prim__each : AnyPtr -> (String -> AnyPtr -> PrimIO ()) -> PrimIO ()

%foreign "node:lambda: (cursor, callback)  => cursor.close((err) => callback(err ? err + '' : '')())"
prim__close : AnyPtr -> (String -> PrimIO ()) -> PrimIO ()

-- %foreign "javascript:lambda: (wrap, nothing, x) => "
prim__adaptChanges : (AnyPtr -> AnyPtr) -> AnyPtr -> AnyPtr -> AnyPtr


export
getChanges : JsonSerializable (Change a) => RethinkServer ts -> Query ts [] (Changes a) -> IOStream (Either String (Change a))
getChanges (MkRethinkServer debug conn) e =
   MkIOStream $ \w => do
    let r = prim__r ()
    cursor <- newIORef $ the (Maybe AnyPtr) Nothing
    canceled <- newIORef False
    let query = compileQuery r Empty e
    if debug then putStrLn "Debug: getChanges \{debugShowQuery e}" else pure ()
    primIO $ prim__run query conn $ 
      \err, result => toPrim $ if err == "" then 
                                             if !(readIORef canceled) 
                                                then primIO $ prim__close result (\x => toPrim $ pure ())
                                                else primIO $ prim__each result $ \err_, r => 
                                                   if err_ == "" then toPrim $ w $ readResultPtr 
                                                                                     "getChanges \{debugShowQuery e}" 
                                                                                     r
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

export
get' : JsonSerializable (Maybe (Record' (("id", a) :: ts))) => 
        JsonSerializable a =>
         RethinkServer db -> 
           Query db [] (Table (("id", a) :: ts)) -> a -> Promise (Maybe (Record' (("id", a) :: ts)))
get' r tbl x = run' r (Get tbl <| Lit x)

