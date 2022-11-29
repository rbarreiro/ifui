module IfuiServer.MongoDB

import public Ifui.ExtensibleRecords
import public Data.List.Elem
import public Data.Vect
import public Data.Vect.Elem
import public IfuiServer.Promise
import public Ifui.JSValue

public export
data Expr : Vect n Type -> Type -> Type where
    Var : Elem t ctxt -> Expr ctxt t
    TrueLit : Expr ctxt Bool 
    FalseLit : Expr ctxt Bool 
    StringLit : String -> Expr ctxt String 
    StrEq : Expr ctxt String -> Expr ctxt String -> Expr ctxt Bool
    GetField : (key : String) ->  {auto p : Elem (key, t) fields}  -> Expr ctxt (Record fields) -> Expr ctxt t

litString : String -> AnyPtr
litString x =
  mkJsObj [("$literal", (str2ptr x))]

exprToMongo : (0 ctxt : Vect n Type) -> (names : Vect n String) -> Expr ctxt a -> AnyPtr
exprToMongo ctxt names (Var x) = 
  str2ptr $ "$$"  ++ index (elemToFin x) names
exprToMongo ctxt names (StringLit x) = 
  litString x
exprToMongo ctxt names (StrEq x y) = 
  let mx = exprToMongo ctxt names x
      my = exprToMongo ctxt names y
  in mkJsObj [("$eq", (mkJsArray [mx, my]))]
exprToMongo ctxt names (GetField k v) =
 let mk = litString k
     mv = exprToMongo ctxt names v 
 in mkJsObj [("$getField", (mkJsObj [("field", mk), ("input", mv) ]))]
exprToMongo ctxt name TrueLit =
 jsv2ptr $ toJS True
exprToMongo ctxt name FalseLit =
 jsv2ptr $ toJS False

public export
data CollectionSchema : String -> String -> Type -> Type where
  MkCollectionSchema : (d : String) -> (c : String) -> (t : Type) -> CollectionSchema d c t
  MigrateCollection : (Expr [] (a -> b)) -> CollectionSchema d c a -> CollectionSchema d c b
  RenameCollection :  (d : String) -> (c : String) -> CollectionSchema d_ c_ t -> CollectionSchema d c t

public export
data ClientSchema : List (String, String, Type) -> Type where
  Nil : ClientSchema [] 
  (::) : CollectionSchema d c t -> ClientSchema ts -> ClientSchema ((d, c, t) :: ts)
  
export
data MongoClient : List (String, String, Type) -> Type where
  MkMongoClient : AnyPtr -> MongoClient a

export
data MongoDatabase : List (String, Type) -> Type where
  MkMongoDatabase : AnyPtr -> MongoDatabase a

export
data MongoCollection : Type -> Type where
  MkMongoCollection : AnyPtr -> MongoCollection a


%foreign "node:lambda: uri => {const { MongoClient } = require('mongodb'); return new MongoClient(uri)}"
prim__createMongoClient : String -> PrimIO AnyPtr
 
%foreign "node:lambda: (client, callback)  => client.connect().then(callback)"
prim__connectMongoClient : AnyPtr -> PrimIO () -> PrimIO ()

export
createMongoClient : ClientSchema a -> String -> Promise (MongoClient a)
createMongoClient s uri = 
  MkPromise $ \w => do
    cli <- (primIO $ prim__createMongoClient uri)
    primIO $ prim__connectMongoClient cli (toPrim $ w (MkMongoClient cli))
    pure $ MkPromiseHandler (pure ())

public export
GetDBTy : String -> List (String, String, Type) ->  List (String, Type)
GetDBTy str xs = [(c,t) | (d, c, t) <- xs, d == str]

%foreign "node:lambda: (client, db) => client.db(db)"
prim__getDB : AnyPtr -> String -> AnyPtr
export
getDB : (name: String) -> MongoClient a -> (MongoDatabase (GetDBTy name a))
getDB name (MkMongoClient cli) = 
  MkMongoDatabase $ prim__getDB cli name

%foreign "node:lambda: (client, collection) => client.collection(collection)"
prim__getCollection : AnyPtr -> String -> AnyPtr
export
getCollection : (name: String) -> MongoDatabase a -> {b : Type} -> {auto p : Elem (name, b) a} -> MongoCollection b
getCollection name (MkMongoDatabase db) = 
  MkMongoCollection $ prim__getCollection db name

export
data InsertResult = MkInsertResult AnyPtr


%foreign "node:lambda: (collection, document, callback)  => collection.insertOne(document).then(r => callback(r)())"
prim__insertOne : AnyPtr -> AnyPtr -> (AnyPtr -> PrimIO ()) -> PrimIO ()

export
insertOne : HasJSValue t => MongoCollection t -> t -> Promise (InsertResult)
insertOne (MkMongoCollection x) y = 
  MkPromise $ \w => do
    primIO $ prim__insertOne x (jsv2ptr $ toJS y) (\z => toPrim $ w (MkInsertResult z))
    pure $ MkPromiseHandler (pure ())

%foreign "node:lambda: (collection, query, options)  => collection.find(query, options)"
prim__find : AnyPtr -> AnyPtr -> AnyPtr -> AnyPtr

%foreign "node:lambda: (cursor, callback)  => cursor.toArray().then(\w => callback(w)())"
prim__toArray : AnyPtr -> (AnyPtr -> PrimIO ()) -> PrimIO ()

export
find : HasJSValue t => MongoCollection t -> (Expr [t] t -> Expr [t] Bool) -> Promise (List t)
find (MkMongoCollection x) f = 
  MkPromise $ \w => do
    let cursor = prim__find x (mkJsObj [("$expr", exprToMongo [t] ["ROOT"] (f (Var Here)))]) (mkJsObj []) 
    primIO $ prim__toArray cursor (\z => case the (Maybe (List t)) (fromPtr z) of
                                              o => ?h)
    pure $ MkPromiseHandler (pure ())

