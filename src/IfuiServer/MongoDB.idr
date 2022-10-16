module IfuiServer.MongoDB

import public Ifui.ExtensibleRecords
import Data.List.Elem
import Data.Vect
import Data.Vect.Elem

public export
data JsTy = 
    JsString
  | JsNumber
  | JsObj (List (String, JsTy))
  | JsList JsTy
  | JsBool

data Expr : Vect n JsTy -> JsTy -> Type where
    Var : Elem t ctxt -> Expr ctxt t
    StringLit : String -> Expr ctxt JsString 
    StrEq : Expr ctxt JsString -> Expr ctxt JsString -> Expr ctxt JsBool
    GetField : (key : String) ->  {auto p : Elem (key, t) fields}  -> Expr ctxt (JsObj fields) -> Expr ctxt t

%foreign "javascript:lambda: x => x"
prim__stringToAnyPtr : String -> PrimIO AnyPtr
stringToAnyPtr : HasIO io => String -> io  AnyPtr
stringToAnyPtr x = primIO $ prim__stringToAnyPtr x


%foreign "javascript:lambda: () => {}"
prim__newObj : () -> PrimIO AnyPtr
%foreign "javascript:lambda: (key, val, ori) =>{const res= {...ori}; res[key]=val; return res}"
prim__objSet : String -> AnyPtr -> AnyPtr -> PrimIO AnyPtr
mkJsObj : HasIO io => List (String, AnyPtr) -> io AnyPtr
mkJsObj xs = foldlM (\ptr, (key, val) => primIO $ prim__objSet key val ptr) !(primIO $ prim__newObj ()) xs

%foreign "javascript:lambda: () => []"
prim__newArray : () -> PrimIO AnyPtr
%foreign "javascript:lambda: (val, ori) => ori.concat([val])"
prim__arrayAppend : AnyPtr -> AnyPtr -> PrimIO AnyPtr
mkJsArray : HasIO io => List AnyPtr -> io AnyPtr
mkJsArray xs = foldlM (\ptr, val => primIO $ prim__arrayAppend val ptr) !(primIO $ prim__newArray ()) xs

litString : HasIO io =>  String -> io AnyPtr
litString x =
  mkJsObj [("$literal", !(stringToAnyPtr x))]

export
exprToMongo : (0 ctxt : Vect n JsTy) -> (names : Vect n String) -> Expr ctxt a -> IO AnyPtr
exprToMongo ctxt names (Var x) = 
  stringToAnyPtr $ "$$"  ++ index (elemToFin x) names
exprToMongo ctxt names (StringLit x) = 
  litString x
exprToMongo ctxt names (StrEq x y) = 
  do
    mx <- exprToMongo ctxt names x
    my <- exprToMongo ctxt names y
    mkJsObj [("$eq", !(mkJsArray [mx, my]))]
exprToMongo ctxt names (GetField k v) =
 do
  mk <- litString k
  mv <- exprToMongo ctxt names v 
  mkJsObj [("$getField", !(mkJsObj [("field", mk), ("input", mv) ]))]


public export
DatabaseSchema : Type
DatabaseSchema = List (String, JsTy)

public export
ClientSchema : Type
ClientSchema = List (String, DatabaseSchema)

export
data MongoClient : ClientSchema -> Type where
  MkMongoClient : AnyPtr -> MongoClient a

export
data MongoDatabase : DatabaseSchema -> Type where
  MkMongoDatabase : AnyPtr -> MongoDatabase a

export
data MongoCollection : JsTy -> Type where
  MkMongoCollection : AnyPtr -> MongoCollection a


--public export
--data MongoExpr : DocumentSchema -> Type -> Type where
  

%foreign "node:lambda: uri => new require('mongodb').MongoClient(uri)"
prim__createMongoClient : String -> PrimIO AnyPtr 
createMongoClient : HasIO io => (0 s : ClientSchema) -> String -> io (MongoClient s)
createMongoClient _ uri = 
  MkMongoClient <$> (primIO $ prim__createMongoClient uri)


%foreign "node:lambda: (client, db) => client.db(db)"
prim__getDB : AnyPtr -> String -> PrimIO AnyPtr
getDB : HasIO io => (name: String)  ->  {auto p : Elem (name, b) a} -> MongoClient a -> io (MongoDatabase b)
getDB name (MkMongoClient cli) = 
  MkMongoDatabase <$> (primIO $ prim__getDB cli name)

%foreign "node:lambda: (client, collection) => client.collection(collection)"
prim__getCollection : AnyPtr -> String -> PrimIO AnyPtr
getCollection : HasIO io => (name: String)  ->  {auto p : Elem (name, b) a} -> MongoDatabase a -> io (MongoCollection b)
getCollection name (MkMongoDatabase db) = 
  MkMongoCollection <$> (primIO $ prim__getCollection db name)
