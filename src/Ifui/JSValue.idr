module Ifui.JSValue

import Ifui.ExtensibleRecords

export
str2ptr : String -> AnyPtr
str2ptr = believe_me

fn2ptr : String -> AnyPtr
fn2ptr = believe_me

ptr2str : AnyPtr -> String
ptr2str = believe_me

int2ptr : Int -> AnyPtr
int2ptr = believe_me 

ptr2int : AnyPtr -> Int
ptr2int = believe_me 

export
data JSValue a = MkJSValue AnyPtr

export 
jsv2ptr : JSValue a -> AnyPtr
jsv2ptr (MkJSValue x) = x

public export
interface HasJSValue a where
  toJS : a -> JSValue a
  fromJS : JSValue a -> a
  checkPtr : AnyPtr -> Bool

export
fromPtr : HasJSValue a => AnyPtr -> Maybe a
fromPtr ptr =
  if checkPtr {a=a} ptr then Just $ fromJS $ MkJSValue ptr
                else Nothing

%foreign "javascript:lambda: x => typeof x"
prim__typeof : AnyPtr -> String

export
HasJSValue String where
  toJS x = MkJSValue $ str2ptr x
  fromJS (MkJSValue x) = ptr2str x
  checkPtr x = prim__typeof x == "string"

export
HasJSValue Int where
  toJS x = MkJSValue $ int2ptr x
  fromJS (MkJSValue x) = ptr2int x
  checkPtr x = prim__typeof x == "number" || prim__typeof x == "bigint"

%foreign "javascript:lambda: x => x>0"
prim__mkBool : Int  -> AnyPtr

%foreign "javascript:lambda: x => x+0"
prim__readBool : AnyPtr -> Int

%foreign "javascript:lambda: x => (x === null || x === undefined)+0"
prim__isNullOrUndefined : AnyPtr -> Int

%foreign "javascript:lambda: () => null"
prim__null : () -> AnyPtr

export
HasJSValue Bool where
  toJS x = MkJSValue $ prim__mkBool $ if x then 1 else 0
  fromJS (MkJSValue x) = prim__readBool x > 0
  checkPtr x = prim__typeof x == "boolean"

export
HasJSValue a => HasJSValue (Maybe a) where
  toJS Nothing = MkJSValue $ prim__null ()
  toJS (Just x) = MkJSValue $ jsv2ptr $ toJS x
  fromJS (MkJSValue x) = if prim__isNullOrUndefined x > 0 then Nothing
                                                          else Just $ fromJS {a=a} (MkJSValue x)
  checkPtr x = prim__isNullOrUndefined x > 0 || checkPtr {a=a} x

%foreign "javascript:lambda: () => {}"
prim__newObj : () -> AnyPtr

%foreign "javascript:lambda: (x, k, v) => {const res= {...x}; res[k]=v; return res}"
prim__setItem : AnyPtr -> String -> AnyPtr -> AnyPtr

%foreign "javascript:lambda: (x, k) => x[k]"
prim__getItem : AnyPtr -> String -> AnyPtr

%foreign "javascript:lambda: (x, k) => x.hasOwnProperty(k) + 0"
prim__hasItem : AnyPtr -> String -> Int

export
HasJSValue (Record []) where
  toJS [] = MkJSValue $ prim__newObj ()
  fromJS _ = []
  checkPtr x = prim__typeof x == "object"

export
{s : String} -> {t : Type} -> {p : UKeyListCanPrepend (s, t) ts} -> (HasJSValue t, HasJSValue (Record ts)) => HasJSValue (Record ((s,t) :: ts)) where
  toJS ((MkEntry s x) :: y) = 
    let MkJSValue y_ = toJS y
        MkJSValue x_ = toJS x
    in MkJSValue $ prim__setItem y_ s x_
  fromJS (MkJSValue ptr) = 
    case t of
         Maybe t_ =>
            let x = if prim__hasItem ptr s > 0 then the (Maybe t_) $ fromJS (MkJSValue $ prim__getItem ptr s)
                                               else Nothing
                y = the (Record ts) $ fromJS (MkJSValue ptr)
            in (MkEntry s x) :: y
         _ =>
            let x = the t $ fromJS (MkJSValue $ prim__getItem ptr s)
                y = the (Record ts) $ fromJS (MkJSValue ptr)
            in (MkEntry s x) :: y
  checkPtr ptr =
    case t of
         Maybe _ => 
           (checkPtr {a=Record ts} ptr) && 
                (prim__hasItem ptr s == 0 || checkPtr {a=t} (prim__getItem ptr s))
         _ =>
          (checkPtr {a=Record ts} ptr) && 
              (prim__hasItem ptr s > 0 && checkPtr {a=t} (prim__getItem ptr s))

%foreign "javascript:lambda: () => Object.freeze([])"
prim__newArray : () -> AnyPtr
%foreign "javascript:lambda: (val, ori) => Object.freeze(ori.concat([Object.freeze(val)]))"
prim__arrayAppend : AnyPtr -> AnyPtr -> AnyPtr
%foreign "javascript:lambda: x => x.length"
prim__arrayLength : AnyPtr -> Int
%foreign "javascript:lambda: (x, pos) => x[pos]"
prim__arrayGet : AnyPtr -> Int -> AnyPtr
%foreign "javascript:lambda: x => Array.isArray(x)+0 "
prim__isArray : AnyPtr -> Int

export
HasJSValue t => HasJSValue (List t) where
  toJS xs = 
    MkJSValue $
      foldl 
        (\ptr, val => prim__arrayAppend val ptr) 
        (prim__newArray ()) 
        [(let MkJSValue p = toJS x in p) | x <- xs]
  fromJS (MkJSValue ptr)  = 
    let len = prim__arrayLength ptr
    in
      if len > 0 then 
        [fromJS (MkJSValue $ prim__arrayGet ptr i) | i <-[0..(prim__arrayLength ptr - 1)]]
        else []
  checkPtr ptr = 
    let len = prim__arrayLength ptr
    in prim__isArray ptr > 0 && 
         (len == 0 || all (\i => (checkPtr {a=t}) $ prim__arrayGet ptr i ) [0..(len - 1)])


export
mkJsObj : List (String, AnyPtr) -> AnyPtr
mkJsObj xs = foldl (\ptr, (key, val) => prim__setItem ptr key val) (prim__newObj ()) xs

export
mkJsArray : List AnyPtr -> AnyPtr
mkJsArray xs = foldl (\ptr, val => prim__arrayAppend val ptr) (prim__newArray ()) xs

%foreign "javascript:lambda: x => x+''"
prim__ptrToString : AnyPtr -> String
export
ptrToString : AnyPtr -> String
ptrToString = prim__ptrToString

