class IdrisError extends Error { }

function __prim_js2idris_array(x){
  let acc = { h:0 };

  for (let i = x.length-1; i>=0; i--) {
      acc = { a1:x[i], a2:acc };
  }
  return acc;
}

function __prim_idris2js_array(x){
  const result = Array();
  while (x.h === undefined) {
    result.push(x.a1); x = x.a2;
  }
  return result;
}

function __lazy(thunk) {
  let res;
  return function () {
    if (thunk === undefined) return res;
    res = thunk();
    thunk = undefined;
    return res;
  };
};

function __prim_stringIteratorNew(_str) {
  return 0
}

function __prim_stringIteratorToString(_, str, it, f) {
  return f(str.slice(it))
}

function __prim_stringIteratorNext(str, it) {
  if (it >= str.length)
    return {h: 0};
  else
    return {a1: str.charAt(it), a2: it + 1};
}

function __tailRec(f,ini) {
  let obj = ini;
  while(true){
    switch(obj.h){
      case 0: return obj.a1;
      default: obj = f(obj);
    }
  }
}

const _idrisworld = Symbol('idrisworld')

const _crashExp = x=>{throw new IdrisError(x)}

const _bigIntOfString = s=> {
  try {
    const idx = s.indexOf('.')
    return idx === -1 ? BigInt(s) : BigInt(s.slice(0, idx))
  } catch (e) { return 0n }
}

const _numberOfString = s=> {
  try {
    const res = Number(s);
    return isNaN(res) ? 0 : res;
  } catch (e) { return 0 }
}

const _intOfString = s=> Math.trunc(_numberOfString(s))

const _truncToChar = x=> String.fromCodePoint(
  (x >= 0 && x <= 55295) || (x >= 57344 && x <= 1114111) ? x : 0
)

// Int8
const _truncInt8 = x => {
  const res = x & 0xff;
  return res >= 0x80 ? res - 0x100 : res;
}

const _truncBigInt8 = x => {
  const res = Number(x & 0xffn);
  return res >= 0x80 ? res - 0x100 : res;
}

// Euclidian Division
const _div = (a,b) => {
  const q = Math.trunc(a / b)
  const r = a % b
  return r < 0 ? (b > 0 ? q - 1 : q + 1) : q
}

const _divBigInt = (a,b) => {
  const q = a / b
  const r = a % b
  return r < 0n ? (b > 0n ? q - 1n : q + 1n) : q
}

// Euclidian Modulo
const _mod = (a,b) => {
  const r = a % b
  return r < 0 ? (b > 0 ? r + b : r - b) : r
}

const _modBigInt = (a,b) => {
  const r = a % b
  return r < 0n ? (b > 0n ? r + b : r - b) : r
}

const _add8s = (a,b) => _truncInt8(a + b)
const _sub8s = (a,b) => _truncInt8(a - b)
const _mul8s = (a,b) => _truncInt8(a * b)
const _div8s = (a,b) => _truncInt8(_div(a,b))
const _shl8s = (a,b) => _truncInt8(a << b)
const _shr8s = (a,b) => _truncInt8(a >> b)

// Int16
const _truncInt16 = x => {
  const res = x & 0xffff;
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _truncBigInt16 = x => {
  const res = Number(x & 0xffffn);
  return res >= 0x8000 ? res - 0x10000 : res;
}

const _add16s = (a,b) => _truncInt16(a + b)
const _sub16s = (a,b) => _truncInt16(a - b)
const _mul16s = (a,b) => _truncInt16(a * b)
const _div16s = (a,b) => _truncInt16(_div(a,b))
const _shl16s = (a,b) => _truncInt16(a << b)
const _shr16s = (a,b) => _truncInt16(a >> b)

//Int32
const _truncInt32 = x => x & 0xffffffff

const _truncBigInt32 = x => {
  const res = Number(x & 0xffffffffn);
  return res >= 0x80000000 ? res - 0x100000000 : res;
}

const _add32s = (a,b) => _truncInt32(a + b)
const _sub32s = (a,b) => _truncInt32(a - b)
const _div32s = (a,b) => _truncInt32(_div(a,b))

const _mul32s = (a,b) => {
  const res = a * b;
  if (res <= Number.MIN_SAFE_INTEGER || res >= Number.MAX_SAFE_INTEGER) {
    return _truncInt32((a & 0xffff) * b + (b & 0xffff) * (a & 0xffff0000))
  } else {
    return _truncInt32(res)
  }
}

//Int64
const _truncBigInt64 = x => {
  const res = x & 0xffffffffffffffffn;
  return res >= 0x8000000000000000n ? res - 0x10000000000000000n : res;
}

const _add64s = (a,b) => _truncBigInt64(a + b)
const _sub64s = (a,b) => _truncBigInt64(a - b)
const _mul64s = (a,b) => _truncBigInt64(a * b)
const _div64s = (a,b) => _truncBigInt64(_divBigInt(a,b))
const _shl64s = (a,b) => _truncBigInt64(a << b)
const _shr64s = (a,b) => _truncBigInt64(a >> b)

//Bits8
const _truncUInt8 = x => x & 0xff

const _truncUBigInt8 = x => Number(x & 0xffn)

const _add8u = (a,b) => (a + b) & 0xff
const _sub8u = (a,b) => (a - b) & 0xff
const _mul8u = (a,b) => (a * b) & 0xff
const _div8u = (a,b) => Math.trunc(a / b)
const _shl8u = (a,b) => (a << b) & 0xff
const _shr8u = (a,b) => (a >> b) & 0xff

//Bits16
const _truncUInt16 = x => x & 0xffff

const _truncUBigInt16 = x => Number(x & 0xffffn)

const _add16u = (a,b) => (a + b) & 0xffff
const _sub16u = (a,b) => (a - b) & 0xffff
const _mul16u = (a,b) => (a * b) & 0xffff
const _div16u = (a,b) => Math.trunc(a / b)
const _shl16u = (a,b) => (a << b) & 0xffff
const _shr16u = (a,b) => (a >> b) & 0xffff

//Bits32
const _truncUBigInt32 = x => Number(x & 0xffffffffn)

const _truncUInt32 = x => {
  const res = x & -1;
  return res < 0 ? res + 0x100000000 : res;
}

const _add32u = (a,b) => _truncUInt32(a + b)
const _sub32u = (a,b) => _truncUInt32(a - b)
const _mul32u = (a,b) => _truncUInt32(_mul32s(a,b))
const _div32u = (a,b) => Math.trunc(a / b)

const _shl32u = (a,b) => _truncUInt32(a << b)
const _shr32u = (a,b) => _truncUInt32(a <= 0x7fffffff ? a >> b : (b == 0 ? a : (a >> b) ^ ((-0x80000000) >> (b-1))))
const _and32u = (a,b) => _truncUInt32(a & b)
const _or32u = (a,b)  => _truncUInt32(a | b)
const _xor32u = (a,b) => _truncUInt32(a ^ b)

//Bits64
const _truncUBigInt64 = x => x & 0xffffffffffffffffn

const _add64u = (a,b) => (a + b) & 0xffffffffffffffffn
const _mul64u = (a,b) => (a * b) & 0xffffffffffffffffn
const _div64u = (a,b) => a / b
const _shl64u = (a,b) => (a << b) & 0xffffffffffffffffn
const _shr64u = (a,b) => (a >> b) & 0xffffffffffffffffn
const _sub64u = (a,b) => (a - b) & 0xffffffffffffffffn

//String
const _strReverse = x => x.split('').reverse().join('')

const _substr = (o,l,x) => x.slice(o, o + l)

const IfuiServer_RethinkDB_prim__wait_table = ( (r, db, tbl) => r.db(db).table(tbl).wait());
const IfuiServer_RethinkDB_prim__typeof = ( x => typeof x);
const IfuiServer_RethinkDB_prim__toString = ( x=> x+'');
const IfuiServer_RethinkDB_prim__toArray = ( (cursor, callback)  => cursor.toArray((err, res) => callback(err ? err + '' : '')(res)()));
const IfuiServer_RethinkDB_prim__table_if_not_exists = ( (r, db, tbl) => r.expr([r.branch(r.dbList().contains(db), r.expr({}), r.dbCreate(db)), r.branch(r.db(db).tableList().contains(tbl), r.expr({}), r.db(db).tableCreate(tbl))]));
const IfuiServer_RethinkDB_prim__setItem = ( (x, k, v) => {const res= {...x}; res[k]=v; return res});
const IfuiServer_RethinkDB_prim__run = ( (q, conn, callback)  => q.run(conn, (err, res) => callback(err ? err + '' : '')(res)()));
const IfuiServer_RethinkDB_prim__rmap = ( r => (f => (x => r.map(x, f))));
const IfuiServer_RethinkDB_prim__rget = ( k => (x => x(k)));
const IfuiServer_RethinkDB_prim__req = ( r => (x => (y => r.eq(x,y))));
const IfuiServer_RethinkDB_prim__read_table = ( (r, db, tbl) => r.db(db).table(tbl));
const IfuiServer_RethinkDB_prim__readBool = ( x => x+0);
const IfuiServer_RethinkDB_prim__r = ( () => require('rethinkdb'));
const IfuiServer_RethinkDB_prim__ptrToString = ( x => x+'');
const IfuiServer_RethinkDB_prim__objectKeys = ( (x) => Object.keys(x));
const IfuiServer_RethinkDB_prim__null = ( () => null);
const IfuiServer_RethinkDB_prim__newObj = ( () => {});
const IfuiServer_RethinkDB_prim__newArray = ( () => Object.freeze([]));
const IfuiServer_RethinkDB_prim__mkBool = ( x => x>0);
const IfuiServer_RethinkDB_prim__isNullOrUndefined = ( x => (x === null || x === undefined)+0);
const IfuiServer_RethinkDB_prim__isArray = ( x => Array.isArray(x)+0 );
const IfuiServer_RethinkDB_prim__insert_table_version = ( (r, db, tbl) => r.db('ifui_meta').table('table_versions').insert([{'id': [db, tbl], 'version': 1}]));
const IfuiServer_RethinkDB_prim__insert = ( (t, xs) => t.insert(xs));
const IfuiServer_RethinkDB_prim__getTable = ( (r, d, t) => r.db(d).table(t));
const IfuiServer_RethinkDB_prim__getItem = ( (x, k) => x[k]);
const IfuiServer_RethinkDB_prim__expr = ( (r, x) => r.expr(x));
const IfuiServer_RethinkDB_prim__errToStr = ( err  => err ? err + '' : '');
const IfuiServer_RethinkDB_prim__each = ( (cursor, callback)  => cursor.each((err, res) => callback(err ? err + '' : '')(res)()));
const IfuiServer_RethinkDB_prim__connect = ( (host, port, callback)  => {const r = require('rethinkdb'); r.connect({host:host, port: port}, (err, conn) => callback(err)(conn)())});
const IfuiServer_RethinkDB_prim__close = ( (cursor, callback)  => cursor.close((err) => callback(err ? err + '' : '')()));
const IfuiServer_RethinkDB_prim__changes = ( (t, options) => t.changes(options));
const IfuiServer_RethinkDB_prim__arrayLength = ( x => x.length);
const IfuiServer_RethinkDB_prim__arrayGet = ( (x, pos) => x[pos]);
const IfuiServer_RethinkDB_prim__arrayAppend = ( (val, ori) => Object.freeze(ori.concat([Object.freeze(val)])));
const IfuiServer_RethinkDB_prim__app = ( (r, f, x) => f(x));
const IfuiServer_RethinkDB_prim__add_null_key = ( (k,x) => {if(x.hasOwnProperty(k)) then return x;else {const res = {...x}; x[k]=null; return res}} );
const Prelude_Types_fastUnpack = ((str)=>__prim_js2idris_array(Array.from(str)));
const Prelude_Types_fastPack = ((xs)=>__prim_idris2js_array(xs).join(''));
const Prelude_IO_prim__putStr = (x=>process.stdout.write(x));
const IfuiServer_Http_prim__serve = ( (staticServer, req, res) => staticServer.serve(req,res));
const IfuiServer_Http_prim__listen = ( (server, port) => server.listen(port));
const IfuiServer_Http_prim__createStaticServer = ( folder => new(require('node-static').Server)(folder));
const IfuiServer_Http_prim__createHttpServer = ( requestListener => require('http').createServer((req, res) => requestListener(req)(res)()));
const IfuiServer_WebSockets_prim__wsSend = ( (ws, msg) => ws.send(msg));
const IfuiServer_WebSockets_prim__startWebSocketsServer = ( port => {const WebSocket = require('ws'); return new WebSocket.Server({port : port})});
const IfuiServer_WebSockets_prim__setOnMessageStr = ( (ws, onMessage) => ws.on('message', (msg) => onMessage(msg + '')()));
const IfuiServer_WebSockets_prim__setOnConnection = ( (wss, onConnection) => wss.on('connection', (ws) => onConnection(ws)()) );
/* {$tcOpt:1} */
function x24tcOpt_1($0) {
 switch($0.a4.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:1} */, a1: $0.a3};
  case undefined: /* cons */ {
   switch($0.a4.a2.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:1} */, a1: ($0.a3+$0.a1.a1($0.a4.a1))};
    default: return {h: 1 /* {TcContinue1:1} */, a1: $0.a1, a2: $0.a2, a3: ($0.a3+($0.a1.a1($0.a4.a1)+', ')), a4: $0.a4.a2};
   }
  }
 }
}

/* Prelude.Show.3192:12128:show' */
function Prelude_Show_n__3192_12128_showx27($0, $1, $2, $3) {
 return __tailRec(x24tcOpt_1, {h: 1 /* {TcContinue1:1} */, a1: $0, a2: $1, a3: $2, a4: $3});
}

/* {$tcOpt:2} */
function x24tcOpt_2($0) {
 switch($0.h) {
  case 1: /* {TcContinue2:1} */ {
   switch($0.a7.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:2} */, a1: {h: 0}};
    case undefined: /* cons */ return {h: 2 /* {TcContinue2:2} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a7.a1.a1, a8: $0.a7.a1.a2, a9: $0.a7.a2, a10: $0.a8, a11: Text_Lexer_Core_scan($0.a7.a1.a1, {h: 0}, $0.a8)};
   }
  }
  case 2: /* {TcContinue2:2} */ {
   switch($0.a11.h) {
    case undefined: /* just */ {
     const $16 = _add32s($0.a5, Number(_truncBigInt32(Text_Lexer_Core_n__3713_2499_countNLs($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1))));
     const $22 = Text_Lexer_Core_n__3713_2500_getCols($0.a1, $0.a2, $0.a3, $0.a4, $0.a5, $0.a6, $0.a11.a1.a1, $0.a4);
     return {h: 0 /* {TcDone:2} */, a1: {a1: {a1: {a1: $0.a8(Prelude_Types_fastPack(Prelude_Types_List_reverse($0.a11.a1.a1))), a2: 0, a3: {a1: $0.a5, a2: $0.a4, a3: $16, a4: $22}}, a2: {a1: $16, a2: {a1: $22, a2: $0.a11.a1.a2}}}}};
    }
    case 0: /* nothing */ return {h: 1 /* {TcContinue2:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3, a4: $0.a4, a5: $0.a5, a6: $0.a6, a7: $0.a9, a8: $0.a10};
   }
  }
 }
}

/* Text.Lexer.Core.3713:2501:getFirstToken */
function Text_Lexer_Core_n__3713_2501_getFirstToken($0, $1, $2, $3, $4, $5, $6, $7) {
 return __tailRec(x24tcOpt_2, {h: 1 /* {TcContinue2:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7});
}

/* Text.Lexer.Core.case block in tokenise,getFirstToken */
function Text_Lexer_Core_case__tokenisex2cgetFirstToken_2634($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 return __tailRec(x24tcOpt_2, {h: 2 /* {TcContinue2:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6, a8: $7, a9: $8, a10: $9, a11: $a});
}

/* {$tcOpt:3} */
function x24tcOpt_3($0) {
 switch($0.a2.h) {
  case 0: /* nil */ {
   switch($0.a3.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:3} */, a1: 1};
    default: return {h: 0 /* {TcDone:3} */, a1: 0};
   }
  }
  case undefined: /* cons */ {
   switch($0.a3.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1($0.a2.a1)($0.a3.a1)) {
      case 1: return {h: 1 /* {TcContinue3:1} */, a1: $0.a1, a2: $0.a2.a2, a3: $0.a3.a2};
      case 0: return {h: 0 /* {TcDone:3} */, a1: 0};
     }
    }
    default: return {h: 0 /* {TcDone:3} */, a1: 0};
   }
  }
  default: return {h: 0 /* {TcDone:3} */, a1: 0};
 }
}

/* Prelude.Types.== */
function Prelude_Types_x3dx3d_Eq_x28Listx20x24ax29($0, $1, $2) {
 return __tailRec(x24tcOpt_3, {h: 1 /* {TcContinue3:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:4} */
function x24tcOpt_4($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:4} */, a1: _truncToChar($0.a3)};
  case undefined: /* cons */ return {h: 1 /* {TcContinue4:1} */, a1: $0.a1, a2: $0.a2.a2, a3: _add32s(Language_JSON_String_Tokens_n__3261_1166_hexVal($0.a1, $0.a2.a1), _mul32s(Number(_truncBigInt32(16n)), $0.a3))};
 }
}

/* Language.JSON.String.Tokens.3261:1167:fromHex */
function Language_JSON_String_Tokens_n__3261_1167_fromHex($0, $1, $2) {
 return __tailRec(x24tcOpt_4, {h: 1 /* {TcContinue4:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:5} */
function x24tcOpt_5($0) {
 switch($0.h) {
  case 1: /* {TcContinue5:1} */ return {h: 2 /* {TcContinue5:2} */, a1: $0.a6, a2: $0.a5, a3: $0.a4, a4: $0.a3, a5: $0.a2, a6: $0.a1, a7: Text_Lexer_Core_n__3713_2501_getFirstToken($0.a6, $0.a5, $0.a4, $0.a3, $0.a2, $0.a1, $0.a5, $0.a6)};
  case 2: /* {TcContinue5:2} */ {
   switch($0.a7.h) {
    case undefined: /* just */ {
     switch($0.a6($0.a7.a1.a1.a1)) {
      case 1: return {h: 0 /* {TcDone:5} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: {h: 0}}}}};
      case 0: return {h: 1 /* {TcContinue5:1} */, a1: $0.a6, a2: $0.a7.a1.a2.a1, a3: $0.a7.a1.a2.a2.a1, a4: {a1: $0.a7.a1.a1, a2: $0.a3}, a5: $0.a2, a6: $0.a7.a1.a2.a2.a2};
     }
    }
    case 0: /* nothing */ return {h: 0 /* {TcDone:5} */, a1: {a1: Prelude_Types_List_reverse($0.a3), a2: {a1: $0.a5, a2: {a1: $0.a4, a2: $0.a1}}}};
   }
  }
 }
}

/* Text.Lexer.Core.tokenise : (a -> Bool) -> Int -> Int -> List (WithBounds a) -> TokenMap a -> List Char -> (List (WithBounds a),
(Int, (Int, List Char))) */
function Text_Lexer_Core_tokenise($0, $1, $2, $3, $4, $5) {
 return __tailRec(x24tcOpt_5, {h: 1 /* {TcContinue5:1} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5});
}

/* Text.Lexer.Core.case block in tokenise */
function Text_Lexer_Core_case__tokenise_2724($0, $1, $2, $3, $4, $5, $6) {
 return __tailRec(x24tcOpt_5, {h: 2 /* {TcContinue5:2} */, a1: $0, a2: $1, a3: $2, a4: $3, a5: $4, a6: $5, a7: $6});
}

/* {$tcOpt:6} */
function x24tcOpt_6($0) {
 switch($0.a2) {
  case 0n: return {h: 0 /* {TcDone:6} */, a1: $0.a1};
  default: {
   const $4 = ($0.a2-1n);
   return {h: 1 /* {TcContinue6:1} */, a1: {a1: $0.a3, a2: $0.a1}, a2: $4, a3: $0.a3};
  }
 }
}

/* Data.List.replicateTR : List a -> Nat -> a -> List a */
function Data_List_replicateTR($0, $1, $2) {
 return __tailRec(x24tcOpt_6, {h: 1 /* {TcContinue6:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:7} */
function x24tcOpt_7($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ return {h: 1 /* {TcContinue7:1} */, a1: {a1: $0.a1, a2: $0.a2($0.a3.a1)}, a2: $0.a2, a3: $0.a3.a2};
  case 0: /* nil */ return {h: 0 /* {TcDone:7} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.mapAppend : SnocList b -> (a -> b) -> List a -> List b */
function Prelude_Types_List_mapAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_7, {h: 1 /* {TcContinue7:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:8} */
function x24tcOpt_8($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:8} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a1($0.a2)($0.a3.a1.a1)) {
    case 1: return {h: 0 /* {TcDone:8} */, a1: {a1: $0.a3.a1.a2}};
    case 0: return {h: 1 /* {TcContinue8:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
 }
}

/* Data.List.lookupBy : (a -> b -> Bool) -> a -> List (b, v) -> Maybe v */
function Data_List_lookupBy($0, $1, $2) {
 return __tailRec(x24tcOpt_8, {h: 1 /* {TcContinue8:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:9} */
function x24tcOpt_9($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:9} */, a1: Prelude_Types_List_reverse($0.a2)};
  case undefined: /* cons */ return {h: 1 /* {TcContinue9:1} */, a1: $0.a1, a2: Prelude_Types_List_reverseOnto($0.a2, $0.a1($0.a3.a1)), a3: $0.a3.a2};
 }
}

/* Prelude.Types.listBindOnto : (a -> List b) -> List b -> List a -> List b */
function Prelude_Types_listBindOnto($0, $1, $2) {
 return __tailRec(x24tcOpt_9, {h: 1 /* {TcContinue9:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:10} */
function x24tcOpt_10($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:10} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue10:1} */, a1: {a1: $0.a2.a1, a2: $0.a1}, a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.reverseOnto : List a -> List a -> List a */
function Prelude_Types_List_reverseOnto($0, $1) {
 return __tailRec(x24tcOpt_10, {h: 1 /* {TcContinue10:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:11} */
function x24tcOpt_11($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:11} */, a1: $0.a1};
  case undefined: /* cons */ return {h: 1 /* {TcContinue11:1} */, a1: ($0.a1+1n), a2: $0.a2.a2};
 }
}

/* Prelude.Types.List.lengthPlus : Nat -> List a -> Nat */
function Prelude_Types_List_lengthPlus($0, $1) {
 return __tailRec(x24tcOpt_11, {h: 1 /* {TcContinue11:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:12} */
function x24tcOpt_12($0) {
 switch($0.a3.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:12} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue12:1} */, a1: $0.a1, a2: $0.a1($0.a2)($0.a3.a1), a3: $0.a3.a2};
 }
}

/* Prelude.Types.foldl */
function Prelude_Types_foldl_Foldable_List($0, $1, $2) {
 return __tailRec(x24tcOpt_12, {h: 1 /* {TcContinue12:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:13} */
function x24tcOpt_13($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:13} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a3) {
    case 0n: return {h: 0 /* {TcDone:13} */, a1: {a1: $0.a2.a1}};
    default: {
     const $7 = ($0.a3-1n);
     return {h: 1 /* {TcContinue13:1} */, a1: $0.a1, a2: $0.a2.a2, a3: $7};
    }
   }
  }
 }
}

/* Data.String.Extra.with block in index */
function Data_String_Extra_with__index_1618($0, $1, $2) {
 return __tailRec(x24tcOpt_13, {h: 1 /* {TcContinue13:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:14} */
function x24tcOpt_14($0) {
 switch($0.a2.h) {
  case 0: /* Nil */ return {h: 0 /* {TcDone:14} */, a1: {h: 0}};
  case 1: /* :: */ {
   switch($0.a2.a2.h) {
    case 0: /* MkRPC */ {
     switch(Prelude_EqOrd_x3dx3d_Eq_String($0.a1, $0.a2.a2.a2)) {
      case 1: return {h: 0 /* {TcDone:14} */, a1: {a1: {a1: 0, a2: {h: 0 /* MkRPC */, a1: $0.a2.a2.a1, a2: $0.a1, a3: $0.a2.a2.a3}}}};
      case 0: return {h: 1 /* {TcContinue14:1} */, a1: $0.a1, a2: $0.a2.a3};
     }
    }
    case 1: /* MkStreamService */ {
     switch(Prelude_EqOrd_x3dx3d_Eq_String($0.a1, $0.a2.a2.a2)) {
      case 1: return {h: 0 /* {TcDone:14} */, a1: {a1: {a1: 1, a2: {h: 1 /* MkStreamService */, a1: $0.a2.a2.a1, a2: $0.a1, a3: $0.a2.a2.a3}}}};
      case 0: return {h: 1 /* {TcContinue14:1} */, a1: $0.a1, a2: $0.a2.a3};
     }
    }
   }
  }
 }
}

/* IfuiServer.Server.getServiceU : (n : String) -> Server ts -> Maybe (k : ServiceKind ** Service n k) */
function IfuiServer_Server_getServiceU($0, $1) {
 return __tailRec(x24tcOpt_14, {h: 1 /* {TcContinue14:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:15} */
function x24tcOpt_15($0) {
 switch($0.a1) {
  case 0n: {
   switch($0.a2.h) {
    case undefined: /* cons */ return {h: 0 /* {TcDone:15} */, a1: {a1: $0.a2.a1}};
    default: return {h: 0 /* {TcDone:15} */, a1: {h: 0}};
   }
  }
  default: {
   const $8 = ($0.a1-1n);
   switch($0.a2.h) {
    case undefined: /* cons */ return {h: 1 /* {TcContinue15:1} */, a1: $8, a2: $0.a2.a2};
    default: return {h: 0 /* {TcDone:15} */, a1: {h: 0}};
   }
  }
 }
}

/* Prelude.Types.getAt : Nat -> List a -> Maybe a */
function Prelude_Types_getAt($0, $1) {
 return __tailRec(x24tcOpt_15, {h: 1 /* {TcContinue15:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:16} */
function x24tcOpt_16($0) {
 switch($0.a1.h) {
  case 0: /* Here */ {
   const $4 = $0.a2.a2;
   return {h: 0 /* {TcDone:16} */, a1: $4};
  }
  case 1: /* There */ return {h: 1 /* {TcContinue16:1} */, a1: $0.a1.a2, a2: $0.a2.a3};
 }
}

/* Ifui.ExtensibleRecords.getAux : Elem k t ts -> Record ts -> t */
function Ifui_ExtensibleRecords_getAux($0, $1) {
 return __tailRec(x24tcOpt_16, {h: 1 /* {TcContinue16:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:17} */
function x24tcOpt_17($0) {
 switch($0.a2.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:17} */, a1: {h: 0}};
  case undefined: /* cons */ {
   switch($0.a1($0.a2.a1)) {
    case 1: return {h: 0 /* {TcDone:17} */, a1: {a1: $0.a2.a1}};
    case 0: return {h: 1 /* {TcContinue17:1} */, a1: $0.a1, a2: $0.a2.a2};
   }
  }
 }
}

/* Data.List.find : (a -> Bool) -> List a -> Maybe a */
function Data_List_find($0, $1) {
 return __tailRec(x24tcOpt_17, {h: 1 /* {TcContinue17:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:18} */
function x24tcOpt_18($0) {
 switch($0.a3.h) {
  case undefined: /* cons */ {
   switch($0.a2($0.a3.a1)) {
    case 1: return {h: 1 /* {TcContinue18:1} */, a1: {a1: $0.a1, a2: $0.a3.a1}, a2: $0.a2, a3: $0.a3.a2};
    case 0: return {h: 1 /* {TcContinue18:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a2};
   }
  }
  case 0: /* nil */ return {h: 0 /* {TcDone:18} */, a1: Prelude_Types_SnocList_x3cx3ex3e($0.a1, {h: 0})};
 }
}

/* Prelude.Types.List.filterAppend : SnocList a -> (a -> Bool) -> List a -> List a */
function Prelude_Types_List_filterAppend($0, $1, $2) {
 return __tailRec(x24tcOpt_18, {h: 1 /* {TcContinue18:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:19} */
function x24tcOpt_19($0) {
 switch($0.a1) {
  case 0n: return {h: 0 /* {TcDone:19} */, a1: $0.a2};
  default: {
   const $4 = ($0.a1-1n);
   switch($0.a2.h) {
    case 0: /* nil */ return {h: 0 /* {TcDone:19} */, a1: {h: 0}};
    case undefined: /* cons */ return {h: 1 /* {TcContinue19:1} */, a1: $4, a2: $0.a2.a2};
   }
  }
 }
}

/* Data.List.drop : Nat -> List a -> List a */
function Data_List_drop($0, $1) {
 return __tailRec(x24tcOpt_19, {h: 1 /* {TcContinue19:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:20} */
function x24tcOpt_20($0) {
 switch($0.a3.h) {
  case 0: /* Var */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_getVar($0.a3.a2, $0.a2)};
  case 1: /* Lambda */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_fnToPtr(w => IfuiServer_RethinkDB_compileExpr($0.a1, {h: 1 /* AddVar */, a1: $0.a3.a1, a2: w, a3: $0.a2}, $0.a3.a3))};
  case 2: /* App */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__app($0.a1, IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a1), IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a2))};
  case 3: /* GetTable */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__getTable($0.a1, $0.a3.a1, $0.a3.a2)};
  case 4: /* ReadTable */ return {h: 1 /* {TcContinue20:1} */, a1: $0.a1, a2: $0.a2, a3: $0.a3.a1};
  case 5: /* GetChanges */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__changes(IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a1), IfuiServer_RethinkDB_json2ptr({h: 5 /* JObject */, a1: {a1: {a1: 'includeInitial', a2: {h: 1 /* JBoolean */, a1: $0.a3.a2}}, a2: {h: 0}}}))};
  case 7: /* Insert */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__insert(IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a2), IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a3))};
  case 6: /* Insert' */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__insert(IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a1), IfuiServer_RethinkDB_compileExpr($0.a1, $0.a2, $0.a3.a2))};
  case 8: /* Lit */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__expr($0.a1, IfuiServer_RethinkDB_toPtr($0.a3.a1, $0.a3.a2))};
  case 9: /* StrEq */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__req($0.a1)};
  case 10: /* GetField */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__rget($0.a3.a1)};
  case 11: /* MapCursor */ return {h: 0 /* {TcDone:20} */, a1: IfuiServer_RethinkDB_prim__rmap($0.a1)};
 }
}

/* IfuiServer.RethinkDB.compileExpr : AnyPtr -> VarStack ctxt -> Expr db ctxt r -> AnyPtr */
function IfuiServer_RethinkDB_compileExpr($0, $1, $2) {
 return __tailRec(x24tcOpt_20, {h: 1 /* {TcContinue20:1} */, a1: $0, a2: $1, a3: $2});
}

/* {$tcOpt:21} */
function x24tcOpt_21($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:21} */, a1: $0.a2.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue21:1} */, a1: $0.a1.a1, a2: $0.a2.a3};
 }
}

/* IfuiServer.RethinkDB.getVar : HasVar name a ctxt -> VarStack ctxt -> AnyPtr */
function IfuiServer_RethinkDB_getVar($0, $1) {
 return __tailRec(x24tcOpt_21, {h: 1 /* {TcContinue21:1} */, a1: $0, a2: $1});
}

/* {$tcOpt:22} */
function x24tcOpt_22($0) {
 switch($0.a1.h) {
  case 0: /* nil */ return {h: 0 /* {TcDone:22} */, a1: $0.a2};
  case undefined: /* cons */ return {h: 1 /* {TcContinue22:1} */, a1: $0.a1.a1, a2: {a1: $0.a1.a2, a2: $0.a2}};
 }
}

/* Prelude.Types.SnocList.<>> : SnocList a -> List a -> List a */
function Prelude_Types_SnocList_x3cx3ex3e($0, $1) {
 return __tailRec(x24tcOpt_22, {h: 1 /* {TcContinue22:1} */, a1: $0, a2: $1});
}

/* {__mainExpression:0} */
const __mainExpression_0 = __lazy(function () {
 return PrimIO_unsafePerformIO(Main_main());
});

/* {csegen:4} */
const csegen_4 = __lazy(function () {
 return {a1: $1 => Ifui_ExtensibleRecords_strNotEq('login', 'password', undefined, $1), a2: {h: 0}};
});

/* {csegen:5} */
const csegen_5 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_String($1), a2: $5 => Ifui_Json_fromJson_JsonSerializable_String($5), a3: $9 => Ifui_Json_stringify_JsonSerializable_String($9)};
});

/* {csegen:6} */
const csegen_6 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20Nilx29($1), a2: $5 => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20Nilx29($5)};
});

/* {csegen:7} */
const csegen_7 = __lazy(function () {
 return {a1: csegen_5(), a2: csegen_6()};
});

/* {csegen:11} */
const csegen_11 = __lazy(function () {
 return {a1: csegen_5(), a2: {a1: $4 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('password', {h: 0}, csegen_7(), $4), a2: $c => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('password', {h: 0}, csegen_7(), $c)}};
});

/* {csegen:14} */
const csegen_14 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('login', csegen_4(), csegen_11(), $1), a2: $a => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('login', csegen_4(), csegen_11(), $a)};
});

/* {csegen:31} */
const csegen_31 = __lazy(function () {
 return {a1: $1 => Ifui_ExtensibleRecords_strNotEq('old_val', 'new_val', undefined, $1), a2: {h: 0}};
});

/* {csegen:35} */
const csegen_35 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_x28Maybex20x24ax29(csegen_5(), $1), a2: $7 => Ifui_Json_fromJson_JsonSerializable_x28Maybex20x24ax29(csegen_5(), $7), a3: $d => Ifui_Json_stringify_JsonSerializable_x28Maybex20x24ax29(csegen_5(), $d)};
});

/* {csegen:36} */
const csegen_36 = __lazy(function () {
 return {a1: csegen_35(), a2: csegen_6()};
});

/* {csegen:40} */
const csegen_40 = __lazy(function () {
 return {a1: csegen_35(), a2: {a1: $4 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('new_val', {h: 0}, csegen_36(), $4), a2: $c => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('new_val', {h: 0}, csegen_36(), $c)}};
});

/* {csegen:43} */
const csegen_43 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('old_val', csegen_31(), csegen_40(), $1), a2: $a => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('old_val', csegen_31(), csegen_40(), $a)};
});

/* {csegen:47} */
const csegen_47 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_x24a(csegen_43(), $1), a2: $7 => Ifui_Json_fromJson_JsonSerializable_x24a(csegen_43(), $7), a3: $d => Ifui_Json_stringify_JsonSerializable_x24a(csegen_43(), $d)};
});

/* {csegen:49} */
const csegen_49 = __lazy(function () {
 return {a1: $1 => Ifui_ExtensibleRecords_strNotEq('id', 'desc', undefined, $1), a2: {h: 0}};
});

/* {csegen:55} */
const csegen_55 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($1), a2: $5 => Ifui_Json_fromJson_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($5), a3: $9 => Ifui_Json_stringify_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($9)};
});

/* {csegen:64} */
const csegen_64 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('desc', {h: 0}, csegen_7(), $1), a2: $9 => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('desc', {h: 0}, csegen_7(), $9)};
});

/* {csegen:68} */
const csegen_68 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_x24a(csegen_64(), $1), a2: $7 => Ifui_Json_fromJson_JsonSerializable_x24a(csegen_64(), $7), a3: $d => Ifui_Json_stringify_JsonSerializable_x24a(csegen_64(), $d)};
});

/* {csegen:90} */
const csegen_90 = __lazy(function () {
 return {a1: {a1: b => a => func => $2 => Prelude_IO_map_Functor_IO(func, $2), a2: a => $7 => $8 => $7, a3: b => a => $a => $b => PrimIO_io_bind($a, fx27 => PrimIO_io_bind($b, ax27 => $12 => fx27(ax27)))}, a2: b => a => $16 => $17 => PrimIO_io_bind($16, $17), a3: a => $1c => PrimIO_io_bind($1c, $20 => $20)};
});

/* {csegen:92} */
const csegen_92 = __lazy(function () {
 return {a1: $1 => $2 => ($1+$2), a2: ''};
});

/* {csegen:107} */
const csegen_107 = __lazy(function () {
 return {a1: acc => elem => func => init => input => Prelude_Types_foldr_Foldable_List(func, init, input), a2: elem => acc => func => init => input => Prelude_Types_foldl_Foldable_List(func, init, input), a3: elem => $b => Prelude_Types_null_Foldable_List($b), a4: elem => acc => m => $f => funcM => init => input => Prelude_Types_foldlM_Foldable_List($f, funcM, init, input), a5: elem => $16 => $16, a6: a => m => $18 => f => $19 => Prelude_Types_foldMap_Foldable_List($18, f, $19)};
});

/* {csegen:108} */
const csegen_108 = __lazy(function () {
 const $0 = $1 => $1;
 return $2 => $0($2);
});

/* {csegen:112} */
const csegen_112 = __lazy(function () {
 return $0 => $1 => $2 => $3 => Prelude_Types_map_Functor_Maybe($2, $3);
});

/* {csegen:119} */
const csegen_119 = __lazy(function () {
 return {a1: b => a => func => $1 => Prelude_Types_map_Functor_Maybe(func, $1), a2: a => $6 => ({a1: $6}), a3: b => a => $9 => $a => Prelude_Types_x3cx2ax3e_Applicative_Maybe($9, $a)};
});

/* {csegen:127} */
const csegen_127 = __lazy(function () {
 return {a1: b => a => func => $1 => Prelude_Types_List_mapAppend({h: 0}, func, $1), a2: csegen_107(), a3: b => a => f => $9 => $a => $b => Prelude_Types_traverse_Traversable_List($9, $a, $b)};
});

/* {csegen:130} */
const csegen_130 = __lazy(function () {
 return {a1: $1 => $2 => _add32s($1, $2), a2: $6 => $7 => _mul32s($6, $7), a3: $b => Number(_truncBigInt32($b))};
});

/* {csegen:144} */
const csegen_144 = __lazy(function () {
 return {a1: {a1: $2 => $3 => Prelude_EqOrd_x3dx3d_Eq_Int($2, $3), a2: $8 => $9 => Prelude_EqOrd_x2fx3d_Eq_Int($8, $9)}, a2: $e => $f => Prelude_EqOrd_compare_Ord_Int($e, $f), a3: $14 => $15 => Prelude_EqOrd_x3c_Ord_Int($14, $15), a4: $1a => $1b => Prelude_EqOrd_x3e_Ord_Int($1a, $1b), a5: $20 => $21 => Prelude_EqOrd_x3cx3d_Ord_Int($20, $21), a6: $26 => $27 => Prelude_EqOrd_x3ex3d_Ord_Int($26, $27), a7: $2c => $2d => Prelude_EqOrd_max_Ord_Int($2c, $2d), a8: $32 => $33 => Prelude_EqOrd_min_Ord_Int($32, $33)};
});

/* {csegen:148} */
const csegen_148 = __lazy(function () {
 return {a1: {a1: csegen_130(), a2: $4 => $5 => Prelude_Num_div_Integral_Int($4, $5), a3: $a => $b => Prelude_Num_mod_Integral_Int($a, $b)}, a2: {a1: csegen_144(), a2: {a1: csegen_130(), a2: $16 => _sub32s(0, $16), a3: $1a => $1b => _sub32s($1a, $1b)}}};
});

/* {csegen:152} */
const csegen_152 = __lazy(function () {
 return {a1: x => Prelude_Show_show_Show_String(x), a2: d => x => Prelude_Show_showPrec_Show_String(d, x)};
});

/* {csegen:156} */
const csegen_156 = __lazy(function () {
 const $0 = $1 => $2 => $3 => $4 => Prelude_Types_List_mapAppend({h: 0}, $3, $4);
 return $9 => $a => $0(undefined)(undefined)($9)($a);
});

/* {csegen:161} */
const csegen_161 = __lazy(function () {
 return {a1: $1 => Ifui_ExtensibleRecords_strNotEq('id', 'version', undefined, $1), a2: {h: 0}};
});

/* {csegen:166} */
const csegen_166 = __lazy(function () {
 return {a1: $1 => $2 => Prelude_EqOrd_x3dx3d_Eq_String($1, $2), a2: $7 => $8 => Prelude_EqOrd_x2fx3d_Eq_String($7, $8)};
});

/* {csegen:168} */
const csegen_168 = __lazy(function () {
 return {a1: csegen_90(), a2: a => $3 => $3};
});

/* {csegen:169} */
const csegen_169 = __lazy(function () {
 return Data_IORef_newIORef(csegen_168(), {h: 0});
});

/* {csegen:171} */
const csegen_171 = __lazy(function () {
 return x => {
  const $0 = $1 => (undefined);
  return $0;
 };
});

/* {csegen:173} */
const csegen_173 = __lazy(function () {
 return csegen_108()('table_versions');
});

/* {csegen:179} */
const csegen_179 = __lazy(function () {
 return {a1: {a1: $2 => Ifui_Json_toJson_JsonSerializable_Int($2), a2: $6 => Ifui_Json_fromJson_JsonSerializable_Int($6), a3: $a => Ifui_Json_stringify_JsonSerializable_Int($a)}, a2: csegen_6()};
});

/* {csegen:183} */
const csegen_183 = __lazy(function () {
 return {a1: {a1: $2 => Ifui_Json_toJson_JsonSerializable_x28Listx20x24ax29(csegen_5(), $2), a2: $8 => Ifui_Json_fromJson_JsonSerializable_x28Listx20x24ax29(csegen_5(), $8), a3: $e => Ifui_Json_stringify_JsonSerializable_x28Listx20x24ax29(csegen_5(), $e)}, a2: {a1: $15 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('version', {h: 0}, csegen_179(), $15), a2: $1d => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('version', {h: 0}, csegen_179(), $1d)}};
});

/* {csegen:186} */
const csegen_186 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('id', csegen_161(), csegen_183(), $1), a2: $a => Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29('id', csegen_161(), csegen_183(), $a)};
});

/* {csegen:190} */
const csegen_190 = __lazy(function () {
 return {a1: $1 => Ifui_Json_toJson_JsonSerializable_x24a(csegen_186(), $1), a2: $7 => Ifui_Json_fromJson_JsonSerializable_x24a(csegen_186(), $7), a3: $d => Ifui_Json_stringify_JsonSerializable_x24a(csegen_186(), $d)};
});

/* {csegen:214} */
const csegen_214 = __lazy(function () {
 return {a1: $1 => $2 => Prelude_EqOrd_x3dx3d_Eq_Char($1, $2), a2: $7 => $8 => Prelude_EqOrd_x2fx3d_Eq_Char($7, $8)};
});

/* {csegen:255} */
const csegen_255 = __lazy(function () {
 return b => a => func => $0 => Text_Bounded_map_Functor_WithBounds(func, $0);
});

/* {csegen:264} */
const csegen_264 = __lazy(function () {
 return $0 => $1 => $2 => $3 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, $2, $3);
});

/* {csegen:265} */
const csegen_265 = __lazy(function () {
 const $0 = csegen_264();
 return $2 => $3 => $0(undefined)(undefined)($2)($3);
});

/* {csegen:269} */
const csegen_269 = __lazy(function () {
 return {a1: $1 => Language_JSON_Tokens_TokType_TokenKind_JSONTokenKind($1), a2: kind => $5 => Language_JSON_Tokens_tokValue_TokenKind_JSONTokenKind(kind, $5)};
});

/* {csegen:272} */
const csegen_272 = __lazy(function () {
 return {a1: $1 => $2 => Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($1, $2), a2: $7 => $8 => Language_JSON_Tokens_x2fx3d_Eq_JSONTokenKind($7, $8)};
});

/* {csegen:314} */
const csegen_314 = __lazy(function () {
 return {a1: $1 => Language_JSON_String_Tokens_TokType_TokenKind_JSONStringTokenKind($1), a2: kind => $5 => Language_JSON_String_Tokens_tokValue_TokenKind_JSONStringTokenKind(kind, $5)};
});

/* {csegen:317} */
const csegen_317 = __lazy(function () {
 return {a1: $1 => $2 => Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($1, $2), a2: $7 => $8 => Language_JSON_String_Tokens_x2fx3d_Eq_JSONStringTokenKind($7, $8)};
});

/* {csegen:335} */
const csegen_335 = __lazy(function () {
 const $0 = b => a => func => $1 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(1, func, $1);
 return $6 => $7 => $0(undefined)(undefined)($6)($7);
});

/* {csegen:336} */
const csegen_336 = __lazy(function () {
 return csegen_335()($3 => $4 => $4);
});

/* {csegen:340} */
const csegen_340 = __lazy(function () {
 return $0 => $1 => $2 => $3 => Text_Bounded_map_Functor_WithBounds($2, $3);
});

/* {csegen:341} */
const csegen_341 = __lazy(function () {
 const $0 = csegen_112();
 return $2 => $3 => $0(undefined)(undefined)($2)($3);
});

/* {csegen:345} */
const csegen_345 = __lazy(function () {
 return {a1: {a1: csegen_108()('End of input'), a2: {h: 0}}, a2: {h: 0}};
});

/* {csegen:389} */
const csegen_389 = __lazy(function () {
 return {a1: x => Language_JSON_Data_show_Show_JSON(x), a2: d => x => Language_JSON_Data_showPrec_Show_JSON(d, x)};
});

/* prim__sub_Integer : Integer -> Integer -> Integer */
function prim__sub_Integer($0, $1) {
 return ($0-$1);
}

/* Main.main : IO () */
const Main_main = __lazy(function () {
 const $6 = _ => {
  const $a = $b => IfuiServer_RethinkDB_connectx27('todo_rethinkdb', Number(_truncBigInt32(28015n)), Api_Schema(), $b);
  const $9 = $a;
  const $8 = $9(db => IfuiServer_Server_startWsServerWithAuth({a1: {a1: $18 => Ifui_Json_toJson_JsonSerializable_x24a(csegen_14(), $18), a2: $1e => Ifui_Json_fromJson_JsonSerializable_x24a(csegen_14(), $1e), a3: $24 => Ifui_Json_stringify_JsonSerializable_x24a(csegen_14(), $24)}, a2: {a1: $2b => Ifui_Json_toJson_JsonSerializable_Bool($2b), a2: $2f => Ifui_Json_fromJson_JsonSerializable_Bool($2f), a3: $33 => Ifui_Json_stringify_JsonSerializable_Bool($33)}}, Number(_truncBigInt32(6402n)), Api_todoApi(db)));
  return PrimIO_io_bind($8, $3c => $3d => (undefined));
 };
 return PrimIO_io_bind(IfuiServer_Server_serveStatic(Number(_truncBigInt32(6401n)), 'www'), $6);
});

/* Api.todoApi : RethinkServer DBTy -> ServerWithAuth Login Bool RoleServer */
function Api_todoApi($0) {
 const $1a = x => {
  switch(x) {
   case 0: return {h: 0 /* Nil */};
   case 1: return Api_logedInTodoApi($0);
  }
 };
 return {a1: x => $2 => IfuiServer_Promise_pure_Applicative_Promise(Prelude_EqOrd_x3dx3d_Eq_String(Ifui_ExtensibleRecords_get('login', Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29(csegen_4()), x), Ifui_ExtensibleRecords_get('password', Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x24ox29x20x24tsx29(csegen_4(), Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29({h: 0})), x)), $2), a2: $1a};
}

/* Api.logedInTodoApi : RethinkServer DBTy -> Server ApiServices */
function Api_logedInTodoApi($0) {
 const $1 = {h: 3 /* GetTable */, a1: 'todoApp', a2: 'todoItem', a3: Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29({h: 0})};
 const $7 = $8 => IfuiServer_RethinkDB_getChangesx27(csegen_47(), $0, {h: 5 /* GetChanges */, a1: IfuiServer_RethinkDB_x7cx3e({h: 4 /* ReadTable */, a1: $1}, IfuiServer_RethinkDB_x3cx7c({h: 11 /* MapCursor */}, {h: 10 /* GetField */, a1: 'desc', a2: Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x24ox29x20x24tsx29(csegen_49(), Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29({h: 0}))})), a2: 1}, $8);
 return {h: 1 /* :: */, a1: {a1: $22 => Ifui_ExtensibleRecords_strNotEq('todoList', 'createTodo', undefined, $22), a2: {h: 0}}, a2: {h: 1 /* MkStreamService */, a1: {a1: csegen_55(), a2: csegen_47()}, a2: 'todoList', a3: $31 => $7}, a3: {h: 1 /* :: */, a1: {h: 0}, a2: {h: 0 /* MkRPC */, a1: {a1: csegen_5(), a2: csegen_55()}, a2: 'createTodo', a3: x => $3c => IfuiServer_Promise_x3ex3ex3d_Monad_Promise($3f => IfuiServer_RethinkDB_runx27({a1: $43 => Ifui_Json_toJson_JsonSerializable_x24a(csegen_6(), $43), a2: $49 => Ifui_Json_fromJson_JsonSerializable_x24a(csegen_6(), $49), a3: $4f => Ifui_Json_stringify_JsonSerializable_x24a(csegen_6(), $4f)}, $0, {h: 7 /* Insert */, a1: csegen_49(), a2: $1, a3: {h: 8 /* Lit */, a1: {a1: $5c => Ifui_Json_toJson_JsonSerializable_x28Listx20x24ax29(csegen_68(), $5c), a2: $62 => Ifui_Json_fromJson_JsonSerializable_x28Listx20x24ax29(csegen_68(), $62), a3: $68 => Ifui_Json_stringify_JsonSerializable_x28Listx20x24ax29(csegen_68(), $68)}, a2: {a1: {h: 1 /* :: */, a1: {h: 0}, a2: x, a3: {h: 0 /* Nil */}}, a2: {h: 0}}}}, $3f), _ => $75 => IfuiServer_Promise_pure_Applicative_Promise(undefined, $75), $3c)}, a3: {h: 0 /* Nil */}}};
}

/* Api.Schema : ServerSchema DBTy */
const Api_Schema = __lazy(function () {
 return {h: 1 /* :: */, a1: {a1: 'todoApp', a2: 'todoItem', a3: undefined, a4: {h: 1 /* :: */, a1: {a1: 'desc', a2: {h: 0}}, a2: {h: 0 /* Nil */}, a3: {h: 0}}, a5: csegen_49()}, a2: {h: 0 /* Nil */}, a3: {h: 0}};
});

/* IfuiServer.RethinkDB.|> : Expr db ctxt a -> Expr db ctxt (a -> b) -> Expr db ctxt b */
function IfuiServer_RethinkDB_x7cx3e($0, $1) {
 return {h: 2 /* App */, a1: $1, a2: $0};
}

/* IfuiServer.RethinkDB.toPtr : JsonSerializable a => a -> AnyPtr */
function IfuiServer_RethinkDB_toPtr($0, $1) {
 return IfuiServer_RethinkDB_json2ptr($0.a1($1));
}

/* IfuiServer.RethinkDB.toArrayPtr : AnyPtr -> Promise (Either String AnyPtr) */
function IfuiServer_RethinkDB_toArrayPtr($0, $1) {
 const $5 = $6 => {
  const $9 = err => result => {
   let $a;
   switch(Prelude_EqOrd_x3dx3d_Eq_String(err, '')) {
    case 1: {
     $a = $1({h: 1 /* Right */, a1: result});
     break;
    }
    case 0: {
     $a = $1({h: 0 /* Left */, a1: err});
     break;
    }
   }
   return $a;
  };
  return IfuiServer_RethinkDB_prim__toArray($0, $9, $6);
 };
 return Prelude_Interfaces_x3ex3e(csegen_90(), $5, () => $17 => $18 => (undefined));
}

/* IfuiServer.RethinkDB.str2ptr : String -> AnyPtr */
function IfuiServer_RethinkDB_str2ptr($0) {
 return $0;
}

/* IfuiServer.RethinkDB.runPtr : AnyPtr -> AnyPtr -> Promise (Either String AnyPtr) */
function IfuiServer_RethinkDB_runPtr($0, $1, $2) {
 const $6 = $7 => {
  const $b = err => result => {
   let $c;
   switch(Prelude_EqOrd_x3dx3d_Eq_String(err, '')) {
    case 1: {
     $c = $2({h: 1 /* Right */, a1: result});
     break;
    }
    case 0: {
     $c = $2({h: 0 /* Left */, a1: err});
     break;
    }
   }
   return $c;
  };
  return IfuiServer_RethinkDB_prim__run($0, $1, $b, $7);
 };
 return Prelude_Interfaces_x3ex3e(csegen_90(), $6, () => $19 => $1a => (undefined));
}

/* IfuiServer.RethinkDB.run' : JsonSerializable a => RethinkServer ts -> Expr ts [] a -> Promise a */
function IfuiServer_RethinkDB_runx27($0, $1, $2, $3) {
 return IfuiServer_Promise_onErrPrint(IfuiServer_RethinkDB_run($0, $1, $2), $3);
}

/* IfuiServer.RethinkDB.run : JsonSerializable a =>
RethinkServer ts -> Expr ts [] a -> Promise (Either String a) */
function IfuiServer_RethinkDB_run($0, $1, $2) {
 const $3 = $1;
 const $4 = IfuiServer_RethinkDB_prim__r(undefined);
 const $7 = IfuiServer_RethinkDB_compileExpr($4, {h: 0 /* Empty */}, $2);
 return $c => {
  const $14 = z => $15 => {
   switch(z.h) {
    case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({h: 0 /* Left */, a1: z.a1}, $15);
    case 1: /* Right */ return IfuiServer_Promise_pure_Applicative_Promise(IfuiServer_RethinkDB_readResultPtr($0, () => Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: csegen_108()('run '), a2: {a1: csegen_108()(IfuiServer_RethinkDB_debugShowExpr($2)), a2: {h: 0}}}), z.a1), $15);
   }
  };
  return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($f => IfuiServer_RethinkDB_runPtr($7, $3, $f), $14, $c);
 };
}

/* IfuiServer.RethinkDB.readResultPtr : JsonSerializable a => Lazy String -> AnyPtr -> Either String a */
function IfuiServer_RethinkDB_readResultPtr($0, $1, $2) {
 const $3 = IfuiServer_RethinkDB_fromPtr($0, $2);
 switch($3.h) {
  case 0: /* nothing */ return {h: 0 /* Left */, a1: Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Error in ', a2: {a1: csegen_108()($1()), a2: {a1: ' trying to read value ', a2: {a1: IfuiServer_RethinkDB_ptrToString($2), a2: {h: 0}}}}})};
  case undefined: /* just */ return {h: 1 /* Right */, a1: $3.a1};
 }
}

/* IfuiServer.RethinkDB.ptrToString : AnyPtr -> String */
function IfuiServer_RethinkDB_ptrToString($0) {
 return IfuiServer_RethinkDB_prim__ptrToString($0);
}

/* IfuiServer.RethinkDB.ptr2str : AnyPtr -> String */
function IfuiServer_RethinkDB_ptr2str($0) {
 return $0;
}

/* IfuiServer.RethinkDB.ptr2json : AnyPtr -> Maybe JSON */
function IfuiServer_RethinkDB_ptr2json($0) {
 switch(Prelude_EqOrd_x3e_Ord_Int(IfuiServer_RethinkDB_prim__isNullOrUndefined($0), Number(_truncBigInt32(0n)))) {
  case 1: return {a1: {h: 0 /* JNull */}};
  case 0: {
   switch(Prelude_EqOrd_x3e_Ord_Int(IfuiServer_RethinkDB_prim__isArray($0), Number(_truncBigInt32(0n)))) {
    case 1: return Prelude_Interfaces_x3cx24x3e(csegen_112(), $14 => ({h: 4 /* JArray */, a1: $14}), Prelude_Interfaces_sequence(csegen_119(), csegen_127(), Prelude_Types_listBind(Prelude_Types_rangeFromTo_Range_x24a(csegen_148(), 0, _sub32s(IfuiServer_RethinkDB_prim__arrayLength($0), 1)), i => Prelude_Types_pure_Applicative_List(IfuiServer_RethinkDB_ptr2json(IfuiServer_RethinkDB_prim__arrayGet($0, i))))));
    case 0: {
     switch(IfuiServer_RethinkDB_prim__typeof($0)) {
      case 'string': return {a1: {h: 3 /* JString */, a1: IfuiServer_RethinkDB_ptr2str($0)}};
      case 'number': return {a1: {h: 2 /* JNumber */, a1: IfuiServer_RethinkDB_ptr2double($0)}};
      case 'boolean': return {a1: {h: 1 /* JBoolean */, a1: Prelude_EqOrd_x3e_Ord_Int(IfuiServer_RethinkDB_prim__readBool($0), Number(_truncBigInt32(0n)))}};
      case 'object': {
       const $43 = IfuiServer_RethinkDB_prim__objectKeys($0);
       const $5e = i => {
        const $61 = IfuiServer_RethinkDB_ptr2str(IfuiServer_RethinkDB_prim__arrayGet($43, i));
        const $60 = Prelude_Interfaces_x3cx24x3e(csegen_112(), $6b => ({a1: $61, a2: $6b}), IfuiServer_RethinkDB_ptr2json(IfuiServer_RethinkDB_prim__getItem($0, $61)));
        return Prelude_Types_pure_Applicative_List($60);
       };
       const $52 = Prelude_Types_listBind(Prelude_Types_rangeFromTo_Range_x24a(csegen_148(), 0, _sub32s(IfuiServer_RethinkDB_prim__arrayLength($43), 1)), $5e);
       const $4c = Prelude_Interfaces_sequence(csegen_119(), csegen_127(), $52);
       return Prelude_Interfaces_x3cx24x3e(csegen_112(), $4a => ({h: 5 /* JObject */, a1: $4a}), $4c);
      }
      default: return {h: 0};
     }
    }
   }
  }
 }
}

/* IfuiServer.RethinkDB.ptr2double : AnyPtr -> Double */
function IfuiServer_RethinkDB_ptr2double($0) {
 return $0;
}

/* IfuiServer.RethinkDB.mkJsObj : List (String, AnyPtr) -> AnyPtr */
function IfuiServer_RethinkDB_mkJsObj($0) {
 return Prelude_Types_foldl_Foldable_List(ptr => $3 => IfuiServer_RethinkDB_prim__setItem(ptr, $3.a1, $3.a2), IfuiServer_RethinkDB_prim__newObj(undefined), $0);
}

/* IfuiServer.RethinkDB.mkJsArray : List AnyPtr -> AnyPtr */
function IfuiServer_RethinkDB_mkJsArray($0) {
 return Prelude_Types_foldl_Foldable_List(ptr => val => IfuiServer_RethinkDB_prim__arrayAppend(val, ptr), IfuiServer_RethinkDB_prim__newArray(undefined), $0);
}

/* IfuiServer.RethinkDB.migrateTable : AnyPtr -> TableSchema d t xs -> Int -> AnyPtr -> Promise (Maybe String) */
function IfuiServer_RethinkDB_migrateTable($0, $1, $2, $3) {
 switch($1.h) {
  case undefined: /* record */ {
   switch($2) {
    case 0: {
     const $6 = IfuiServer_RethinkDB_prim__table_if_not_exists($0, $1.a1, $1.a2);
     const $b = IfuiServer_RethinkDB_prim__insert_table_version($0, $1.a1, $1.a2);
     return $10 => {
      const $18 = $19 => $1a => {
       switch($19.h) {
        case 1: /* Right */ {
         const $23 = $24 => $25 => {
          switch($24.h) {
           case 1: /* Right */ return IfuiServer_Promise_pure_Applicative_Promise({h: 0}, $25);
           case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $24.a1}, $25);
          }
         };
         return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($1e => IfuiServer_RethinkDB_runPtr($b, $3, $1e), $23, $1a);
        }
        case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $19.a1}, $1a);
       }
      };
      return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($13 => IfuiServer_RethinkDB_runPtr($6, $3, $13), $18, $10);
     };
    }
    case 1: return $34 => IfuiServer_Promise_pure_Applicative_Promise({h: 0}, $34);
    default: return $38 => IfuiServer_Promise_pure_Applicative_Promise({a1: Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Unexpected version ', a2: {a1: Prelude_Show_show_Show_Int($2), a2: {a1: ' for table ', a2: {a1: Prelude_Show_show_Show_x28Listx20x24ax29(csegen_152(), IfuiServer_RethinkDB_getDbTableNames($1)), a2: {h: 0}}}}})}, $38);
   }
  }
  default: return $53 => IfuiServer_Promise_pure_Applicative_Promise({a1: Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Unexpected version ', a2: {a1: Prelude_Show_show_Show_Int($2), a2: {a1: ' for table ', a2: {a1: Prelude_Show_show_Show_x28Listx20x24ax29(csegen_152(), IfuiServer_RethinkDB_getDbTableNames($1)), a2: {h: 0}}}}})}, $53);
 }
}

/* IfuiServer.RethinkDB.migrateServer : AnyPtr -> ServerSchema ts -> List (Record [("id", List String),
                                           ("version",
                                           Int)]) -> AnyPtr -> Promise (Maybe String) */
function IfuiServer_RethinkDB_migrateServer($0, $1, $2, $3, $4) {
 switch($1.h) {
  case 0: /* Nil */ return IfuiServer_Promise_pure_Applicative_Promise({h: 0}, $4);
  case 1: /* :: */ {
   const $13 = $14 => $15 => {
    switch($14.h) {
     case 0: /* nothing */ return IfuiServer_RethinkDB_migrateServer($0, $1.a2, $2, $3, $15);
     case undefined: /* just */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $14.a1}, $15);
    }
   };
   return IfuiServer_Promise_x3ex3ex3d_Monad_Promise(IfuiServer_RethinkDB_migrateTable($0, $1.a1, IfuiServer_RethinkDB_getVersion($1.a1, $2), $3), $13, $4);
  }
 }
}

/* IfuiServer.RethinkDB.json2ptr : JSON -> AnyPtr */
function IfuiServer_RethinkDB_json2ptr($0) {
 switch($0.h) {
  case 0: /* JNull */ return IfuiServer_RethinkDB_prim__null(undefined);
  case 1: /* JBoolean */ {
   let $5;
   switch($0.a1) {
    case 1: {
     $5 = 1;
     break;
    }
    case 0: {
     $5 = 0;
     break;
    }
   }
   return IfuiServer_RethinkDB_prim__mkBool($5);
  }
  case 2: /* JNumber */ return IfuiServer_RethinkDB_double2ptr($0.a1);
  case 3: /* JString */ return IfuiServer_RethinkDB_str2ptr($0.a1);
  case 4: /* JArray */ return IfuiServer_RethinkDB_mkJsArray(csegen_156()($11 => IfuiServer_RethinkDB_json2ptr($11))($0.a1));
  case 5: /* JObject */ return IfuiServer_RethinkDB_mkJsObj(csegen_156()($1b => ({a1: $1b.a1, a2: IfuiServer_RethinkDB_json2ptr($1b.a2)}))($0.a1));
 }
}

/* IfuiServer.RethinkDB.getVersion : TableSchema d t xs -> List (Record [("id", List String),
                                    ("version", Int)]) -> Int */
function IfuiServer_RethinkDB_getVersion($0, $1) {
 return Data_Maybe_fromMaybe(() => Number(_truncBigInt32(0n)), Prelude_Interfaces_x3cx24x3e(csegen_112(), $a => Ifui_ExtensibleRecords_get('version', Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x24ox29x20x24tsx29(csegen_161(), Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29({h: 0})), $a), Data_List_find(w => Prelude_Types_x3dx3d_Eq_x28Listx20x24ax29(csegen_166(), IfuiServer_RethinkDB_getDbTableNames($0), Ifui_ExtensibleRecords_get('id', Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29(csegen_161()), w)), $1)));
}

/* IfuiServer.RethinkDB.getDbTableNames : TableSchema d t xs -> List String */
function IfuiServer_RethinkDB_getDbTableNames($0) {
 return {a1: $0.a1, a2: {a1: $0.a2, a2: {h: 0}}};
}

/* IfuiServer.RethinkDB.getChanges' : JsonSerializable (Change a) =>
RethinkServer ts -> Expr ts [] (Changes a) -> IOStream (Change a) */
function IfuiServer_RethinkDB_getChangesx27($0, $1, $2, $3) {
 return IfuiServer_IOStream_onErrPrint(IfuiServer_RethinkDB_getChanges($0, $1, $2), $3);
}

/* IfuiServer.RethinkDB.getChanges : JsonSerializable (Change a) =>
RethinkServer ts -> Expr ts [] (Changes a) -> IOStream (Either String (Change a)) */
function IfuiServer_RethinkDB_getChanges($0, $1, $2) {
 const $3 = $1;
 return w => {
  const $4 = IfuiServer_RethinkDB_prim__r(undefined);
  const $a = cursor => {
   const $11 = canceled => {
    const $12 = IfuiServer_RethinkDB_compileExpr($4, {h: 0 /* Empty */}, $2);
    const $1a = $1b => {
     const $1f = err => result => {
      let $20;
      switch(Prelude_EqOrd_x3dx3d_Eq_String(err, '')) {
       case 1: {
        const $27 = canceled;
        const $26 = $28 => ($27.value);
        const $2c = $2d => $2e => {
         switch($2d) {
          case 1: return IfuiServer_RethinkDB_prim__close(result, csegen_171(), $2e);
          case 0: {
           const $37 = err_ => $38 => {
            switch(Prelude_EqOrd_x3dx3d_Eq_String(err_, csegen_108()(''))) {
             case 1: {
              const $40 = w(IfuiServer_RethinkDB_readResultPtr($0, () => Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'getChanges ', a2: {a1: IfuiServer_RethinkDB_debugShowExpr($2), a2: {h: 0}}}), IfuiServer_RethinkDB_prim__add_null_key('old_val', $38)));
              return $40;
             }
             case 0: {
              const $56 = w({h: 0 /* Left */, a1: err});
              return $56;
             }
            }
           };
           return IfuiServer_RethinkDB_prim__each(result, $37, $2e);
          }
         }
        };
        $20 = PrimIO_io_bind($26, $2c);
        break;
       }
       case 0: {
        $20 = w({h: 0 /* Left */, a1: err});
        break;
       }
      }
      return $20;
     };
     return IfuiServer_RethinkDB_prim__run($12, $3, $1f, $1b);
    };
    return Prelude_Interfaces_x3ex3e(csegen_90(), $1a, () => $60 => {
     const $65 = canceled;
     const $64 = $66 => ($65.value=1);
     const $6b = () => {
      const $6e = cursor;
      const $6d = $6f => ($6e.value);
      const $73 = $74 => $75 => {
       switch($74.h) {
        case 0: /* nothing */ return undefined;
        case undefined: /* just */ return IfuiServer_RethinkDB_prim__close($74.a1, csegen_171(), $75);
       }
      };
      return PrimIO_io_bind($6d, $73);
     };
     return Prelude_Interfaces_x3ex3e(csegen_90(), $64, $6b);
    });
   };
   return PrimIO_io_bind(Data_IORef_newIORef(csegen_168(), 0), $11);
  };
  return PrimIO_io_bind(csegen_169(), $a);
 };
}

/* IfuiServer.RethinkDB.fromPtr : JsonSerializable a => AnyPtr -> Maybe a */
function IfuiServer_RethinkDB_fromPtr($0, $1) {
 return Prelude_Types_x3ex3ex3d_Monad_Maybe(IfuiServer_RethinkDB_ptr2json($1), $7 => $0.a2($7));
}

/* IfuiServer.RethinkDB.fnToPtr : (AnyPtr -> AnyPtr) -> AnyPtr */
function IfuiServer_RethinkDB_fnToPtr($0) {
 return $0;
}

/* IfuiServer.RethinkDB.double2ptr : Double -> AnyPtr */
function IfuiServer_RethinkDB_double2ptr($0) {
 return $0;
}

/* IfuiServer.RethinkDB.doMigration : ServerSchema ts -> AnyPtr -> Promise (Maybe String) */
function IfuiServer_RethinkDB_doMigration($0, $1) {
 const $2 = IfuiServer_RethinkDB_prim__r(undefined);
 const $5 = IfuiServer_RethinkDB_prim__table_if_not_exists($2, 'ifui_meta', 'table_versions');
 return $a => {
  const $12 = $13 => {
   switch($13.h) {
    case 1: /* Right */ {
     const $15 = IfuiServer_RethinkDB_prim__wait_table($2, 'ifui_meta', csegen_173());
     const $1b = IfuiServer_RethinkDB_prim__read_table($2, 'ifui_meta', csegen_173());
     return $21 => {
      const $29 = $2a => $2b => {
       switch($2a.h) {
        case 1: /* Right */ {
         const $34 = $35 => $36 => {
          switch($35.h) {
           case 1: /* Right */ {
            const $3e = $3f => {
             switch($3f.h) {
              case 1: /* Right */ {
               return $41 => {
                const $42 = IfuiServer_RethinkDB_readResultPtr({a1: $46 => Ifui_Json_toJson_JsonSerializable_x28Listx20x24ax29(csegen_190(), $46), a2: $4c => Ifui_Json_fromJson_JsonSerializable_x28Listx20x24ax29(csegen_190(), $4c), a3: $52 => Ifui_Json_stringify_JsonSerializable_x28Listx20x24ax29(csegen_190(), $52)}, () => csegen_108()('doMigration'), $3f.a1);
                switch($42.h) {
                 case 1: /* Right */ return IfuiServer_RethinkDB_migrateServer($2, $0, $42.a1, $1, $41);
                 case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $42.a1}, $41);
                }
               };
              }
              case 0: /* Left */ return $66 => IfuiServer_Promise_pure_Applicative_Promise({a1: $3f.a1}, $66);
             }
            };
            return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($3a => IfuiServer_RethinkDB_toArrayPtr($35.a1, $3a), $3e, $36);
           }
           case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $35.a1}, $36);
          }
         };
         return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($2f => IfuiServer_RethinkDB_runPtr($1b, $1, $2f), $34, $2b);
        }
        case 0: /* Left */ return IfuiServer_Promise_pure_Applicative_Promise({a1: $2a.a1}, $2b);
       }
      };
      return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($24 => IfuiServer_RethinkDB_runPtr($15, $1, $24), $29, $21);
     };
    }
    case 0: /* Left */ return $76 => IfuiServer_Promise_pure_Applicative_Promise({a1: $13.a1}, $76);
   }
  };
  return IfuiServer_Promise_x3ex3ex3d_Monad_Promise($d => IfuiServer_RethinkDB_runPtr($5, $1, $d), $12, $a);
 };
}

/* IfuiServer.RethinkDB.debugShowExpr : Expr db [] t -> String */
function IfuiServer_RethinkDB_debugShowExpr($0) {
 const $1 = IfuiServer_RethinkDB_prim__r(undefined);
 const $4 = IfuiServer_RethinkDB_compileExpr($1, {h: 0 /* Empty */}, $0);
 return IfuiServer_RethinkDB_prim__toString($4);
}

/* IfuiServer.RethinkDB.connect' : String -> Int -> ServerSchema ts -> Promise (RethinkServer ts) */
function IfuiServer_RethinkDB_connectx27($0, $1, $2, $3) {
 return IfuiServer_Promise_onErrPrint($6 => IfuiServer_RethinkDB_connect($0, $1, $2, $6), $3);
}

/* IfuiServer.RethinkDB.connect : String -> Int -> ServerSchema ts -> Promise (Either String (RethinkServer ts)) */
function IfuiServer_RethinkDB_connect($0, $1, $2, $3) {
 const $7 = $8 => {
  const $c = e => conn => {
   const $e = IfuiServer_RethinkDB_prim__errToStr(e);
   let $d;
   switch(Prelude_EqOrd_x3dx3d_Eq_String($e, '')) {
    case 1: {
     const $18 = IfuiServer_RethinkDB_doMigration($2, conn);
     const $17 = $18;
     const $1c = z => {
      switch(z.h) {
       case 0: /* nothing */ return $3({h: 1 /* Right */, a1: conn});
       case undefined: /* just */ return $3({h: 0 /* Left */, a1: z.a1});
      }
     };
     const $16 = $17($1c);
     $d = PrimIO_io_bind($16, _ => $25 => (undefined));
     break;
    }
    case 0: {
     $d = $3({h: 0 /* Left */, a1: $e});
     break;
    }
   }
   return $d;
  };
  return IfuiServer_RethinkDB_prim__connect($0, $1, $c, $8);
 };
 return Prelude_Interfaces_x3ex3e(csegen_90(), $7, () => $2b => $2c => (undefined));
}

/* IfuiServer.RethinkDB.<| : Expr db ctxt (a -> b) -> Expr db ctxt a -> Expr db ctxt b */
function IfuiServer_RethinkDB_x3cx7c($0, $1) {
 return {h: 2 /* App */, a1: $0, a2: $1};
}

/* Data.IORef.newIORef : HasIO io => a -> io (IORef a) */
function Data_IORef_newIORef($0, $1) {
 return $0.a1.a2(undefined)(undefined)($0.a2(undefined)($10 => ({value:$1})))(m => $0.a1.a1.a2(undefined)(m));
}

/* Prelude.Basics.flip : (a -> b -> c) -> b -> a -> c */
function Prelude_Basics_flip($0, $1, $2) {
 return $0($2)($1);
}

/* Builtin.snd : (a, b) -> b */
function Builtin_snd($0) {
 return $0.a2;
}

/* Builtin.idris_crash : String -> a */
function Builtin_idris_crash($0) {
 return _crashExp($0);
}

/* Builtin.fst : (a, b) -> a */
function Builtin_fst($0) {
 return $0.a1;
}

/* Prelude.Types.9932:9115:hexChars */
function Prelude_Types_n__9932_9115_hexChars($0) {
 return {a1: '0', a2: {a1: '1', a2: {a1: '2', a2: {a1: '3', a2: {a1: '4', a2: {a1: '5', a2: {a1: '6', a2: {a1: '7', a2: {a1: '8', a2: {a1: '9', a2: {a1: 'A', a2: {a1: 'B', a2: {a1: 'C', a2: {a1: 'D', a2: {a1: 'E', a2: {a1: 'F', a2: {h: 0}}}}}}}}}}}}}}}}};
}

/* Prelude.Types.traverse */
function Prelude_Types_traverse_Traversable_List($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return $0.a2(undefined)({h: 0});
  case undefined: /* cons */ return $0.a3(undefined)(undefined)($0.a3(undefined)(undefined)($0.a2(undefined)($1e => $1f => ({a1: $1e, a2: $1f})))($1($2.a1)))(Prelude_Types_traverse_Traversable_List($0, $1, $2.a2));
 }
}

/* Prelude.Types.rangeFromTo */
function Prelude_Types_rangeFromTo_Range_x24a($0, $1, $2) {
 const $4 = Builtin_fst(Builtin_snd($0));
 const $3 = $4.a2($1)($2);
 switch($3) {
  case 0: {
   const $e = $f => {
    const $10 = Builtin_fst(Builtin_snd($0));
    return $10.a6($f)($2);
   };
   const $1c = $1d => {
    const $1e = Builtin_snd(Builtin_snd($0));
    const $28 = Builtin_snd(Builtin_snd($0));
    const $27 = $28.a1.a3(1n);
    return $1e.a1.a1($1d)($27);
   };
   const $19 = Prelude_Types_countFrom($1, $1c);
   return Prelude_Types_takeUntil($e, $19);
  }
  case 1: return Prelude_Types_pure_Applicative_List($1);
  case 2: {
   const $33 = $34 => {
    const $35 = Builtin_fst(Builtin_snd($0));
    return $35.a5($34)($2);
   };
   const $41 = x => {
    const $42 = Builtin_snd(Builtin_snd($0));
    const $4b = Builtin_snd(Builtin_snd($0));
    const $4a = $4b.a1.a3(1n);
    return $42.a3(x)($4a);
   };
   const $3e = Prelude_Types_countFrom($1, $41);
   return Prelude_Types_takeUntil($33, $3e);
  }
 }
}

/* Prelude.Types.pure */
function Prelude_Types_pure_Applicative_List($0) {
 return {a1: $0, a2: {h: 0}};
}

/* Prelude.Types.null */
function Prelude_Types_null_Foldable_List($0) {
 switch($0.h) {
  case 0: /* nil */ return 1;
  case undefined: /* cons */ return 0;
 }
}

/* Prelude.Types.map */
function Prelude_Types_map_Functor_Maybe($0, $1) {
 switch($1.h) {
  case undefined: /* just */ return {a1: $0($1.a1)};
  case 0: /* nothing */ return {h: 0};
 }
}

/* Prelude.Types.foldr */
function Prelude_Types_foldr_Foldable_List($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return $1;
  case undefined: /* cons */ return $0($2.a1)(Prelude_Types_foldr_Foldable_List($0, $1, $2.a2));
 }
}

/* Prelude.Types.foldlM */
function Prelude_Types_foldlM_Foldable_List($0, $1, $2, $3) {
 return Prelude_Types_foldl_Foldable_List(ma => b => $0.a2(undefined)(undefined)(ma)($f => Prelude_Basics_flip($1, b, $f)), $0.a1.a2(undefined)($2), $3);
}

/* Prelude.Types.foldMap */
function Prelude_Types_foldMap_Foldable_List($0, $1, $2) {
 const $4 = acc => elem => {
  const $7 = $0.a1;
  const $6 = $9 => $a => $7($9)($a);
  const $5 = $6(acc);
  return $5($1(elem));
 };
 return Prelude_Types_foldl_Foldable_List($4, $0.a2, $2);
}

/* Prelude.Types.>>= */
function Prelude_Types_x3ex3ex3d_Monad_Maybe($0, $1) {
 switch($0.h) {
  case 0: /* nothing */ return {h: 0};
  case undefined: /* just */ return $1($0.a1);
 }
}

/* Prelude.Types.<|> */
function Prelude_Types_x3cx7cx3e_Alternative_Maybe($0, $1) {
 switch($0.h) {
  case undefined: /* just */ return {a1: $0.a1};
  case 0: /* nothing */ return $1();
 }
}

/* Prelude.Types.<*> */
function Prelude_Types_x3cx2ax3e_Applicative_Maybe($0, $1) {
 switch($0.h) {
  case undefined: /* just */ {
   switch($1.h) {
    case undefined: /* just */ return {a1: $0.a1($1.a1)};
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Prelude.Types.toUpper : Char -> Char */
function Prelude_Types_toUpper($0) {
 switch(Prelude_Types_isLower($0)) {
  case 1: return _truncToChar(_sub32s(_truncInt32($0.codePointAt(0)), 32));
  case 0: return $0;
 }
}

/* Prelude.Types.takeUntil : (n -> Bool) -> Stream n -> List n */
function Prelude_Types_takeUntil($0, $1) {
 switch($0($1.a1)) {
  case 1: return {a1: $1.a1, a2: {h: 0}};
  case 0: return {a1: $1.a1, a2: Prelude_Types_takeUntil($0, $1.a2())};
 }
}

/* Prelude.Types.List.tailRecAppend : List a -> List a -> List a */
function Prelude_Types_List_tailRecAppend($0, $1) {
 return Prelude_Types_List_reverseOnto($1, Prelude_Types_List_reverse($0));
}

/* Prelude.Types.List.reverse : List a -> List a */
function Prelude_Types_List_reverse($0) {
 return Prelude_Types_List_reverseOnto({h: 0}, $0);
}

/* Prelude.Types.prim__integerToNat : Integer -> Nat */
function Prelude_Types_prim__integerToNat($0) {
 let $1;
 switch(((0n<=$0)?1:0)) {
  case 0: {
   $1 = 0;
   break;
  }
  default: $1 = 1;
 }
 switch($1) {
  case 1: return $0;
  case 0: return 0n;
 }
}

/* Prelude.Types.maybe : Lazy b -> Lazy (a -> b) -> Maybe a -> b */
function Prelude_Types_maybe($0, $1, $2) {
 switch($2.h) {
  case 0: /* nothing */ return $0();
  case undefined: /* just */ return $1()($2.a1);
 }
}

/* Prelude.Types.listBind : List a -> (a -> List b) -> List b */
function Prelude_Types_listBind($0, $1) {
 return Prelude_Types_listBindOnto($1, {h: 0}, $0);
}

/* Prelude.Types.List.lengthTR : List a -> Nat */
function Prelude_Types_List_lengthTR($0) {
 return Prelude_Types_List_lengthPlus(0n, $0);
}

/* Prelude.Types.String.length : String -> Nat */
function Prelude_Types_String_length($0) {
 return Prelude_Types_prim__integerToNat(BigInt($0.length));
}

/* Prelude.Types.isSpace : Char -> Bool */
function Prelude_Types_isSpace($0) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, ' ')) {
  case 1: return 1;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, '\u{9}')) {
    case 1: return 1;
    case 0: {
     switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, '\r')) {
      case 1: return 1;
      case 0: {
       switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, '\n')) {
        case 1: return 1;
        case 0: {
         switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, '\u{c}')) {
          case 1: return 1;
          case 0: {
           switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, '\u{b}')) {
            case 1: return 1;
            case 0: return Prelude_EqOrd_x3dx3d_Eq_Char($0, '\u{a0}');
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}

/* Prelude.Types.isLower : Char -> Bool */
function Prelude_Types_isLower($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, 'a')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, 'z');
  case 0: return 0;
 }
}

/* Prelude.Types.isHexDigit : Char -> Bool */
function Prelude_Types_isHexDigit($0) {
 return Prelude_Types_elem(csegen_107(), csegen_214(), Prelude_Types_toUpper($0), Prelude_Types_n__9932_9115_hexChars($0));
}

/* Prelude.Types.isDigit : Char -> Bool */
function Prelude_Types_isDigit($0) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '0')) {
  case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, '9');
  case 0: return 0;
 }
}

/* Prelude.Types.isControl : Char -> Bool */
function Prelude_Types_isControl($0) {
 let $1;
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '\0')) {
  case 1: {
   $1 = Prelude_EqOrd_x3cx3d_Ord_Char($0, '\u{1f}');
   break;
  }
  case 0: {
   $1 = 0;
   break;
  }
 }
 switch($1) {
  case 1: return 1;
  case 0: {
   switch(Prelude_EqOrd_x3ex3d_Ord_Char($0, '\u{7f}')) {
    case 1: return Prelude_EqOrd_x3cx3d_Ord_Char($0, '\u{9f}');
    case 0: return 0;
   }
  }
 }
}

/* Prelude.Types.elemBy : Foldable t => (a -> a -> Bool) -> a -> t a -> Bool */
function Prelude_Types_elemBy($0, $1, $2, $3) {
 return Prelude_Interfaces_any($0, $1($2), $3);
}

/* Prelude.Types.elem : Foldable t => Eq a => a -> t a -> Bool */
function Prelude_Types_elem($0, $1, $2, $3) {
 return Prelude_Types_elemBy($0, $7 => $8 => $1.a1($7)($8), $2, $3);
}

/* Prelude.Types.countFrom : n -> (n -> n) -> Stream n */
function Prelude_Types_countFrom($0, $1) {
 return {a1: $0, a2: () => Prelude_Types_countFrom($1($0), $1)};
}

/* Prelude.Num.mod */
function Prelude_Num_mod_Integral_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($1, Number(_truncBigInt32(0n)))) {
  case 0: return _mod($0, $1);
  default: return Builtin_idris_crash(undefined)('Unhandled input for Prelude.Num.case block in mod at Prelude.Num:131:3--133:40');
 }
}

/* Prelude.Num.div */
function Prelude_Num_div_Integral_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($1, Number(_truncBigInt32(0n)))) {
  case 0: return _div32s($0, $1);
  default: return Builtin_idris_crash(undefined)('Unhandled input for Prelude.Num.case block in div at Prelude.Num:128:3--130:40');
 }
}

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Int($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_Char($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Char($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.min */
function Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch(Prelude_EqOrd_x3c_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3)) {
  case 1: return $2;
  case 0: return $3;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Int($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_Char($0, $1) {
 switch(Prelude_EqOrd_x3e_Ord_Char($0, $1)) {
  case 1: return $0;
  case 0: return $1;
 }
}

/* Prelude.EqOrd.max */
function Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch(Prelude_EqOrd_x3e_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3)) {
  case 1: return $2;
  case 0: return $3;
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_Integer($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Integer($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_Int($0, $1) {
 switch(Prelude_EqOrd_x3c_Ord_Int($0, $1)) {
  case 1: return 0;
  case 0: {
   switch(Prelude_EqOrd_x3dx3d_Eq_Int($0, $1)) {
    case 1: return 1;
    case 0: return 2;
   }
  }
 }
}

/* Prelude.EqOrd.compare */
function Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 switch($0.a1.a2($2.a1)($3.a1)) {
  case 1: return $0.a2($2.a1)($3.a1);
  case 0: return $1.a2($2.a2)($3.a2);
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Int($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_Char($0, $1) {
 switch((($0>$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.> */
function Prelude_EqOrd_x3e_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 return Prelude_EqOrd_x3dx3d_Eq_Ordering(Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3), 2);
}

/* Prelude.EqOrd.>= */
function Prelude_EqOrd_x3ex3d_Ord_Int($0, $1) {
 switch((($0>=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.>= */
function Prelude_EqOrd_x3ex3d_Ord_Char($0, $1) {
 switch((($0>=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_String($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 2: {
   switch($1) {
    case 2: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Integer($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Int($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Char($0, $1) {
 switch((($0===$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.== */
function Prelude_EqOrd_x3dx3d_Eq_Bool($0, $1) {
 switch($0) {
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Integer($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Int($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_Char($0, $1) {
 switch((($0<$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.< */
function Prelude_EqOrd_x3c_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3) {
 return Prelude_EqOrd_x3dx3d_Eq_Ordering(Prelude_EqOrd_compare_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29($0, $1, $2, $3), 0);
}

/* Prelude.EqOrd.<= */
function Prelude_EqOrd_x3cx3d_Ord_Int($0, $1) {
 switch((($0<=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd.<= */
function Prelude_EqOrd_x3cx3d_Ord_Char($0, $1) {
 switch((($0<=$1)?1:0)) {
  case 0: return 0;
  default: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_String($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_String($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Ordering($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Ordering($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Int($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Int($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd./= */
function Prelude_EqOrd_x2fx3d_Eq_Char($0, $1) {
 switch(Prelude_EqOrd_x3dx3d_Eq_Char($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Prelude.EqOrd.compareInteger : Integer -> Integer -> Ordering */
function Prelude_EqOrd_compareInteger($0, $1) {
 return Prelude_EqOrd_compare_Ord_Integer($0, $1);
}

/* Prelude.Interfaces.Bool.Semigroup.<+> */
function Prelude_Interfaces_Bool_Semigroup_x3cx2bx3e_Semigroup_AnyBool($0, $1) {
 switch($0) {
  case 1: return 1;
  case 0: return $1;
 }
}

/* Prelude.Interfaces.sequence : Applicative f => Traversable t => t (f a) -> f (t a) */
function Prelude_Interfaces_sequence($0, $1, $2) {
 return $1.a3(undefined)(undefined)(undefined)($0)($f => $f)($2);
}

/* Prelude.Interfaces.concatMap : Monoid m => Foldable t => (a -> m) -> t a -> m */
function Prelude_Interfaces_concatMap($0, $1, $2, $3) {
 return $1.a6(undefined)(undefined)($0)($2)($3);
}

/* Prelude.Interfaces.concat : Monoid a => Foldable t => t a -> a */
function Prelude_Interfaces_concat($0, $1, $2) {
 return $1.a6(undefined)(undefined)($0)($d => $d)($2);
}

/* Prelude.Interfaces.any : Foldable t => (a -> Bool) -> t a -> Bool */
function Prelude_Interfaces_any($0, $1, $2) {
 return $0.a6(undefined)(undefined)({a1: $d => $e => Prelude_Interfaces_Bool_Semigroup_x3cx2bx3e_Semigroup_AnyBool($d, $e), a2: 0})($1)($2);
}

/* Prelude.Interfaces.>> : Monad m => m () -> Lazy (m b) -> m b */
function Prelude_Interfaces_x3ex3e($0, $1, $2) {
 return $0.a2(undefined)(undefined)($1)($c => $2());
}

/* Prelude.Interfaces.<$> : Functor f => (a -> b) -> f a -> f b */
function Prelude_Interfaces_x3cx24x3e($0, $1, $2) {
 const $5 = $0;
 const $4 = $6 => $7 => $5(undefined)(undefined)($6)($7);
 const $3 = $4($1);
 return $3($2);
}

/* Prelude.Show.2410:11407:asciiTab */
function Prelude_Show_n__2410_11407_asciiTab($0) {
 return {a1: 'NUL', a2: {a1: 'SOH', a2: {a1: 'STX', a2: {a1: 'ETX', a2: {a1: 'EOT', a2: {a1: 'ENQ', a2: {a1: 'ACK', a2: {a1: 'BEL', a2: {a1: 'BS', a2: {a1: 'HT', a2: {a1: 'LF', a2: {a1: 'VT', a2: {a1: 'FF', a2: {a1: 'CR', a2: {a1: 'SO', a2: {a1: 'SI', a2: {a1: 'DLE', a2: {a1: 'DC1', a2: {a1: 'DC2', a2: {a1: 'DC3', a2: {a1: 'DC4', a2: {a1: 'NAK', a2: {a1: 'SYN', a2: {a1: 'ETB', a2: {a1: 'CAN', a2: {a1: 'EM', a2: {a1: 'SUB', a2: {a1: 'ESC', a2: {a1: 'FS', a2: {a1: 'GS', a2: {a1: 'RS', a2: {a1: 'US', a2: {h: 0}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}};
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_String($0) {
 return ('\"'+Prelude_Show_showLitString(Prelude_Types_fastUnpack($0))('\"'));
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Int($0) {
 return Prelude_Show_showPrec_Show_Int({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_Double($0) {
 return Prelude_Show_showPrec_Show_Double({h: 0 /* Open */}, $0);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_x28Maybex20x24ax29($0, $1) {
 return Prelude_Show_showPrec_Show_x28Maybex20x24ax29($0, {h: 0 /* Open */}, $1);
}

/* Prelude.Show.show */
function Prelude_Show_show_Show_x28Listx20x24ax29($0, $1) {
 return ('['+(Prelude_Show_n__3192_12128_showx27($0, $1, '', $1)+']'));
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_String($0, $1) {
 return Prelude_Show_show_Show_String($1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Int($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_Double($0, $1) {
 return Prelude_Show_primNumShow($4 => (''+$4), $0, $1);
}

/* Prelude.Show.showPrec */
function Prelude_Show_showPrec_Show_x28Maybex20x24ax29($0, $1, $2) {
 switch($2.h) {
  case 0: /* nothing */ return 'Nothing';
  case undefined: /* just */ return Prelude_Show_showCon($1, 'Just', Prelude_Show_showArg($0, $2.a1));
 }
}

/* Prelude.Show.compare */
function Prelude_Show_compare_Ord_Prec($0, $1) {
 switch($0.h) {
  case 4: /* User */ {
   switch($1.h) {
    case 4: /* User */ return Prelude_EqOrd_compare_Ord_Integer($0.a1, $1.a1);
    default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
   }
  }
  default: return Prelude_EqOrd_compare_Ord_Integer(Prelude_Show_precCon($0), Prelude_Show_precCon($1));
 }
}

/* Prelude.Show.>= */
function Prelude_Show_x3ex3d_Ord_Prec($0, $1) {
 return Prelude_EqOrd_x2fx3d_Eq_Ordering(Prelude_Show_compare_Ord_Prec($0, $1), 0);
}

/* Prelude.Show.showParens : Bool -> String -> String */
function Prelude_Show_showParens($0, $1) {
 switch($0) {
  case 0: return $1;
  case 1: return ('('+($1+')'));
 }
}

/* Prelude.Show.showLitString : List Char -> String -> String */
function Prelude_Show_showLitString($0) {
 return $1 => {
  switch($0.h) {
   case 0: /* nil */ return $1;
   case undefined: /* cons */ {
    switch($0.a1) {
     case '\"': return ('\u{5c}\"'+Prelude_Show_showLitString($0.a2)($1));
     default: return Prelude_Show_showLitChar($0.a1)(Prelude_Show_showLitString($0.a2)($1));
    }
   }
  }
 };
}

/* Prelude.Show.showLitChar : Char -> String -> String */
function Prelude_Show_showLitChar($0) {
 switch($0) {
  case '\u{7}': return $2 => ('\u{5c}a'+$2);
  case '\u{8}': return $5 => ('\u{5c}b'+$5);
  case '\u{c}': return $8 => ('\u{5c}f'+$8);
  case '\n': return $b => ('\u{5c}n'+$b);
  case '\r': return $e => ('\u{5c}r'+$e);
  case '\u{9}': return $11 => ('\u{5c}t'+$11);
  case '\u{b}': return $14 => ('\u{5c}v'+$14);
  case '\u{e}': return $17 => Prelude_Show_protectEsc($1a => Prelude_EqOrd_x3dx3d_Eq_Char($1a, 'H'), '\u{5c}SO', $17);
  case '\u{7f}': return $20 => ('\u{5c}DEL'+$20);
  case '\u{5c}': return $23 => ('\u{5c}\u{5c}'+$23);
  default: {
   return $26 => {
    const $27 = Prelude_Types_getAt(Prelude_Types_prim__integerToNat(BigInt($0.codePointAt(0))), Prelude_Show_n__2410_11407_asciiTab($0));
    switch($27.h) {
     case undefined: /* just */ return ('\u{5c}'+($27.a1+$26));
     case 0: /* nothing */ {
      switch(Prelude_EqOrd_x3e_Ord_Char($0, '\u{7f}')) {
       case 1: return ('\u{5c}'+Prelude_Show_protectEsc($3c => Prelude_Types_isDigit($3c), Prelude_Show_show_Show_Int(_truncInt32($0.codePointAt(0))), $26));
       case 0: return ($0+$26);
      }
     }
    }
   };
  }
 }
}

/* Prelude.Show.showCon : Prec -> String -> String -> String */
function Prelude_Show_showCon($0, $1, $2) {
 return Prelude_Show_showParens(Prelude_Show_x3ex3d_Ord_Prec($0, {h: 6 /* App */}), ($1+$2));
}

/* Prelude.Show.showArg : Show a => a -> String */
function Prelude_Show_showArg($0, $1) {
 return (' '+$0.a2({h: 6 /* App */})($1));
}

/* Prelude.Show.protectEsc : (Char -> Bool) -> String -> String -> String */
function Prelude_Show_protectEsc($0, $1, $2) {
 let $5;
 switch(Prelude_Show_firstCharIs($0, $2)) {
  case 1: {
   $5 = '\u{5c}&';
   break;
  }
  case 0: {
   $5 = '';
   break;
  }
 }
 const $4 = ($5+$2);
 return ($1+$4);
}

/* Prelude.Show.primNumShow : (a -> String) -> Prec -> a -> String */
function Prelude_Show_primNumShow($0, $1, $2) {
 const $3 = $0($2);
 let $7;
 switch(Prelude_Show_x3ex3d_Ord_Prec($1, {h: 5 /* PrefixMinus */})) {
  case 1: {
   $7 = Prelude_Show_firstCharIs($e => Prelude_EqOrd_x3dx3d_Eq_Char($e, '-'), $3);
   break;
  }
  case 0: {
   $7 = 0;
   break;
  }
 }
 return Prelude_Show_showParens($7, $3);
}

/* Prelude.Show.precCon : Prec -> Integer */
function Prelude_Show_precCon($0) {
 switch($0.h) {
  case 0: /* Open */ return 0n;
  case 1: /* Equal */ return 1n;
  case 2: /* Dollar */ return 2n;
  case 3: /* Backtick */ return 3n;
  case 4: /* User */ return 4n;
  case 5: /* PrefixMinus */ return 5n;
  case 6: /* App */ return 6n;
 }
}

/* Prelude.Show.firstCharIs : (Char -> Bool) -> String -> Bool */
function Prelude_Show_firstCharIs($0, $1) {
 switch($1) {
  case '': return 0;
  default: return $0(($1.charAt(0)));
 }
}

/* Prelude.IO.map */
function Prelude_IO_map_Functor_IO($0, $1) {
 return PrimIO_io_bind($1, $5 => $6 => $0($5));
}

/* Prelude.IO.putStrLn : HasIO io => String -> io () */
function Prelude_IO_putStrLn($0, $1) {
 return Prelude_IO_putStr($0, ($1+'\n'));
}

/* Prelude.IO.putStr : HasIO io => String -> io () */
function Prelude_IO_putStr($0, $1) {
 return $0.a2(undefined)($7 => Prelude_IO_prim__putStr($1, $7));
}

/* PrimIO.case block in io_bind */
function PrimIO_case__io_bind_933($0, $1) {
 const $2 = $1;
 const $3 = $0($2);
 return $3(undefined);
}

/* PrimIO.unsafePerformIO : IO a -> a */
function PrimIO_unsafePerformIO($0) {
 const $1 = $0;
 const $3 = w => {
  const $4 = $1(w);
  return $4;
 };
 return PrimIO_unsafeCreateWorld($3);
}

/* PrimIO.unsafeCreateWorld : (1 _ : ((1 _ : %World) -> a)) -> a */
function PrimIO_unsafeCreateWorld($0) {
 return $0(_idrisworld);
}

/* PrimIO.io_bind : (1 _ : IO a) -> (1 _ : (a -> IO b)) -> IO b */
function PrimIO_io_bind($0, $1) {
 const $2 = $0;
 return w => PrimIO_case__io_bind_933($1, $2(w));
}

/* Ifui.Json.toListJson */
function Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20Nilx29($0) {
 return {h: 0};
}

/* Ifui.Json.toListJson */
function Ifui_Json_toListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29($0, $1, $2, $3) {
 const $5 = $3.a2;
 const $9 = Builtin_fst($2);
 const $8 = $9.a1($5);
 const $6 = {a1: $0, a2: $8};
 const $f = Builtin_snd($2);
 const $e = $f.a1($3.a3);
 return {a1: $6, a2: $e};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_String($0) {
 return {h: 3 /* JString */, a1: $0};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_Int($0) {
 return {h: 2 /* JNumber */, a1: $0};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_Bool($0) {
 return {h: 1 /* JBoolean */, a1: $0};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($0) {
 return {h: 0 /* JNull */};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_x28Maybex20x24ax29($0, $1) {
 switch($1.h) {
  case 0: /* nothing */ return {h: 0 /* JNull */};
  case undefined: /* just */ return $0.a1($1.a1);
 }
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_x28Listx20x24ax29($0, $1) {
 return {h: 4 /* JArray */, a1: csegen_156()($7 => $0.a1($7))($1)};
}

/* Ifui.Json.toJson */
function Ifui_Json_toJson_JsonSerializable_x24a($0, $1) {
 return {h: 5 /* JObject */, a1: $0.a1($1)};
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_String($0) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_String($0));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_Int($0) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_Int($0));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_Bool($0) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_Bool($0));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($0) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($0));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_x28Maybex20x24ax29($0, $1) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_x28Maybex20x24ax29($0, $1));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_x28Listx20x24ax29($0, $1) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_x28Listx20x24ax29($0, $1));
}

/* Ifui.Json.stringify */
function Ifui_Json_stringify_JsonSerializable_x24a($0, $1) {
 return Language_JSON_Data_show_Show_JSON(Ifui_Json_toJson_JsonSerializable_x24a($0, $1));
}

/* Ifui.Json.fromListJson */
function Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20Nilx29($0) {
 switch($0.h) {
  case 0: /* nil */ return {a1: {h: 0 /* Nil */}};
  default: return {h: 0};
 }
}

/* Ifui.Json.fromListJson */
function Ifui_Json_fromListJson_JsonObjectSerializable_x28Recordx20x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24sx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24sx29x20x24tx29x7cx29x29x20x24tsx29x29($0, $1, $2, $3) {
 switch($3.h) {
  case 0: /* nil */ return {h: 0};
  default: {
   const $7 = Builtin_snd($2);
   const $6 = $7.a2($3);
   const $c = ws => {
    const $14 = j => {
     const $17 = Builtin_fst($2);
     const $16 = $17.a2(j);
     return Prelude_Types_x3ex3ex3d_Monad_Maybe($16, w => ({a1: {h: 1 /* :: */, a1: $1, a2: w, a3: ws}}));
    };
    return Prelude_Types_x3ex3ex3d_Monad_Maybe(Data_List_lookup(csegen_166(), $0, $3), $14);
   };
   return Prelude_Types_x3ex3ex3d_Monad_Maybe($6, $c);
  }
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_String($0) {
 switch($0.h) {
  case 3: /* JString */ return {a1: $0.a1};
  default: return {h: 0};
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_Int($0) {
 switch($0.h) {
  case 2: /* JNumber */ return {a1: _truncInt32(Math.trunc($0.a1))};
  default: return {h: 0};
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_Bool($0) {
 switch($0.h) {
  case 1: /* JBoolean */ return {a1: $0.a1};
  default: return {h: 0};
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_x28x7cUnitx2cMkUnitx7cx29($0) {
 return {a1: undefined};
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_x28Maybex20x24ax29($0, $1) {
 switch($1.h) {
  case 0: /* JNull */ return {a1: {h: 0}};
  default: return Prelude_Interfaces_x3cx24x3e(csegen_112(), $8 => ({a1: $8}), $0.a2($1));
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_x28Listx20x24ax29($0, $1) {
 switch($1.h) {
  case 4: /* JArray */ return Prelude_Interfaces_sequence(csegen_119(), csegen_127(), csegen_156()($d => $0.a2($d))($1.a1));
  default: return {h: 0};
 }
}

/* Ifui.Json.fromJson */
function Ifui_Json_fromJson_JsonSerializable_x24a($0, $1) {
 switch($1.h) {
  case 5: /* JObject */ return $0.a2($1.a1);
  default: return {h: 0};
 }
}

/* Data.List.span : (a -> Bool) -> List a -> (List a, List a) */
function Data_List_span($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {a1: {h: 0}, a2: {h: 0}};
  case undefined: /* cons */ {
   switch($0($1.a1)) {
    case 1: {
     const $8 = Data_List_span($0, $1.a2);
     return {a1: {a1: $1.a1, a2: $8.a1}, a2: $8.a2};
    }
    case 0: return {a1: {h: 0}, a2: {a1: $1.a1, a2: $1.a2}};
   }
  }
 }
}

/* Data.List.lookup : Eq a => a -> List (a, b) -> Maybe b */
function Data_List_lookup($0, $1, $2) {
 return Data_List_lookupBy($5 => $6 => $0.a1($5)($6), $1, $2);
}

/* Data.List.isNil : List a -> Bool */
function Data_List_isNil($0) {
 switch($0.h) {
  case 0: /* nil */ return 1;
  default: return 0;
 }
}

/* Data.List.head' : List a -> Maybe a */
function Data_List_headx27($0) {
 switch($0.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ return {a1: $0.a1};
 }
}

/* Data.List.deleteBy : (a -> b -> Bool) -> a -> List b -> List b */
function Data_List_deleteBy($0, $1, $2) {
 switch($2.h) {
  case 0: /* nil */ return {h: 0};
  case undefined: /* cons */ {
   switch($0($1)($2.a1)) {
    case 1: return $2.a2;
    case 0: return {a1: $2.a1, a2: Data_List_deleteBy($0, $1, $2.a2)};
   }
  }
 }
}

/* Data.Maybe.isJust : Maybe a -> Bool */
function Data_Maybe_isJust($0) {
 switch($0.h) {
  case 0: /* nothing */ return 0;
  case undefined: /* just */ return 1;
 }
}

/* Data.Maybe.fromMaybe : Lazy a -> Maybe a -> a */
function Data_Maybe_fromMaybe($0, $1) {
 switch($1.h) {
  case 0: /* nothing */ return $0();
  case undefined: /* just */ return $1.a1;
 }
}

/* Data.List1.forget : List1 a -> List a */
function Data_List1_forget($0) {
 return {a1: $0.a1, a2: $0.a2};
}

/* Data.List1.appendl : List1 a -> List a -> List1 a */
function Data_List1_appendl($0, $1) {
 return {a1: $0.a1, a2: Prelude_Types_List_tailRecAppend($0.a2, $1)};
}

/* Data.List1.++ : List1 a -> List1 a -> List1 a */
function Data_List1_x2bx2b($0, $1) {
 return Data_List1_appendl($0, Data_List1_forget($1));
}

/* Ifui.ExtensibleRecords.hasValue */
function Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x28x7cx28x28Builtinx2ePairx20x24kx29x20x24tx29x2cx28x28Builtinx2eMkPairx20x24kx29x20x24tx29x7cx29x29x20x24tsx29($0) {
 return {h: 0 /* Here */, a1: $0};
}

/* Ifui.ExtensibleRecords.hasValue */
function Ifui_ExtensibleRecords_hasValue_HasValue_x24k_x24t_x28x28x3ax3ax20x24ox29x20x24tsx29($0, $1) {
 const $4 = $1;
 const $3 = $4;
 return {h: 1 /* There */, a1: $0, a2: $3};
}

/* Ifui.ExtensibleRecords.strNotEq : So (a /= b) => Not (a = b) */
function Ifui_ExtensibleRecords_strNotEq($0, $1, $2, $3) {
 return $2;
}

/* Ifui.ExtensibleRecords.get : (k : String) -> HasValue k t ts => Record ts -> t */
function Ifui_ExtensibleRecords_get($0, $1, $2) {
 const $5 = $1;
 const $4 = $5;
 return Ifui_ExtensibleRecords_getAux($4, $2);
}

/* Language.JSON.parse : String -> Maybe JSON */
function Language_JSON_parse($0) {
 return Prelude_Types_x3ex3ex3d_Monad_Maybe(Language_JSON_Lexer_lexJSON($0), $6 => Language_JSON_Parser_parseJSON($6));
}

/* Text.Bounded.map */
function Text_Bounded_map_Functor_WithBounds($0, $1) {
 return {a1: $0($1.a1), a2: $1.a2, a3: $1.a3};
}

/* Text.Bounded.startBounds : Bounds -> (Int, Int) */
function Text_Bounded_startBounds($0) {
 return {a1: $0.a1, a2: $0.a2};
}

/* Text.Bounded.start : WithBounds ty -> (Int, Int) */
function Text_Bounded_start($0) {
 return Text_Bounded_startBounds($0.a3);
}

/* Text.Bounded.removeIrrelevance : WithBounds ty -> WithBounds ty */
function Text_Bounded_removeIrrelevance($0) {
 return {a1: $0.a1, a2: 1, a3: $0.a3};
}

/* Text.Bounded.mergeBounds : WithBounds ty -> WithBounds ty' -> WithBounds ty' */
function Text_Bounded_mergeBounds($0, $1) {
 switch($0.h) {
  case undefined: /* record */ {
   switch($0.a2) {
    case 1: {
     switch($1.h) {
      case undefined: /* record */ {
       switch($1.a2) {
        case 1: return Text_Bounded_irrelevantBounds($1.a1);
        default: return $1;
       }
      }
      default: return $1;
     }
    }
    default: {
     switch($1.h) {
      case undefined: /* record */ {
       switch($1.a2) {
        case 1: return Prelude_Interfaces_x3cx24x3e(csegen_255(), $e => $1.a1, $0);
        default: {
         const $10 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_start($0), Text_Bounded_start($1));
         const $1c = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_end($0), Text_Bounded_end($1));
         return {a1: $1.a1, a2: 0, a3: {a1: $10.a1, a2: $10.a2, a3: $1c.a1, a4: $1c.a2}};
        }
       }
      }
      default: {
       const $30 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_start($0), Text_Bounded_start($1));
       const $3c = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_end($0), Text_Bounded_end($1));
       return {a1: $1.a1, a2: 0, a3: {a1: $30.a1, a2: $30.a2, a3: $3c.a1, a4: $3c.a2}};
      }
     }
    }
   }
  }
  default: {
   switch($1.h) {
    case undefined: /* record */ {
     switch($1.a2) {
      case 1: return Prelude_Interfaces_x3cx24x3e(csegen_255(), $56 => $1.a1, $0);
      default: {
       const $58 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_start($0), Text_Bounded_start($1));
       const $64 = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_end($0), Text_Bounded_end($1));
       return {a1: $1.a1, a2: 0, a3: {a1: $58.a1, a2: $58.a2, a3: $64.a1, a4: $64.a2}};
      }
     }
    }
    default: {
     const $78 = Prelude_EqOrd_min_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_start($0), Text_Bounded_start($1));
     const $84 = Prelude_EqOrd_max_Ord_x28x7cx28x28Builtinx2ePairx20x24ax29x20x24bx29x2cx28x28Builtinx2eMkPairx20x24ax29x20x24bx29x7cx29(csegen_144(), csegen_144(), Text_Bounded_end($0), Text_Bounded_end($1));
     return {a1: $1.a1, a2: 0, a3: {a1: $78.a1, a2: $78.a2, a3: $84.a1, a4: $84.a2}};
    }
   }
  }
 }
}

/* Text.Bounded.irrelevantBounds : ty -> WithBounds ty */
function Text_Bounded_irrelevantBounds($0) {
 return {a1: $0, a2: 1, a3: {a1: -1, a2: -1, a3: -1, a4: -1}};
}

/* Text.Bounded.endBounds : Bounds -> (Int, Int) */
function Text_Bounded_endBounds($0) {
 return {a1: $0.a3, a2: $0.a4};
}

/* Text.Bounded.end : WithBounds ty -> (Int, Int) */
function Text_Bounded_end($0) {
 return Text_Bounded_endBounds($0.a3);
}

/* Language.JSON.Data.4720:7187:stringifyValues */
function Language_JSON_Data_n__4720_7187_stringifyValues($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return '';
  case undefined: /* cons */ {
   let $6;
   switch(Data_List_isNil($1.a2)) {
    case 1: {
     $6 = '';
     break;
    }
    case 0: {
     $6 = (','+Language_JSON_Data_n__4720_7187_stringifyValues($0, $1.a2));
     break;
    }
   }
   return (Language_JSON_Data_stringify($1.a1)+$6);
  }
 }
}

/* Language.JSON.Data.4720:7232:stringifyProps */
function Language_JSON_Data_n__4720_7232_stringifyProps($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return '';
  case undefined: /* cons */ {
   let $7;
   switch(Data_List_isNil($1.a2)) {
    case 1: {
     $7 = '';
     break;
    }
    case 0: {
     $7 = (','+Language_JSON_Data_n__4720_7232_stringifyProps($0, $1.a2));
     break;
    }
   }
   return (Language_JSON_Data_n__4720_7231_stringifyProp($0, $1.a1)+$7);
  }
 }
}

/* Language.JSON.Data.4720:7231:stringifyProp */
function Language_JSON_Data_n__4720_7231_stringifyProp($0, $1) {
 return (Language_JSON_Data_showString($1.a1)+(':'+Language_JSON_Data_stringify($1.a2)));
}

/* Language.JSON.Data.show */
function Language_JSON_Data_show_Show_JSON($0) {
 return Language_JSON_Data_stringify($0);
}

/* Language.JSON.Data.showPrec */
function Language_JSON_Data_showPrec_Show_JSON($0, $1) {
 return Language_JSON_Data_show_Show_JSON($1);
}

/* Language.JSON.Data.stringify : JSON -> String */
function Language_JSON_Data_stringify($0) {
 switch($0.h) {
  case 0: /* JNull */ return 'null';
  case 1: /* JBoolean */ {
   switch($0.a1) {
    case 1: return 'true';
    case 0: return 'false';
   }
  }
  case 2: /* JNumber */ return Prelude_Show_show_Show_Double($0.a1);
  case 3: /* JString */ return Language_JSON_Data_showString($0.a1);
  case 4: /* JArray */ return ('['+(Language_JSON_Data_n__4720_7187_stringifyValues($0.a1, $0.a1)+']'));
  case 5: /* JObject */ return ('{'+(Language_JSON_Data_n__4720_7232_stringifyProps($0.a1, $0.a1)+'}'));
 }
}

/* Language.JSON.Data.showString : String -> String */
function Language_JSON_Data_showString($0) {
 return ('\"'+(Prelude_Interfaces_concatMap(csegen_92(), csegen_107(), $a => Language_JSON_Data_showChar($a), Prelude_Types_fastUnpack($0))+'\"'));
}

/* Language.JSON.Data.showChar : Char -> String */
function Language_JSON_Data_showChar($0) {
 switch($0) {
  case '\u{8}': return '\u{5c}b';
  case '\u{c}': return '\u{5c}f';
  case '\n': return '\u{5c}n';
  case '\r': return '\u{5c}r';
  case '\u{9}': return '\u{5c}t';
  case '\u{5c}': return '\u{5c}\u{5c}';
  case '\"': return '\u{5c}\"';
  default: {
   let $2;
   switch(Prelude_Types_isControl($0)) {
    case 1: {
     $2 = 1;
     break;
    }
    case 0: {
     $2 = Prelude_EqOrd_x3ex3d_Ord_Char($0, csegen_108()('\u{7f}'));
     break;
    }
   }
   switch($2) {
    case 1: {
     const $c = Language_JSON_Data_b16ToHexString(_truncUInt16(_truncInt32($0.codePointAt(0))));
     return ('\u{5c}u'+Data_String_Extra_justifyRight(4n, '0', $c));
    }
    case 0: return Data_String_singleton($0);
   }
  }
 }
}

/* Language.JSON.Data.b16ToHexString : Bits16 -> String */
function Language_JSON_Data_b16ToHexString($0) {
 switch($0) {
  case 0: return '0';
  case 1: return '1';
  case 2: return '2';
  case 3: return '3';
  case 4: return '4';
  case 5: return '5';
  case 6: return '6';
  case 7: return '7';
  case 8: return '8';
  case 9: return '9';
  case 10: return 'A';
  case 11: return 'B';
  case 12: return 'C';
  case 13: return 'D';
  case 14: return 'E';
  case 15: return 'F';
  default: return (Language_JSON_Data_b16ToHexString(_shr16u($0, 4))+Language_JSON_Data_b16ToHexString(($0&15)));
 }
}

/* Data.String.singleton : Char -> String */
function Data_String_singleton($0) {
 return ($0+'');
}

/* Data.String.replicate : Nat -> Char -> String */
function Data_String_replicate($0, $1) {
 return Prelude_Types_fastPack(Data_List_replicateTR({h: 0}, $0, $1));
}

/* Data.String.Extra.justifyRight : Nat -> Char -> String -> String */
function Data_String_Extra_justifyRight($0, $1, $2) {
 return (Data_String_replicate(Prelude_Types_prim__integerToNat(($0-Prelude_Types_String_length($2))), $1)+$2);
}

/* Data.String.Extra.index : Nat -> String -> Maybe Char */
function Data_String_Extra_index($0, $1) {
 return Data_String_Extra_with__index_1618($1, Prelude_Types_fastUnpack($1), $0);
}

/* Language.JSON.Parser.3635:2407:values */
const Language_JSON_Parser_n__3635_2407_values = __lazy(function () {
 return Text_Parser_sepBy(1, Language_JSON_Parser_punct({h: 0 /* Comma */}), Language_JSON_Parser_json());
});

/* Language.JSON.Parser.3632:2316:properties */
const Language_JSON_Parser_n__3632_2316_properties = __lazy(function () {
 return Text_Parser_sepBy(1, Language_JSON_Parser_punct({h: 0 /* Comma */}), {h: 8 /* SeqEat */, a1: 1, a2: Language_JSON_Parser_rawString(), a3: () => key => ({h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 1 /* Colon */}), a3: () => ({h: 8 /* SeqEat */, a1: 0, a2: Language_JSON_Parser_json(), a3: () => value => ({h: 0 /* Empty */, a1: {a1: key, a2: value}})})})});
});

/* Language.JSON.Parser.string : Grammar state JSONToken True JSON */
const Language_JSON_Parser_string = __lazy(function () {
 return csegen_265()($4 => ({h: 3 /* JString */, a1: $4}))(Language_JSON_Parser_rawString());
});

/* Language.JSON.Parser.rawString : Grammar state JSONToken True String */
const Language_JSON_Parser_rawString = __lazy(function () {
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_match(csegen_269(), csegen_272(), {h: 2 /* JTString */}), a3: () => mstr => {
  switch(mstr.h) {
   case undefined: /* just */ return {h: 0 /* Empty */, a1: mstr.a1};
   case 0: /* nothing */ return {h: 4 /* Fail */, a1: {h: 0}, a2: 0, a3: csegen_108()('invalid string')};
  }
 }};
});

/* Language.JSON.Parser.punct : Punctuation -> Grammar state JSONToken True () */
function Language_JSON_Parser_punct($0) {
 return Text_Parser_match(csegen_269(), csegen_272(), {h: 4 /* JTPunct */, a1: $0});
}

/* Language.JSON.Parser.parseJSON : List (WithBounds JSONToken) -> Maybe JSON */
function Language_JSON_Parser_parseJSON($0) {
 const $9 = $a => {
  switch(Language_JSON_Tokens_ignored($a)) {
   case 1: return 0;
   case 0: return 1;
  }
 };
 const $6 = Prelude_Types_List_filterAppend({h: 0}, $9, $0);
 const $1 = Text_Parser_Core_parse(1, Language_JSON_Parser_json(), $6);
 switch($1.h) {
  case 1: /* Right */ {
   switch($1.a1.h) {
    case undefined: /* cons */ {
     switch($1.a1.a2.h) {
      case 0: /* nil */ return {a1: $1.a1.a1};
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Language.JSON.Parser.object : Grammar state JSONToken True JSON */
const Language_JSON_Parser_object = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 3 /* Curly */, a1: 0}), a3: () => ({h: 11 /* ThenEmpty */, a1: 0, a2: 1, a3: {h: 6 /* Commit */}, a4: {h: 9 /* SeqEmpty */, a1: 0, a2: 1, a3: Language_JSON_Parser_n__3632_2316_properties(), a4: props => ({h: 10 /* ThenEat */, a1: 0, a2: Language_JSON_Parser_punct({h: 3 /* Curly */, a1: 1}), a3: () => ({h: 0 /* Empty */, a1: {h: 5 /* JObject */, a1: props}})})}})};
});

/* Language.JSON.Parser.number : Grammar state JSONToken True JSON */
const Language_JSON_Parser_number = __lazy(function () {
 return csegen_265()($4 => ({h: 2 /* JNumber */, a1: $4}))(Text_Parser_match(csegen_269(), csegen_272(), {h: 1 /* JTNumber */}));
});

/* Language.JSON.Parser.null : Grammar state JSONToken True JSON */
const Language_JSON_Parser_null$ = __lazy(function () {
 return csegen_265()($4 => ({h: 0 /* JNull */}))(Text_Parser_match(csegen_269(), csegen_272(), {h: 3 /* JTNull */}));
});

/* Language.JSON.Parser.json : Grammar state JSONToken True JSON */
const Language_JSON_Parser_json = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_object(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_array(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_string(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_boolean(), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Language_JSON_Parser_number(), a4: () => Language_JSON_Parser_null$()})})})})};
});

/* Language.JSON.Parser.boolean : Grammar state JSONToken True JSON */
const Language_JSON_Parser_boolean = __lazy(function () {
 return csegen_265()($4 => ({h: 1 /* JBoolean */, a1: $4}))(Text_Parser_match(csegen_269(), csegen_272(), {h: 0 /* JTBoolean */}));
});

/* Language.JSON.Parser.array : Grammar state JSONToken True JSON */
const Language_JSON_Parser_array = __lazy(function () {
 return {h: 10 /* ThenEat */, a1: 1, a2: Language_JSON_Parser_punct({h: 2 /* Square */, a1: 0}), a3: () => ({h: 11 /* ThenEmpty */, a1: 0, a2: 1, a3: {h: 6 /* Commit */}, a4: {h: 9 /* SeqEmpty */, a1: 0, a2: 1, a3: Language_JSON_Parser_n__3635_2407_values(), a4: vals => ({h: 10 /* ThenEat */, a1: 0, a2: Language_JSON_Parser_punct({h: 2 /* Square */, a1: 1}), a3: () => ({h: 0 /* Empty */, a1: {h: 4 /* JArray */, a1: vals}})})}})};
});

/* Language.JSON.Tokens.tokValue */
function Language_JSON_Tokens_tokValue_TokenKind_JSONTokenKind($0, $1) {
 switch($0.h) {
  case 0: /* JTBoolean */ return Prelude_EqOrd_x3dx3d_Eq_String($1, 'true');
  case 1: /* JTNumber */ return _numberOfString($1);
  case 2: /* JTString */ return Language_JSON_String_stringValue($1);
  case 3: /* JTNull */ return undefined;
  case 4: /* JTPunct */ return undefined;
  case 5: /* JTIgnore */ return undefined;
 }
}

/* Language.JSON.Tokens.TokType */
function Language_JSON_Tokens_TokType_TokenKind_JSONTokenKind($0) {
 switch($0.h) {
  case 0: /* JTBoolean */ return {h: 'Prelude.Basics.Bool'};
  case 1: /* JTNumber */ return {h: 'Double'};
  case 2: /* JTString */ return {h: 'Prelude.Types.Maybe', a1: {h: 'String'}};
  case 3: /* JTNull */ return {h: 'Builtin.Unit'};
  case 4: /* JTPunct */ return {h: 'Builtin.Unit'};
  case 5: /* JTIgnore */ return {h: 'Builtin.Unit'};
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_Punctuation($0, $1) {
 switch($0.h) {
  case 0: /* Comma */ {
   switch($1.h) {
    case 0: /* Comma */ return 1;
    default: return 0;
   }
  }
  case 1: /* Colon */ {
   switch($1.h) {
    case 1: /* Colon */ return 1;
    default: return 0;
   }
  }
  case 2: /* Square */ {
   switch($1.h) {
    case 2: /* Square */ return Language_JSON_Tokens_x3dx3d_Eq_Bracket($0.a1, $1.a1);
    default: return 0;
   }
  }
  case 3: /* Curly */ {
   switch($1.h) {
    case 3: /* Curly */ return Language_JSON_Tokens_x3dx3d_Eq_Bracket($0.a1, $1.a1);
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($0, $1) {
 switch($0.h) {
  case 0: /* JTBoolean */ {
   switch($1.h) {
    case 0: /* JTBoolean */ return 1;
    default: return 0;
   }
  }
  case 1: /* JTNumber */ {
   switch($1.h) {
    case 1: /* JTNumber */ return 1;
    default: return 0;
   }
  }
  case 2: /* JTString */ {
   switch($1.h) {
    case 2: /* JTString */ return 1;
    default: return 0;
   }
  }
  case 3: /* JTNull */ {
   switch($1.h) {
    case 3: /* JTNull */ return 1;
    default: return 0;
   }
  }
  case 4: /* JTPunct */ {
   switch($1.h) {
    case 4: /* JTPunct */ return Language_JSON_Tokens_x3dx3d_Eq_Punctuation($0.a1, $1.a1);
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.Tokens.== */
function Language_JSON_Tokens_x3dx3d_Eq_Bracket($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.Tokens./= */
function Language_JSON_Tokens_x2fx3d_Eq_JSONTokenKind($0, $1) {
 switch(Language_JSON_Tokens_x3dx3d_Eq_JSONTokenKind($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Language.JSON.Tokens.ignored : WithBounds JSONToken -> Bool */
function Language_JSON_Tokens_ignored($0) {
 switch($0.h) {
  case undefined: /* record */ {
   switch($0.a1.h) {
    case undefined: /* cons */ {
     switch($0.a1.a1.h) {
      case 5: /* JTIgnore */ return 1;
      default: return 0;
     }
    }
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.String.stringValue : String -> Maybe String */
function Language_JSON_String_stringValue($0) {
 return Prelude_Types_x3ex3ex3d_Monad_Maybe(Language_JSON_String_Lexer_lexString($0), $6 => Language_JSON_String_Parser_parseString($6));
}

/* Language.JSON.String.permissiveStringLit : Lexer */
const Language_JSON_String_permissiveStringLit = __lazy(function () {
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Language_JSON_String_Lexer_quo(), a2: () => Text_Lexer_manyUntil(Language_JSON_String_Lexer_quo(), Text_Lexer_Core_x3cx7cx3e(Language_JSON_String_Lexer_esc(Text_Lexer_any()), Text_Lexer_any()))}, a2: () => Text_Lexer_opt(Language_JSON_String_Lexer_quo())};
});

/* Text.Lexer.toTokenMap : List (Lexer, k) -> TokenMap (Token k) */
const Text_Lexer_toTokenMap = __lazy(function () {
 return csegen_156()($3 => ({a1: $3.a1, a2: $7 => ({a1: $3.a2, a2: $7})}));
});

/* Text.Lexer.spaces : Lexer */
const Text_Lexer_spaces = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_space());
});

/* Text.Lexer.space : Lexer */
const Text_Lexer_space = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isSpace($2));
});

/* Text.Lexer.some : Lexer -> Lexer */
function Text_Lexer_some($0) {
 return {h: 4 /* SeqEat */, a1: $0, a2: () => Text_Lexer_many($0)};
}

/* Text.Lexer.range : Char -> Char -> Lexer */
function Text_Lexer_range($0, $1) {
 const $3 = x => {
  switch(Prelude_EqOrd_x3ex3d_Ord_Char(x, Prelude_EqOrd_min_Ord_Char($0, $1))) {
   case 1: return Prelude_EqOrd_x3cx3d_Ord_Char(x, Prelude_EqOrd_max_Ord_Char($0, $1));
   case 0: return 0;
  }
 };
 return Text_Lexer_Core_pred($3);
}

/* Text.Lexer.opt : Lexer -> Recognise False */
function Text_Lexer_opt($0) {
 return Text_Lexer_Core_x3cx7cx3e($0, Text_Lexer_Core_empty());
}

/* Text.Lexer.oneOf : String -> Lexer */
function Text_Lexer_oneOf($0) {
 return Text_Lexer_Core_pred(x => Prelude_Types_elem(csegen_107(), csegen_214(), x, Prelude_Types_fastUnpack($0)));
}

/* Text.Lexer.non : Lexer -> Lexer */
function Text_Lexer_non($0) {
 return {h: 5 /* SeqEmpty */, a1: Text_Lexer_Core_reject($0), a2: Text_Lexer_any()};
}

/* Text.Lexer.manyUntil : Recognise c -> Lexer -> Recognise False */
function Text_Lexer_manyUntil($0, $1) {
 return Text_Lexer_many({h: 5 /* SeqEmpty */, a1: Text_Lexer_Core_reject($0), a2: $1});
}

/* Text.Lexer.many : Lexer -> Recognise False */
function Text_Lexer_many($0) {
 return Text_Lexer_opt(Text_Lexer_some($0));
}

/* Text.Lexer.like : Char -> Lexer */
function Text_Lexer_like($0) {
 return Text_Lexer_Core_pred(y => Prelude_EqOrd_x3dx3d_Eq_Char(Prelude_Types_toUpper($0), Prelude_Types_toUpper(y)));
}

/* Text.Lexer.is : Char -> Lexer */
function Text_Lexer_is($0) {
 return Text_Lexer_Core_pred($3 => Prelude_EqOrd_x3dx3d_Eq_Char($3, $0));
}

/* Text.Lexer.hexDigit : Lexer */
const Text_Lexer_hexDigit = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isHexDigit($2));
});

/* Text.Lexer.exact : String -> Lexer */
function Text_Lexer_exact($0) {
 const $1 = Prelude_Types_fastUnpack($0);
 switch($1.h) {
  case 0: /* nil */ return Text_Lexer_Core_fail();
  case undefined: /* cons */ return Text_Lexer_Core_concatMap($7 => Text_Lexer_is($7), {a1: $1.a1, a2: $1.a2});
 }
}

/* Text.Lexer.escape : Lexer -> Lexer -> Lexer */
function Text_Lexer_escape($0, $1) {
 return {h: 4 /* SeqEat */, a1: $0, a2: () => $1};
}

/* Text.Lexer.digits : Lexer */
const Text_Lexer_digits = __lazy(function () {
 return Text_Lexer_some(Text_Lexer_digit());
});

/* Text.Lexer.digit : Lexer */
const Text_Lexer_digit = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isDigit($2));
});

/* Text.Lexer.count : (q : Quantity) -> Lexer -> Recognise (isSucc (min q)) */
function Text_Lexer_count($0, $1) {
 switch($0.a1) {
  case 0n: {
   switch($0.a2.h) {
    case 0: /* nothing */ return Text_Lexer_many($1);
    case undefined: /* just */ {
     switch($0.a2.a1) {
      case 0n: return Text_Lexer_Core_empty();
      default: {
       const $9 = ($0.a2.a1-1n);
       return Text_Lexer_opt({h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_atMost($9), $1)});
      }
     }
    }
   }
  }
  default: {
   const $15 = ($0.a1-1n);
   switch($0.a2.h) {
    case 0: /* nothing */ return {h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_atLeast($15), $1)};
    case undefined: /* just */ {
     switch($0.a2.a1) {
      case 0n: return Text_Lexer_Core_fail();
      default: {
       const $22 = ($0.a2.a1-1n);
       return {h: 4 /* SeqEat */, a1: $1, a2: () => Text_Lexer_count(Text_Quantity_between($15, $22), $1)};
      }
     }
    }
   }
  }
 }
}

/* Text.Lexer.control : Lexer */
const Text_Lexer_control = __lazy(function () {
 return Text_Lexer_Core_pred($2 => Prelude_Types_isControl($2));
});

/* Text.Lexer.any : Lexer */
const Text_Lexer_any = __lazy(function () {
 return Text_Lexer_Core_pred($2 => 1);
});

/* Text.Quantity.exactly : Nat -> Quantity */
function Text_Quantity_exactly($0) {
 return {a1: $0, a2: {a1: $0}};
}

/* Text.Quantity.between : Nat -> Nat -> Quantity */
function Text_Quantity_between($0, $1) {
 return {a1: $0, a2: {a1: $1}};
}

/* Text.Quantity.atMost : Nat -> Quantity */
function Text_Quantity_atMost($0) {
 return {a1: 0n, a2: {a1: $0}};
}

/* Text.Quantity.atLeast : Nat -> Quantity */
function Text_Quantity_atLeast($0) {
 return {a1: $0, a2: {h: 0}};
}

/* Text.Lexer.Core.3713:2500:getCols */
function Text_Lexer_Core_n__3713_2500_getCols($0, $1, $2, $3, $4, $5, $6, $7) {
 const $8 = Data_List_span($b => Prelude_EqOrd_x2fx3d_Eq_Char($b, '\n'), $6);
 switch($8.a2.h) {
  case 0: /* nil */ return _add32s($7, Number(_truncBigInt32(Prelude_Types_List_lengthTR($8.a1))));
  default: return Number(_truncBigInt32(Prelude_Types_List_lengthTR($8.a1)));
 }
}

/* Text.Lexer.Core.3713:2499:countNLs */
function Text_Lexer_Core_n__3713_2499_countNLs($0, $1, $2, $3, $4, $5, $6) {
 return Prelude_Types_List_lengthTR(Prelude_Types_List_filterAppend({h: 0}, $c => Prelude_EqOrd_x3dx3d_Eq_Char($c, '\n'), $6));
}

/* Text.Lexer.Core.scan : Recognise c -> List Char -> List Char -> Maybe (List Char, List Char) */
function Text_Lexer_Core_scan($0, $1, $2) {
 switch($0.h) {
  case 0: /* Empty */ return {a1: {a1: $1, a2: $2}};
  case 1: /* Fail */ return {h: 0};
  case 2: /* Lookahead */ {
   switch(Prelude_EqOrd_x3dx3d_Eq_Bool(Data_Maybe_isJust(Text_Lexer_Core_scan($0.a2, $1, $2)), $0.a1)) {
    case 1: return {a1: {a1: $1, a2: $2}};
    case 0: return {h: 0};
   }
  }
  case 3: /* Pred */ {
   switch($2.h) {
    case 0: /* nil */ return {h: 0};
    case undefined: /* cons */ {
     switch($0.a1($2.a1)) {
      case 1: return {a1: {a1: {a1: $2.a1, a2: $1}, a2: $2.a2}};
      case 0: return {h: 0};
     }
    }
   }
  }
  case 4: /* SeqEat */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $24 => Text_Lexer_Core_scan($0.a2(), $24.a1, $24.a2));
  case 5: /* SeqEmpty */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $32 => Text_Lexer_Core_scan($0.a2, $32.a1, $32.a2));
  case 6: /* SeqSame */ return Prelude_Types_x3ex3ex3d_Monad_Maybe(Text_Lexer_Core_scan($0.a1, $1, $2), $3f => Text_Lexer_Core_scan($0.a2, $3f.a1, $3f.a2));
  case 7: /* Alt */ return Prelude_Types_maybe(() => Text_Lexer_Core_scan($0.a2, $1, $2), () => $4c => ({a1: $4c}), Text_Lexer_Core_scan($0.a1, $1, $2));
 }
}

/* Text.Lexer.Core.reject : Recognise c -> Recognise False */
function Text_Lexer_Core_reject($0) {
 return {h: 2 /* Lookahead */, a1: 0, a2: $0};
}

/* Text.Lexer.Core.pred : (Char -> Bool) -> Lexer */
function Text_Lexer_Core_pred($0) {
 return {h: 3 /* Pred */, a1: $0};
}

/* Text.Lexer.Core.lex : TokenMap a -> String -> (List (WithBounds a), (Int, (Int, String))) */
function Text_Lexer_Core_lex($0, $1) {
 const $2 = Text_Lexer_Core_tokenise($5 => 0, 0, 0, {h: 0}, $0, Prelude_Types_fastUnpack($1));
 return {a1: $2.a1, a2: {a1: $2.a2.a1, a2: {a1: $2.a2.a2.a1, a2: Prelude_Types_fastPack($2.a2.a2.a2)}}};
}

/* Text.Lexer.Core.fail : Recognise c */
const Text_Lexer_Core_fail = __lazy(function () {
 return {h: 1 /* Fail */};
});

/* Text.Lexer.Core.empty : Recognise False */
const Text_Lexer_Core_empty = __lazy(function () {
 return {h: 0 /* Empty */};
});

/* Text.Lexer.Core.concatMap : (a -> Recognise c) -> (xs : List a) -> Recognise (isCons xs && Delay c) */
function Text_Lexer_Core_concatMap($0, $1) {
 switch($1.h) {
  case 0: /* nil */ return {h: 0 /* Empty */};
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case 0: /* nil */ return $0($1.a1);
    case undefined: /* cons */ return {h: 6 /* SeqSame */, a1: $0($1.a1), a2: Text_Lexer_Core_concatMap($0, $1.a2)};
   }
  }
 }
}

/* Text.Lexer.Core.<|> : Recognise c1 -> Recognise c2 -> Recognise (c1 && Delay c2) */
function Text_Lexer_Core_x3cx7cx3e($0, $1) {
 return {h: 7 /* Alt */, a1: $0, a2: $1};
}

/* Language.JSON.String.Tokens.3261:1166:hexVal */
function Language_JSON_String_Tokens_n__3261_1166_hexVal($0, $1) {
 switch(Prelude_EqOrd_x3ex3d_Ord_Char($1, 'A')) {
  case 1: return _add32s(_sub32s(_truncInt32($1.codePointAt(0)), _truncInt32('A'.codePointAt(0))), 10);
  case 0: return _sub32s(_truncInt32($1.codePointAt(0)), _truncInt32('0'.codePointAt(0)));
 }
}

/* Language.JSON.String.Tokens.tokValue */
function Language_JSON_String_Tokens_tokValue_TokenKind_JSONStringTokenKind($0, $1) {
 switch($0) {
  case 0: return undefined;
  case 1: return Language_JSON_String_Tokens_charValue($1);
  case 2: return Language_JSON_String_Tokens_simpleEscapeValue($1);
  case 3: return Language_JSON_String_Tokens_unicodeEscapeValue($1);
 }
}

/* Language.JSON.String.Tokens.TokType */
function Language_JSON_String_Tokens_TokType_TokenKind_JSONStringTokenKind($0) {
 switch($0) {
  case 0: return {h: 'Builtin.Unit'};
  case 1: return {h: 'Char'};
  case 2: return {h: 'Char'};
  case 3: return {h: 'Char'};
 }
}

/* Language.JSON.String.Tokens.== */
function Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($0, $1) {
 switch($0) {
  case 0: {
   switch($1) {
    case 0: return 1;
    default: return 0;
   }
  }
  case 1: {
   switch($1) {
    case 1: return 1;
    default: return 0;
   }
  }
  case 2: {
   switch($1) {
    case 2: return 1;
    default: return 0;
   }
  }
  case 3: {
   switch($1) {
    case 3: return 1;
    default: return 0;
   }
  }
  default: return 0;
 }
}

/* Language.JSON.String.Tokens./= */
function Language_JSON_String_Tokens_x2fx3d_Eq_JSONStringTokenKind($0, $1) {
 switch(Language_JSON_String_Tokens_x3dx3d_Eq_JSONStringTokenKind($0, $1)) {
  case 1: return 0;
  case 0: return 1;
 }
}

/* Language.JSON.String.Tokens.unicodeEscapeValue : String -> Char */
function Language_JSON_String_Tokens_unicodeEscapeValue($0) {
 return Language_JSON_String_Tokens_n__3261_1167_fromHex($0, Data_List_drop(2n, Prelude_Types_fastUnpack($0)), 0);
}

/* Language.JSON.String.Tokens.simpleEscapeValue : String -> Char */
function Language_JSON_String_Tokens_simpleEscapeValue($0) {
 const $1 = Data_String_Extra_index(1n, $0);
 switch($1.h) {
  case 0: /* nothing */ return '\0';
  case undefined: /* just */ {
   switch($1.a1) {
    case '\"': return '\"';
    case '\u{5c}': return '\u{5c}';
    case '/': return '/';
    case 'b': return '\u{8}';
    case 'f': return '\u{c}';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\u{9}';
    default: return '\0';
   }
  }
 }
}

/* Language.JSON.String.Tokens.charValue : String -> Char */
function Language_JSON_String_Tokens_charValue($0) {
 const $1 = Data_String_Extra_index(0n, $0);
 switch($1.h) {
  case 0: /* nothing */ return '\0';
  case undefined: /* just */ return $1.a1;
 }
}

/* Language.JSON.String.Parser.stringChar : Grammar state JSONStringToken True Char */
const Language_JSON_String_Parser_stringChar = __lazy(function () {
 return {h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_match(csegen_314(), csegen_317(), 1), a4: () => ({h: 12 /* Alt */, a1: 1, a2: 1, a3: Text_Parser_match(csegen_314(), csegen_317(), 2), a4: () => Text_Parser_match(csegen_314(), csegen_317(), 3)})};
});

/* Language.JSON.String.Parser.quotedString : Grammar state JSONStringToken True String */
const Language_JSON_String_Parser_quotedString = __lazy(function () {
 const $0 = Text_Parser_match(csegen_314(), csegen_317(), 0);
 return {h: 8 /* SeqEat */, a1: 0, a2: Text_Parser_between(0, $0, $0, Text_Parser_many(Language_JSON_String_Parser_stringChar())), a3: () => chars => ({h: 11 /* ThenEmpty */, a1: 0, a2: 0, a3: {h: 3 /* EOF */}, a4: {h: 0 /* Empty */, a1: Prelude_Types_fastPack(chars)}})};
});

/* Language.JSON.String.Parser.parseString : List (WithBounds JSONStringToken) -> Maybe String */
function Language_JSON_String_Parser_parseString($0) {
 const $1 = Text_Parser_Core_parse(1, Language_JSON_String_Parser_quotedString(), $0);
 switch($1.h) {
  case 1: /* Right */ {
   switch($1.a1.h) {
    case undefined: /* cons */ {
     switch($1.a1.a2.h) {
      case 0: /* nil */ return {a1: $1.a1.a1};
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Text.Parser.some : Grammar state tok True a -> Grammar state tok True (List1 a) */
function Text_Parser_some($0) {
 return {h: 8 /* SeqEat */, a1: 0, a2: $0, a3: () => $4 => ({h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: Text_Parser_many($0), a4: $b => ({h: 0 /* Empty */, a1: {a1: $4, a2: $b}})})};
}

/* Text.Parser.sepBy1 : Grammar state tok True s -> Grammar state tok c a -> Grammar state tok c (List1 a) */
function Text_Parser_sepBy1($0, $1, $2) {
 const $e = f => {
  const $11 = b => a => func => $12 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $12);
  const $10 = $17 => $18 => $11(undefined)(undefined)($17)($18);
  const $f = $10(f);
  return $f($2);
 };
 const $5 = {h: 9 /* SeqEmpty */, a1: 0, a2: $0, a3: {h: 0 /* Empty */, a1: $a => $b => ({a1: $a, a2: $b})}, a4: $e};
 const $23 = f => {
  const $26 = b => a => func => $27 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29(0, func, $27);
  const $25 = $2c => $2d => $26(undefined)(undefined)($2c)($2d);
  const $24 = $25(f);
  const $40 = $41 => {
   const $44 = b => a => func => $45 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $45);
   const $43 = $4a => $4b => $44(undefined)(undefined)($4a)($4b);
   const $42 = $43($41);
   return $42($2);
  };
  const $39 = {h: 9 /* SeqEmpty */, a1: 1, a2: $0, a3: csegen_336()($1), a4: $40};
  const $37 = Text_Parser_many($39);
  return $24($37);
 };
 return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: $5, a4: $23};
}

/* Text.Parser.sepBy : Grammar state tok True s -> Grammar state tok c a -> Grammar state tok False (List a) */
function Text_Parser_sepBy($0, $1, $2) {
 return Text_Parser_option($0, {h: 0}, Prelude_Interfaces_x3cx24x3e($9 => $a => $b => $c => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $b, $c), $12 => Data_List1_forget($12), Text_Parser_sepBy1($0, $1, $2)));
}

/* Text.Parser.option : a -> Grammar state tok c a -> Grammar state tok False a */
function Text_Parser_option($0, $1, $2) {
 switch($0) {
  case 0: return {h: 12 /* Alt */, a1: 0, a2: 0, a3: $2, a4: () => ({h: 0 /* Empty */, a1: $1})};
  case 1: return {h: 12 /* Alt */, a1: 1, a2: 0, a3: $2, a4: () => ({h: 0 /* Empty */, a1: $1})};
 }
}

/* Text.Parser.match : {auto conArg : TokenKind k} -> Eq k => (kind : k) ->
Grammar state (Token k) True (TokType kind) */
function Text_Parser_match($0, $1, $2) {
 const $4 = t => {
  switch($1.a1(t.a1)($2)) {
   case 1: return {a1: $0.a2($2)(t.a2)};
   case 0: return {h: 0};
  }
 };
 return {h: 1 /* Terminal */, a1: 'Unrecognised input', a2: $4};
}

/* Text.Parser.many : Grammar state tok True a -> Grammar state tok False (List a) */
function Text_Parser_many($0) {
 return Text_Parser_option(1, {h: 0}, Prelude_Interfaces_x3cx24x3e(csegen_264(), $9 => Data_List1_forget($9), Text_Parser_some($0)));
}

/* Text.Parser.between : Grammar state tok True l -> Grammar state tok True r -> Grammar state tok c a -> Grammar state tok True a */
function Text_Parser_between($0, $1, $2, $3) {
 const $14 = f => {
  const $17 = b => a => func => $18 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $18);
  const $16 = $1d => $1e => $17(undefined)(undefined)($1d)($1e);
  const $15 = $16(f);
  return $15($3);
 };
 const $d = {h: 9 /* SeqEmpty */, a1: 1, a2: $0, a3: csegen_336()($1), a4: $14};
 const $6 = csegen_335()($b => $c => $b)($d);
 return {h: 9 /* SeqEmpty */, a1: 1, a2: 1, a3: $6, a4: f => csegen_335()(f)($2)};
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_5194($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: $5.a2, a3: $5.a3};
  case 1: /* Res */ return {h: 1 /* Res */, a1: $5.a1, a2: $5.a2, a3: Prelude_Interfaces_x3cx24x3e(csegen_340(), $11 => $5.a3, $5.a3), a4: $5.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4881($0, $1, $2, $3, $4, $5, $6, $7) {
 switch($7.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $7.a1, a2: $7.a2, a3: $7.a3};
  case 1: /* Res */ return Text_Parser_Core_mergeWith($7.a3, Text_Parser_Core_doParse($0, $7.a1, $7.a2, $3()($7.a3.a1), $7.a4));
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4767($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 switch($8.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $8.a1, a2: $8.a2, a3: $8.a3};
  case 1: /* Res */ return Text_Parser_Core_mergeWith($8.a3, Text_Parser_Core_doParse($0, $8.a1, $8.a2, $4($8.a3.a1), $8.a4));
 }
}

/* Text.Parser.Core.case block in case block in case block in doParse */
function Text_Parser_Core_case__casex20blockx20inx20casex20blockx20inx20doParse_4529($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b) {
 switch($b.h) {
  case 0: /* Failure */ {
   let $d;
   switch($b.a1) {
    case 1: {
     $d = 1;
     break;
    }
    case 0: {
     $d = $b.a2;
     break;
    }
   }
   switch($d) {
    case 1: return {h: 0 /* Failure */, a1: $b.a1, a2: $b.a2, a3: $b.a3};
    case 0: return {h: 0 /* Failure */, a1: $6, a2: 0, a3: Data_List1_x2bx2b($7, $b.a3)};
   }
  }
  case 1: /* Res */ return {h: 1 /* Res */, a1: $b.a1, a2: $6, a3: $b.a3, a4: $b.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4413($0, $1, $2, $3, $4, $5, $6, $7, $8) {
 switch($8.h) {
  case 0: /* Failure */ {
   let $a;
   switch($8.a1) {
    case 1: {
     $a = 1;
     break;
    }
    case 0: {
     $a = $8.a2;
     break;
    }
   }
   switch($a) {
    case 1: return {h: 0 /* Failure */, a1: $7, a2: $8.a2, a3: $8.a3};
    case 0: return Text_Parser_Core_case__casex20blockx20inx20casex20blockx20inx20doParse_4529($0, $2, $3, $4, $5, $6, $7, $8.a3, $8.a2, $8.a1, $1, Text_Parser_Core_doParse($0, $1, 0, $3(), $6));
   }
  }
  case 1: /* Res */ return {h: 1 /* Res */, a1: $8.a1, a2: $7, a3: $8.a3, a4: $8.a4};
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_4048($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: 1, a3: $5.a3};
  default: return $5;
 }
}

/* Text.Parser.Core.case block in doParse */
function Text_Parser_Core_case__doParse_3951($0, $1, $2, $3, $4, $5) {
 switch($5.h) {
  case 0: /* Failure */ return {h: 0 /* Failure */, a1: $5.a1, a2: 0, a3: $5.a3};
  default: return $5;
 }
}

/* Text.Parser.Core.map */
function Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, $1, $2) {
 switch($0) {
  case 0: {
   switch($2.h) {
    case 0: /* Empty */ return {h: 0 /* Empty */, a1: $1($2.a1)};
    default: {
     switch($2.h) {
      case 4: /* Fail */ return {h: 4 /* Fail */, a1: $2.a1, a2: $2.a2, a3: $2.a3};
      case 5: /* Try */ {
       const $f = b => a => func => $10 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $10);
       const $e = $15 => $16 => $f(undefined)(undefined)($15)($16);
       const $d = $e($1);
       const $c = $d($2.a1);
       return {h: 5 /* Try */, a1: $c};
      }
      case 7: /* MustWork */ {
       const $24 = b => a => func => $25 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $25);
       const $23 = $2a => $2b => $24(undefined)(undefined)($2a)($2b);
       const $22 = $23($1);
       const $21 = $22($2.a1);
       return {h: 7 /* MustWork */, a1: $21};
      }
      default: {
       switch($0) {
        case 1: {
         switch($2.h) {
          case 1: /* Terminal */ return {h: 1 /* Terminal */, a1: $2.a1, a2: $3a => csegen_341()($1)($2.a2($3a))};
          default: {
           switch($2.h) {
            case 12: /* Alt */ {
             const $48 = b => a => func => $49 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $49);
             const $47 = $4e => $4f => $48(undefined)(undefined)($4e)($4f);
             const $46 = $47($1);
             const $45 = $46($2.a3);
             const $5a = () => {
              const $5d = b => a => func => $5e => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $5e);
              const $5c = $63 => $64 => $5d(undefined)(undefined)($63)($64);
              const $5b = $5c($1);
              return $5b($2.a4());
             };
             return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: $45, a4: $5a};
            }
            default: {
             switch($0) {
              case 1: {
               switch($2.h) {
                case 8: /* SeqEat */ {
                 return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => {
                  const $77 = b => a => func => $78 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $78);
                  const $76 = $7d => $7e => $77(undefined)(undefined)($7d)($7e);
                  const $75 = $76($1);
                  return $75($2.a3()(val));
                 }};
                }
                default: {
                 switch($2.h) {
                  case 9: /* SeqEmpty */ {
                   const $90 = val => {
                    const $93 = b => a => func => $94 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $94);
                    const $92 = $99 => $9a => $93(undefined)(undefined)($99)($9a);
                    const $91 = $92($1);
                    return $91($2.a4(val));
                   };
                   return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $90};
                  }
                  default: {
                   switch($0) {
                    case 1: {
                     switch($2.h) {
                      case 10: /* ThenEat */ {
                       const $ab = () => {
                        const $ae = b => a => func => $af => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $af);
                        const $ad = $b4 => $b5 => $ae(undefined)(undefined)($b4)($b5);
                        const $ac = $ad($1);
                        return $ac($2.a3());
                       };
                       return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $ab};
                      }
                      default: {
                       switch($2.h) {
                        case 11: /* ThenEmpty */ {
                         const $c8 = b => a => func => $c9 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $c9);
                         const $c7 = $ce => $cf => $c8(undefined)(undefined)($ce)($cf);
                         const $c6 = $c7($1);
                         const $c5 = $c6($2.a4);
                         return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $c5};
                        }
                        case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $df => ({h: 0 /* Empty */, a1: $1($df)})};
                        default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $e7 => ({h: 0 /* Empty */, a1: $1($e7)})};
                       }
                      }
                     }
                    }
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ {
                       const $f2 = b => a => func => $f3 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $f3);
                       const $f1 = $f8 => $f9 => $f2(undefined)(undefined)($f8)($f9);
                       const $f0 = $f1($1);
                       const $ef = $f0($2.a4);
                       return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $ef};
                      }
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $109 => ({h: 0 /* Empty */, a1: $1($109)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $111 => ({h: 0 /* Empty */, a1: $1($111)})};
                     }
                    }
                   }
                  }
                 }
                }
               }
              }
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ {
                 const $119 = val => {
                  const $11c = b => a => func => $11d => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $11d);
                  const $11b = $122 => $123 => $11c(undefined)(undefined)($122)($123);
                  const $11a = $11b($1);
                  return $11a($2.a4(val));
                 };
                 return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $119};
                }
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ {
                     const $134 = () => {
                      const $137 = b => a => func => $138 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $138);
                      const $136 = $13d => $13e => $137(undefined)(undefined)($13d)($13e);
                      const $135 = $136($1);
                      return $135($2.a3());
                     };
                     return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $134};
                    }
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ {
                       const $151 = b => a => func => $152 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $152);
                       const $150 = $157 => $158 => $151(undefined)(undefined)($157)($158);
                       const $14f = $150($1);
                       const $14e = $14f($2.a4);
                       return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $14e};
                      }
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $168 => ({h: 0 /* Empty */, a1: $1($168)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $170 => ({h: 0 /* Empty */, a1: $1($170)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $17b = b => a => func => $17c => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $17c);
                     const $17a = $181 => $182 => $17b(undefined)(undefined)($181)($182);
                     const $179 = $17a($1);
                     const $178 = $179($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $178};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $192 => ({h: 0 /* Empty */, a1: $1($192)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $19a => ({h: 0 /* Empty */, a1: $1($19a)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
        default: {
         switch($2.h) {
          case 12: /* Alt */ {
           const $1a4 = b => a => func => $1a5 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $1a5);
           const $1a3 = $1aa => $1ab => $1a4(undefined)(undefined)($1aa)($1ab);
           const $1a2 = $1a3($1);
           const $1a1 = $1a2($2.a3);
           const $1b6 = () => {
            const $1b9 = b => a => func => $1ba => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $1ba);
            const $1b8 = $1bf => $1c0 => $1b9(undefined)(undefined)($1bf)($1c0);
            const $1b7 = $1b8($1);
            return $1b7($2.a4());
           };
           return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: $1a1, a4: $1b6};
          }
          default: {
           switch($0) {
            case 1: {
             switch($2.h) {
              case 8: /* SeqEat */ {
               return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => {
                const $1d3 = b => a => func => $1d4 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $1d4);
                const $1d2 = $1d9 => $1da => $1d3(undefined)(undefined)($1d9)($1da);
                const $1d1 = $1d2($1);
                return $1d1($2.a3()(val));
               }};
              }
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ {
                 const $1ec = val => {
                  const $1ef = b => a => func => $1f0 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $1f0);
                  const $1ee = $1f5 => $1f6 => $1ef(undefined)(undefined)($1f5)($1f6);
                  const $1ed = $1ee($1);
                  return $1ed($2.a4(val));
                 };
                 return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $1ec};
                }
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ {
                     const $207 = () => {
                      const $20a = b => a => func => $20b => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $20b);
                      const $209 = $210 => $211 => $20a(undefined)(undefined)($210)($211);
                      const $208 = $209($1);
                      return $208($2.a3());
                     };
                     return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $207};
                    }
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ {
                       const $224 = b => a => func => $225 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $225);
                       const $223 = $22a => $22b => $224(undefined)(undefined)($22a)($22b);
                       const $222 = $223($1);
                       const $221 = $222($2.a4);
                       return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $221};
                      }
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $23b => ({h: 0 /* Empty */, a1: $1($23b)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $243 => ({h: 0 /* Empty */, a1: $1($243)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $24e = b => a => func => $24f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $24f);
                     const $24d = $254 => $255 => $24e(undefined)(undefined)($254)($255);
                     const $24c = $24d($1);
                     const $24b = $24c($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $24b};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $265 => ({h: 0 /* Empty */, a1: $1($265)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $26d => ({h: 0 /* Empty */, a1: $1($26d)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ {
               const $275 = val => {
                const $278 = b => a => func => $279 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $279);
                const $277 = $27e => $27f => $278(undefined)(undefined)($27e)($27f);
                const $276 = $277($1);
                return $276($2.a4(val));
               };
               return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $275};
              }
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ {
                   const $290 = () => {
                    const $293 = b => a => func => $294 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $294);
                    const $292 = $299 => $29a => $293(undefined)(undefined)($299)($29a);
                    const $291 = $292($1);
                    return $291($2.a3());
                   };
                   return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $290};
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $2ad = b => a => func => $2ae => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $2ae);
                     const $2ac = $2b3 => $2b4 => $2ad(undefined)(undefined)($2b3)($2b4);
                     const $2ab = $2ac($1);
                     const $2aa = $2ab($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $2aa};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $2c4 => ({h: 0 /* Empty */, a1: $1($2c4)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $2cc => ({h: 0 /* Empty */, a1: $1($2cc)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ {
                   const $2d7 = b => a => func => $2d8 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $2d8);
                   const $2d6 = $2dd => $2de => $2d7(undefined)(undefined)($2dd)($2de);
                   const $2d5 = $2d6($1);
                   const $2d4 = $2d5($2.a4);
                   return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $2d4};
                  }
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $2ee => ({h: 0 /* Empty */, a1: $1($2ee)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $2f6 => ({h: 0 /* Empty */, a1: $1($2f6)})};
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
  default: {
   switch($2.h) {
    case 4: /* Fail */ return {h: 4 /* Fail */, a1: $2.a1, a2: $2.a2, a3: $2.a3};
    case 5: /* Try */ {
     const $301 = b => a => func => $302 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $302);
     const $300 = $307 => $308 => $301(undefined)(undefined)($307)($308);
     const $2ff = $300($1);
     const $2fe = $2ff($2.a1);
     return {h: 5 /* Try */, a1: $2fe};
    }
    case 7: /* MustWork */ {
     const $316 = b => a => func => $317 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($0, func, $317);
     const $315 = $31c => $31d => $316(undefined)(undefined)($31c)($31d);
     const $314 = $315($1);
     const $313 = $314($2.a1);
     return {h: 7 /* MustWork */, a1: $313};
    }
    default: {
     switch($0) {
      case 1: {
       switch($2.h) {
        case 1: /* Terminal */ return {h: 1 /* Terminal */, a1: $2.a1, a2: $32c => csegen_341()($1)($2.a2($32c))};
        default: {
         switch($2.h) {
          case 12: /* Alt */ {
           const $33a = b => a => func => $33b => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $33b);
           const $339 = $340 => $341 => $33a(undefined)(undefined)($340)($341);
           const $338 = $339($1);
           const $337 = $338($2.a3);
           const $34c = () => {
            const $34f = b => a => func => $350 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $350);
            const $34e = $355 => $356 => $34f(undefined)(undefined)($355)($356);
            const $34d = $34e($1);
            return $34d($2.a4());
           };
           return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: $337, a4: $34c};
          }
          default: {
           switch($0) {
            case 1: {
             switch($2.h) {
              case 8: /* SeqEat */ {
               return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => {
                const $369 = b => a => func => $36a => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $36a);
                const $368 = $36f => $370 => $369(undefined)(undefined)($36f)($370);
                const $367 = $368($1);
                return $367($2.a3()(val));
               }};
              }
              default: {
               switch($2.h) {
                case 9: /* SeqEmpty */ {
                 const $382 = val => {
                  const $385 = b => a => func => $386 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $386);
                  const $384 = $38b => $38c => $385(undefined)(undefined)($38b)($38c);
                  const $383 = $384($1);
                  return $383($2.a4(val));
                 };
                 return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $382};
                }
                default: {
                 switch($0) {
                  case 1: {
                   switch($2.h) {
                    case 10: /* ThenEat */ {
                     const $39d = () => {
                      const $3a0 = b => a => func => $3a1 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $3a1);
                      const $39f = $3a6 => $3a7 => $3a0(undefined)(undefined)($3a6)($3a7);
                      const $39e = $39f($1);
                      return $39e($2.a3());
                     };
                     return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $39d};
                    }
                    default: {
                     switch($2.h) {
                      case 11: /* ThenEmpty */ {
                       const $3ba = b => a => func => $3bb => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $3bb);
                       const $3b9 = $3c0 => $3c1 => $3ba(undefined)(undefined)($3c0)($3c1);
                       const $3b8 = $3b9($1);
                       const $3b7 = $3b8($2.a4);
                       return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $3b7};
                      }
                      case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $3d1 => ({h: 0 /* Empty */, a1: $1($3d1)})};
                      default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $3d9 => ({h: 0 /* Empty */, a1: $1($3d9)})};
                     }
                    }
                   }
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $3e4 = b => a => func => $3e5 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $3e5);
                     const $3e3 = $3ea => $3eb => $3e4(undefined)(undefined)($3ea)($3eb);
                     const $3e2 = $3e3($1);
                     const $3e1 = $3e2($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $3e1};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $3fb => ({h: 0 /* Empty */, a1: $1($3fb)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $403 => ({h: 0 /* Empty */, a1: $1($403)})};
                   }
                  }
                 }
                }
               }
              }
             }
            }
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ {
               const $40b = val => {
                const $40e = b => a => func => $40f => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $40f);
                const $40d = $414 => $415 => $40e(undefined)(undefined)($414)($415);
                const $40c = $40d($1);
                return $40c($2.a4(val));
               };
               return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $40b};
              }
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ {
                   const $426 = () => {
                    const $429 = b => a => func => $42a => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $42a);
                    const $428 = $42f => $430 => $429(undefined)(undefined)($42f)($430);
                    const $427 = $428($1);
                    return $427($2.a3());
                   };
                   return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $426};
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $443 = b => a => func => $444 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $444);
                     const $442 = $449 => $44a => $443(undefined)(undefined)($449)($44a);
                     const $441 = $442($1);
                     const $440 = $441($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $440};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $45a => ({h: 0 /* Empty */, a1: $1($45a)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $462 => ({h: 0 /* Empty */, a1: $1($462)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ {
                   const $46d = b => a => func => $46e => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $46e);
                   const $46c = $473 => $474 => $46d(undefined)(undefined)($473)($474);
                   const $46b = $46c($1);
                   const $46a = $46b($2.a4);
                   return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $46a};
                  }
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $484 => ({h: 0 /* Empty */, a1: $1($484)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $48c => ({h: 0 /* Empty */, a1: $1($48c)})};
                 }
                }
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
      default: {
       switch($2.h) {
        case 12: /* Alt */ {
         const $496 = b => a => func => $497 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $497);
         const $495 = $49c => $49d => $496(undefined)(undefined)($49c)($49d);
         const $494 = $495($1);
         const $493 = $494($2.a3);
         const $4a8 = () => {
          const $4ab = b => a => func => $4ac => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $4ac);
          const $4aa = $4b1 => $4b2 => $4ab(undefined)(undefined)($4b1)($4b2);
          const $4a9 = $4aa($1);
          return $4a9($2.a4());
         };
         return {h: 12 /* Alt */, a1: $2.a1, a2: $2.a2, a3: $493, a4: $4a8};
        }
        default: {
         switch($0) {
          case 1: {
           switch($2.h) {
            case 8: /* SeqEat */ {
             return {h: 8 /* SeqEat */, a1: $2.a1, a2: $2.a2, a3: () => val => {
              const $4c5 = b => a => func => $4c6 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $4c6);
              const $4c4 = $4cb => $4cc => $4c5(undefined)(undefined)($4cb)($4cc);
              const $4c3 = $4c4($1);
              return $4c3($2.a3()(val));
             }};
            }
            default: {
             switch($2.h) {
              case 9: /* SeqEmpty */ {
               const $4de = val => {
                const $4e1 = b => a => func => $4e2 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $4e2);
                const $4e0 = $4e7 => $4e8 => $4e1(undefined)(undefined)($4e7)($4e8);
                const $4df = $4e0($1);
                return $4df($2.a4(val));
               };
               return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $4de};
              }
              default: {
               switch($0) {
                case 1: {
                 switch($2.h) {
                  case 10: /* ThenEat */ {
                   const $4f9 = () => {
                    const $4fc = b => a => func => $4fd => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $4fd);
                    const $4fb = $502 => $503 => $4fc(undefined)(undefined)($502)($503);
                    const $4fa = $4fb($1);
                    return $4fa($2.a3());
                   };
                   return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $4f9};
                  }
                  default: {
                   switch($2.h) {
                    case 11: /* ThenEmpty */ {
                     const $516 = b => a => func => $517 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $517);
                     const $515 = $51c => $51d => $516(undefined)(undefined)($51c)($51d);
                     const $514 = $515($1);
                     const $513 = $514($2.a4);
                     return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $513};
                    }
                    case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $52d => ({h: 0 /* Empty */, a1: $1($52d)})};
                    default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $535 => ({h: 0 /* Empty */, a1: $1($535)})};
                   }
                  }
                 }
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ {
                   const $540 = b => a => func => $541 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $541);
                   const $53f = $546 => $547 => $540(undefined)(undefined)($546)($547);
                   const $53e = $53f($1);
                   const $53d = $53e($2.a4);
                   return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $53d};
                  }
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $557 => ({h: 0 /* Empty */, a1: $1($557)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $55f => ({h: 0 /* Empty */, a1: $1($55f)})};
                 }
                }
               }
              }
             }
            }
           }
          }
          default: {
           switch($2.h) {
            case 9: /* SeqEmpty */ {
             const $567 = val => {
              const $56a = b => a => func => $56b => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $56b);
              const $569 = $570 => $571 => $56a(undefined)(undefined)($570)($571);
              const $568 = $569($1);
              return $568($2.a4(val));
             };
             return {h: 9 /* SeqEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $567};
            }
            default: {
             switch($0) {
              case 1: {
               switch($2.h) {
                case 10: /* ThenEat */ {
                 const $582 = () => {
                  const $585 = b => a => func => $586 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a1, func, $586);
                  const $584 = $58b => $58c => $585(undefined)(undefined)($58b)($58c);
                  const $583 = $584($1);
                  return $583($2.a3());
                 };
                 return {h: 10 /* ThenEat */, a1: $2.a1, a2: $2.a2, a3: $582};
                }
                default: {
                 switch($2.h) {
                  case 11: /* ThenEmpty */ {
                   const $59f = b => a => func => $5a0 => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $5a0);
                   const $59e = $5a5 => $5a6 => $59f(undefined)(undefined)($5a5)($5a6);
                   const $59d = $59e($1);
                   const $59c = $59d($2.a4);
                   return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $59c};
                  }
                  case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $5b6 => ({h: 0 /* Empty */, a1: $1($5b6)})};
                  default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $5be => ({h: 0 /* Empty */, a1: $1($5be)})};
                 }
                }
               }
              }
              default: {
               switch($2.h) {
                case 11: /* ThenEmpty */ {
                 const $5c9 = b => a => func => $5ca => Text_Parser_Core_map_Functor_x28x28x28Grammarx20x24statex29x20x24tokx29x20x24cx29($2.a2, func, $5ca);
                 const $5c8 = $5cf => $5d0 => $5c9(undefined)(undefined)($5cf)($5d0);
                 const $5c7 = $5c8($1);
                 const $5c6 = $5c7($2.a4);
                 return {h: 11 /* ThenEmpty */, a1: $2.a1, a2: $2.a2, a3: $2.a3, a4: $5c6};
                }
                case 13: /* Bounds */ return {h: 9 /* SeqEmpty */, a1: $0, a2: 0, a3: {h: 13 /* Bounds */, a1: $2.a1}, a4: $5e0 => ({h: 0 /* Empty */, a1: $1($5e0)})};
                default: return {h: 9 /* SeqEmpty */, a1: 0, a2: 0, a3: $2, a4: $5e8 => ({h: 0 /* Empty */, a1: $1($5e8)})};
               }
              }
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
}

/* Text.Parser.Core.parse : Grammar () tok c ty -> List (WithBounds tok) -> Either (List1 (ParsingError tok)) (ty,
List (WithBounds tok)) */
function Text_Parser_Core_parse($0, $1, $2) {
 const $3 = Text_Parser_Core_doParse($6 => $7 => (undefined), undefined, 0, $1, $2);
 switch($3.h) {
  case 0: /* Failure */ return {h: 0 /* Left */, a1: $3.a3};
  case 1: /* Res */ return {h: 1 /* Right */, a1: {a1: $3.a3.a1, a2: $3.a4}};
 }
}

/* Text.Parser.Core.mergeWith : WithBounds ty -> ParseResult state tok sy -> ParseResult state tok sy */
function Text_Parser_Core_mergeWith($0, $1) {
 switch($1.h) {
  case 1: /* Res */ return {h: 1 /* Res */, a1: $1.a1, a2: $1.a2, a3: Text_Bounded_mergeBounds($0, $1.a3), a4: $1.a4};
  default: return $1;
 }
}

/* Text.Parser.Core.doParse : Semigroup state =>
state -> Bool -> Grammar state tok c ty -> List (WithBounds tok) -> ParseResult state tok ty */
function Text_Parser_Core_doParse($0, $1, $2, $3, $4) {
 switch($3.h) {
  case 0: /* Empty */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds($3.a1), a4: $4};
  case 4: /* Fail */ return {h: 0 /* Failure */, a1: $2, a2: $3.a2, a3: {a1: {a1: $3.a3, a2: Prelude_Types_x3cx7cx3e_Alternative_Maybe($3.a1, () => Prelude_Interfaces_x3cx24x3e(csegen_112(), $19 => $19.a3, Data_List_headx27($4)))}, a2: {h: 0}}};
  case 5: /* Try */ return Text_Parser_Core_case__doParse_3951($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 6: /* Commit */ return {h: 1 /* Res */, a1: $1, a2: 1, a3: Text_Bounded_irrelevantBounds(undefined), a4: $4};
  case 7: /* MustWork */ return Text_Parser_Core_case__doParse_4048($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 1: /* Terminal */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_345()};
    case undefined: /* cons */ {
     const $44 = $3.a2($4.a1.a1);
     switch($44.h) {
      case 0: /* nothing */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: $3.a1, a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
      case undefined: /* just */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Prelude_Interfaces_x3cx24x3e(csegen_340(), $58 => $44.a1, $4.a1), a4: $4.a2};
     }
    }
   }
  }
  case 3: /* EOF */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds(undefined), a4: {h: 0}};
    case undefined: /* cons */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: csegen_108()('Expected end of input'), a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
   }
  }
  case 2: /* NextIs */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_345()};
    case undefined: /* cons */ {
     switch($3.a2($4.a1.a1)) {
      case 1: return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_removeIrrelevance($4.a1), a4: {a1: $4.a1, a2: $4.a2}};
      case 0: return {h: 0 /* Failure */, a1: $2, a2: 0, a3: {a1: {a1: $3.a1, a2: {a1: $4.a1.a3}}, a2: {h: 0}}};
     }
    }
   }
  }
  case 12: /* Alt */ return Text_Parser_Core_case__doParse_4413($0, $1, $3.a2, $3.a4, $3.a1, $3.a3, $4, $2, Text_Parser_Core_doParse($0, $1, 0, $3.a3, $4));
  case 9: /* SeqEmpty */ return Text_Parser_Core_case__doParse_4767($0, $3.a1, $3.a2, $1, $3.a4, $3.a3, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a3, $4));
  case 8: /* SeqEat */ return Text_Parser_Core_case__doParse_4881($0, $3.a1, $1, $3.a3, $3.a2, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a2, $4));
  case 11: /* ThenEmpty */ {
   const $b7 = Text_Parser_Core_doParse($0, $1, $2, $3.a3, $4);
   switch($b7.h) {
    case 0: /* Failure */ return {h: 0 /* Failure */, a1: $b7.a1, a2: $b7.a2, a3: $b7.a3};
    case 1: /* Res */ return Text_Parser_Core_mergeWith($b7.a3, Text_Parser_Core_doParse($0, $b7.a1, $b7.a2, $3.a4, $b7.a4));
   }
  }
  case 10: /* ThenEat */ {
   const $ca = Text_Parser_Core_doParse($0, $1, $2, $3.a2, $4);
   switch($ca.h) {
    case 0: /* Failure */ return {h: 0 /* Failure */, a1: $ca.a1, a2: $ca.a2, a3: $ca.a3};
    case 1: /* Res */ return Text_Parser_Core_mergeWith($ca.a3, Text_Parser_Core_doParse($0, $ca.a1, $ca.a2, $3.a3(), $ca.a4));
   }
  }
  case 13: /* Bounds */ return Text_Parser_Core_case__doParse_5194($0, $1, $3.a1, $4, $2, Text_Parser_Core_doParse($0, $1, $2, $3.a1, $4));
  case 14: /* Position */ {
   switch($4.h) {
    case 0: /* nil */ return {h: 0 /* Failure */, a1: $2, a2: 0, a3: csegen_345()};
    case undefined: /* cons */ return {h: 1 /* Res */, a1: $1, a2: $2, a3: Text_Bounded_irrelevantBounds($4.a1.a3), a4: {a1: $4.a1, a2: $4.a2}};
   }
  }
  case 15: /* Act */ {
   const $fc = $0;
   const $fb = $fd => $fe => $fc($fd)($fe);
   const $fa = $fb($1);
   const $f9 = $fa($3.a1);
   return {h: 1 /* Res */, a1: $f9, a2: $2, a3: Text_Bounded_irrelevantBounds(undefined), a4: $4};
  }
 }
}

/* Language.JSON.String.Lexer.unicodeEscape : Lexer */
const Language_JSON_String_Lexer_unicodeEscape = __lazy(function () {
 return Language_JSON_String_Lexer_esc({h: 4 /* SeqEat */, a1: Text_Lexer_is('u'), a2: () => Text_Lexer_count(Text_Quantity_exactly(4n), Text_Lexer_hexDigit())});
});

/* Language.JSON.String.Lexer.simpleEscape : Lexer */
const Language_JSON_String_Lexer_simpleEscape = __lazy(function () {
 return Language_JSON_String_Lexer_esc(Text_Lexer_oneOf('\"\u{5c}/bfnrt'));
});

/* Language.JSON.String.Lexer.quo : Lexer */
const Language_JSON_String_Lexer_quo = __lazy(function () {
 return Text_Lexer_is('\"');
});

/* Language.JSON.String.Lexer.lexString : String -> Maybe (List (WithBounds JSONStringToken)) */
function Language_JSON_String_Lexer_lexString($0) {
 const $1 = Text_Lexer_Core_lex(Language_JSON_String_Lexer_jsonStringTokenMap(), $0);
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case undefined: /* cons */ {
     switch($1.a2.a2.h) {
      case undefined: /* cons */ {
       switch($1.a2.a2.a2) {
        case '': return {a1: $1.a1};
        default: return {h: 0};
       }
      }
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Language.JSON.String.Lexer.legalChar : Lexer */
const Language_JSON_String_Lexer_legalChar = __lazy(function () {
 return Text_Lexer_non(Text_Lexer_Core_x3cx7cx3e(Language_JSON_String_Lexer_quo(), Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('\u{5c}'), Text_Lexer_control())));
});

/* Language.JSON.String.Lexer.jsonStringTokenMap : TokenMap JSONStringToken */
const Language_JSON_String_Lexer_jsonStringTokenMap = __lazy(function () {
 return Text_Lexer_toTokenMap()({a1: {a1: Language_JSON_String_Lexer_quo(), a2: 0}, a2: {a1: {a1: Language_JSON_String_Lexer_unicodeEscape(), a2: 3}, a2: {a1: {a1: Language_JSON_String_Lexer_simpleEscape(), a2: 2}, a2: {a1: {a1: Language_JSON_String_Lexer_legalChar(), a2: 1}, a2: {h: 0}}}}});
});

/* Language.JSON.String.Lexer.esc : Lexer -> Lexer */
function Language_JSON_String_Lexer_esc($0) {
 return Text_Lexer_escape(Text_Lexer_is('\u{5c}'), $0);
}

/* Language.JSON.Lexer.numberLit : Lexer */
const Language_JSON_Lexer_numberLit = __lazy(function () {
 const $0 = Text_Lexer_is('-');
 const $3 = Text_Lexer_Core_x3cx7cx3e(Text_Lexer_is('0'), {h: 4 /* SeqEat */, a1: Text_Lexer_range('1', '9'), a2: () => Text_Lexer_many(Text_Lexer_digit())});
 const $11 = {h: 4 /* SeqEat */, a1: Text_Lexer_is('.'), a2: () => Text_Lexer_digits()};
 const $17 = {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: Text_Lexer_like('e'), a2: () => Text_Lexer_opt(Text_Lexer_oneOf('+-'))}, a2: () => Text_Lexer_digits()};
 return {h: 4 /* SeqEat */, a1: {h: 4 /* SeqEat */, a1: {h: 5 /* SeqEmpty */, a1: Text_Lexer_opt($0), a2: $3}, a2: () => Text_Lexer_opt($11)}, a2: () => Text_Lexer_opt($17)};
});

/* Language.JSON.Lexer.lexJSON : String -> Maybe (List (WithBounds JSONToken)) */
function Language_JSON_Lexer_lexJSON($0) {
 const $1 = Text_Lexer_Core_lex(Language_JSON_Lexer_jsonTokenMap(), $0);
 switch($1.h) {
  case undefined: /* cons */ {
   switch($1.a2.h) {
    case undefined: /* cons */ {
     switch($1.a2.a2.h) {
      case undefined: /* cons */ {
       switch($1.a2.a2.a2) {
        case '': return {a1: $1.a1};
        default: return {h: 0};
       }
      }
      default: return {h: 0};
     }
    }
    default: return {h: 0};
   }
  }
  default: return {h: 0};
 }
}

/* Language.JSON.Lexer.jsonTokenMap : TokenMap JSONToken */
const Language_JSON_Lexer_jsonTokenMap = __lazy(function () {
 return Text_Lexer_toTokenMap()({a1: {a1: Text_Lexer_spaces(), a2: {h: 5 /* JTIgnore */}}, a2: {a1: {a1: Text_Lexer_is(','), a2: {h: 4 /* JTPunct */, a1: {h: 0 /* Comma */}}}, a2: {a1: {a1: Text_Lexer_is(':'), a2: {h: 4 /* JTPunct */, a1: {h: 1 /* Colon */}}}, a2: {a1: {a1: Text_Lexer_is('['), a2: {h: 4 /* JTPunct */, a1: {h: 2 /* Square */, a1: 0}}}, a2: {a1: {a1: Text_Lexer_is(']'), a2: {h: 4 /* JTPunct */, a1: {h: 2 /* Square */, a1: 1}}}, a2: {a1: {a1: Text_Lexer_is('{'), a2: {h: 4 /* JTPunct */, a1: {h: 3 /* Curly */, a1: 0}}}, a2: {a1: {a1: Text_Lexer_is('}'), a2: {h: 4 /* JTPunct */, a1: {h: 3 /* Curly */, a1: 1}}}, a2: {a1: {a1: Text_Lexer_exact('null'), a2: {h: 3 /* JTNull */}}, a2: {a1: {a1: Text_Lexer_Core_x3cx7cx3e(Text_Lexer_exact('true'), Text_Lexer_exact('false')), a2: {h: 0 /* JTBoolean */}}, a2: {a1: {a1: Language_JSON_Lexer_numberLit(), a2: {h: 1 /* JTNumber */}}, a2: {a1: {a1: Language_JSON_String_permissiveStringLit(), a2: {h: 2 /* JTString */}}, a2: {h: 0}}}}}}}}}}}});
});

/* IfuiServer.IOStream.onErrPrint : IOStream (Either String a) -> IOStream a */
function IfuiServer_IOStream_onErrPrint($0, $1) {
 const $3 = $0;
 const $2 = $3;
 const $4 = z => {
  switch(z.h) {
   case 0: /* Left */ return Prelude_IO_putStrLn(csegen_168(), z.a1);
   case 1: /* Right */ return $1(z.a1);
  }
 };
 return $2($4);
}

/* IfuiServer.Promise.pure */
function IfuiServer_Promise_pure_Applicative_Promise($0, $1) {
 return Prelude_Interfaces_x3ex3e(csegen_90(), $1($0), () => $9 => $a => (undefined));
}

/* IfuiServer.Promise.>>= */
function IfuiServer_Promise_x3ex3ex3d_Monad_Promise($0, $1, $2) {
 const $a = h => {
  const $e = $0;
  const $d = $e;
  const $f = z => {
   const $13 = $1(z);
   const $12 = $13;
   const $11 = $12($2);
   const $17 = h2 => {
    const $18 = h;
    return $19 => ($18.value=h2);
   };
   return PrimIO_io_bind($11, $17);
  };
  const $c = $d($f);
  const $1e = h1 => {
   const $23 = h;
   const $22 = $24 => ($23.value=h1);
   return Prelude_Interfaces_x3ex3e(csegen_90(), $22, () => $2a => {
    const $2d = h;
    const $2c = $2e => ($2d.value);
    const $32 = h_ => {
     const $33 = h_;
     return $33;
    };
    return PrimIO_io_bind($2c, $32);
   });
  };
  return PrimIO_io_bind($c, $1e);
 };
 return PrimIO_io_bind(Data_IORef_newIORef(csegen_168(), $9 => (undefined)), $a);
}

/* IfuiServer.Promise.onErrPrint : Promise (Either String a) -> Promise a */
function IfuiServer_Promise_onErrPrint($0, $1) {
 const $3 = $0;
 const $2 = $3;
 const $4 = z => {
  switch(z.h) {
   case 0: /* Left */ return Prelude_IO_putStrLn(csegen_168(), z.a1);
   case 1: /* Right */ return $1(z.a1);
  }
 };
 return $2($4);
}

/* IfuiServer.Server.case block in case block in startWsServerWithAuth */
function IfuiServer_Server_case__casex20blockx20inx20startWsServerWithAuth_3620($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a) {
 switch($a.h) {
  case undefined: /* just */ {
   const $c = Builtin_fst($0);
   const $f = $c.a2($a.a1);
   switch($f.h) {
    case undefined: /* just */ {
     const $15 = $2($f.a1);
     const $14 = $15;
     const $18 = w => {
      const $1d = $6;
      const $1c = $1e => ($1d.value={a1: w});
      const $24 = () => {
       const $2a = Builtin_snd($0);
       const $29 = $2a.a3(w);
       return IfuiServer_WebSockets_wsSend(csegen_168(), $5, $29);
      };
      return Prelude_Interfaces_x3ex3e(csegen_90(), $1c, $24);
     };
     const $13 = $14($18);
     return PrimIO_io_bind($13, _ => $30 => (undefined));
    }
    case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid login info ', a2: {a1: $8, a2: {h: 0}}}));
   }
  }
  case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid login info ', a2: {a1: $8, a2: {h: 0}}}));
 }
}

/* IfuiServer.Server.case block in case block in onMessageFn */
function IfuiServer_Server_case__casex20blockx20inx20onMessageFn_3034($0, $1, $2, $3, $4, $5, $6, $7) {
 switch($7.h) {
  case undefined: /* just */ {
   switch($7.a1.a1) {
    case 0: {
     const $c = Builtin_fst($7.a1.a2.a1);
     const $f = $c.a2($6);
     switch($f.h) {
      case undefined: /* just */ {
       const $15 = $7.a1.a2.a3($f.a1);
       const $14 = $15;
       const $18 = z => {
        const $20 = () => {
         const $2d = Builtin_snd($7.a1.a2.a1);
         const $2c = $2d.a1(z);
         const $2b = {a1: $2c, a2: {h: 0}};
         const $28 = {a1: {h: 3 /* JString */, a1: $5}, a2: $2b};
         const $27 = {h: 4 /* JArray */, a1: $28};
         const $25 = Language_JSON_Data_show_Show_JSON($27);
         return IfuiServer_WebSockets_wsSend(csegen_168(), $1, $25);
        };
        return Prelude_Interfaces_x3ex3e(csegen_90(), IfuiServer_Server_removeHandle($5, $3), $20);
       };
       const $13 = $14($18);
       const $33 = promiseh => {
        const $36 = $3;
        const $35 = $37 => ($36.value);
        const $3b = h => {
         const $3c = $3;
         return $3d => {
          const $44 = promiseh;
          const $43 = $44;
          const $41 = {a1: $5, a2: $43};
          const $40 = {a1: $41, a2: h};
          return ($3c.value=$40);
         };
        };
        return PrimIO_io_bind($35, $3b);
       };
       return PrimIO_io_bind($13, $33);
      }
      case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid Input to service ', a2: {a1: $4, a2: {a1: ' ', a2: {a1: Language_JSON_Data_show_Show_JSON($6), a2: {h: 0}}}}}));
     }
    }
    case 1: {
     const $5c = Builtin_fst($7.a1.a2.a1);
     const $5f = $5c.a2($6);
     switch($5f.h) {
      case undefined: /* just */ {
       const $65 = $7.a1.a2.a3($5f.a1);
       const $64 = $65;
       const $68 = z => {
        const $75 = Builtin_snd($7.a1.a2.a1);
        const $74 = $75.a1(z);
        const $73 = {a1: $74, a2: {h: 0}};
        const $70 = {a1: {h: 3 /* JString */, a1: $5}, a2: $73};
        const $6f = {h: 4 /* JArray */, a1: $70};
        const $6d = Language_JSON_Data_show_Show_JSON($6f);
        return IfuiServer_WebSockets_wsSend(csegen_168(), $1, $6d);
       };
       const $63 = $64($68);
       const $7b = streamh => {
        const $7e = $3;
        const $7d = $7f => ($7e.value);
        const $83 = h => {
         const $84 = $3;
         return $85 => {
          const $8c = streamh;
          const $8b = $8c;
          const $89 = {a1: $5, a2: $8b};
          const $88 = {a1: $89, a2: h};
          return ($84.value=$88);
         };
        };
        return PrimIO_io_bind($7d, $83);
       };
       return PrimIO_io_bind($63, $7b);
      }
      case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid Input to service ', a2: {a1: $4, a2: {a1: ' ', a2: {a1: Language_JSON_Data_show_Show_JSON($6), a2: {h: 0}}}}}));
     }
    }
   }
  }
  case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid Service ', a2: {a1: $4, a2: {h: 0}}}));
 }
}

/* IfuiServer.Server.case block in onMessageFn */
function IfuiServer_Server_case__onMessageFn_2916($0, $1, $2, $3, $4) {
 switch($4.h) {
  case undefined: /* just */ {
   switch($4.a1.h) {
    case 4: /* JArray */ {
     switch($4.a1.a1.h) {
      case undefined: /* cons */ {
       switch($4.a1.a1.a1.h) {
        case 3: /* JString */ {
         switch($4.a1.a1.a1.a1) {
          case 'cancel': {
           switch($4.a1.a1.a2.h) {
            case undefined: /* cons */ {
             switch($4.a1.a1.a2.a1.h) {
              case 3: /* JString */ {
               switch($4.a1.a1.a2.a2.h) {
                case 0: /* nil */ {
                 const $f = $3;
                 const $e = $10 => ($f.value);
                 const $14 = $15 => {
                  const $16 = Data_List_lookup(csegen_166(), $4.a1.a1.a2.a1.a1, $15);
                  switch($16.h) {
                   case 0: /* nothing */ return $1c => (undefined);
                   case undefined: /* just */ return Prelude_Interfaces_x3ex3e(csegen_90(), IfuiServer_Server_removeHandle($4.a1.a1.a2.a1.a1, $3), () => $16.a1);
                  }
                 };
                 return PrimIO_io_bind($e, $14);
                }
                default: {
                 switch($4.a1.a1.a2.h) {
                  case undefined: /* cons */ {
                   switch($4.a1.a1.a2.a1.h) {
                    case 3: /* JString */ {
                     switch($4.a1.a1.a2.a2.h) {
                      case undefined: /* cons */ {
                       switch($4.a1.a1.a2.a2.a2.h) {
                        case 0: /* nil */ return IfuiServer_Server_case__casex20blockx20inx20onMessageFn_3034($0, $1, $2, $3, $4.a1.a1.a1.a1, $4.a1.a1.a2.a1.a1, $4.a1.a1.a2.a2.a1, IfuiServer_Server_getServiceU($4.a1.a1.a1.a1, $2));
                        default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                       }
                      }
                      default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                     }
                    }
                    default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                   }
                  }
                  default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                 }
                }
               }
              }
              default: {
               switch($4.a1.a1.a2.h) {
                case undefined: /* cons */ {
                 switch($4.a1.a1.a2.a1.h) {
                  case 3: /* JString */ {
                   switch($4.a1.a1.a2.a2.h) {
                    case undefined: /* cons */ {
                     switch($4.a1.a1.a2.a2.a2.h) {
                      case 0: /* nil */ return IfuiServer_Server_case__casex20blockx20inx20onMessageFn_3034($0, $1, $2, $3, $4.a1.a1.a1.a1, $4.a1.a1.a2.a1.a1, $4.a1.a1.a2.a2.a1, IfuiServer_Server_getServiceU($4.a1.a1.a1.a1, $2));
                      default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                     }
                    }
                    default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                   }
                  }
                  default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                 }
                }
                default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
               }
              }
             }
            }
            default: {
             switch($4.a1.a1.a2.h) {
              case undefined: /* cons */ {
               switch($4.a1.a1.a2.a1.h) {
                case 3: /* JString */ {
                 switch($4.a1.a1.a2.a2.h) {
                  case undefined: /* cons */ {
                   switch($4.a1.a1.a2.a2.a2.h) {
                    case 0: /* nil */ return IfuiServer_Server_case__casex20blockx20inx20onMessageFn_3034($0, $1, $2, $3, $4.a1.a1.a1.a1, $4.a1.a1.a2.a1.a1, $4.a1.a1.a2.a2.a1, IfuiServer_Server_getServiceU($4.a1.a1.a1.a1, $2));
                    default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                   }
                  }
                  default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                 }
                }
                default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
               }
              }
              default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
             }
            }
           }
          }
          default: {
           switch($4.a1.a1.a2.h) {
            case undefined: /* cons */ {
             switch($4.a1.a1.a2.a1.h) {
              case 3: /* JString */ {
               switch($4.a1.a1.a2.a2.h) {
                case undefined: /* cons */ {
                 switch($4.a1.a1.a2.a2.a2.h) {
                  case 0: /* nil */ return IfuiServer_Server_case__casex20blockx20inx20onMessageFn_3034($0, $1, $2, $3, $4.a1.a1.a1.a1, $4.a1.a1.a2.a1.a1, $4.a1.a1.a2.a2.a1, IfuiServer_Server_getServiceU($4.a1.a1.a1.a1, $2));
                  default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
                 }
                }
                default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
               }
              }
              default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
             }
            }
            default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
           }
          }
         }
        }
        default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
       }
      }
      default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
     }
    }
    default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
   }
  }
  case 0: /* nothing */ return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Error parsing request ', a2: {a1: $0, a2: {h: 0}}}));
  default: return Prelude_IO_putStrLn(csegen_168(), Prelude_Interfaces_concat(csegen_92(), csegen_107(), {a1: 'Invalid request ', a2: {a1: Prelude_Show_show_Show_x28Maybex20x24ax29(csegen_389(), $4), a2: {h: 0}}}));
 }
}

/* IfuiServer.Server.startWsServerWithAuth : (JsonSerializable l, JsonSerializable r) =>
Int -> ServerWithAuth l r sf -> IO () */
function IfuiServer_Server_startWsServerWithAuth($0, $1, $2) {
 const $a = wss => {
  const $d = wsc => {
   const $11 = roleRef => {
    const $18 = cancelHandles => {
     const $1b = msg => {
      const $1e = roleRef;
      const $1d = $1f => ($1e.value);
      const $23 = mrole => {
       switch(mrole.h) {
        case 0: /* nothing */ {
         const $25 = {h: 0};
         return IfuiServer_Server_case__casex20blockx20inx20startWsServerWithAuth_3620($0, $2.a2, $2.a1, $1, wss, wsc, roleRef, cancelHandles, msg, $25, Language_JSON_parse(msg));
        }
        case undefined: /* just */ return IfuiServer_Server_onMessageFn(cancelHandles, $2.a2(mrole.a1), wsc, msg);
       }
      };
      return PrimIO_io_bind($1d, $23);
     };
     return IfuiServer_WebSockets_setOnMessageStr(wsc, $1b);
    };
    return PrimIO_io_bind(Data_IORef_newIORef(csegen_168(), {h: 0}), $18);
   };
   return PrimIO_io_bind(csegen_169(), $11);
  };
  return IfuiServer_WebSockets_setOnConnection(wss, $d);
 };
 return PrimIO_io_bind(IfuiServer_WebSockets_startWebSocketsServer(csegen_168(), $1), $a);
}

/* IfuiServer.Server.serveStatic : Int -> String -> IO () */
function IfuiServer_Server_serveStatic($0, $1) {
 return PrimIO_io_bind(IfuiServer_Http_createStaticServer(csegen_168(), $1), staticServer => PrimIO_io_bind(IfuiServer_Http_createHttpServer(csegen_168(), req => res => IfuiServer_Http_serve(csegen_168(), staticServer, req, res)), s => IfuiServer_Http_listen(csegen_168(), s, $0)));
}

/* IfuiServer.Server.removeHandle : String -> IORef (List (String, a)) -> IO () */
function IfuiServer_Server_removeHandle($0, $1) {
 const $4 = $1;
 const $3 = $5 => ($4.value);
 const $9 = h => {
  const $a = $1;
  return $b => ($a.value=Data_List_deleteBy(x => $11 => Prelude_EqOrd_x3dx3d_Eq_String(x, $11.a1), $0, h));
 };
 return PrimIO_io_bind($3, $9);
}

/* IfuiServer.Server.onMessageFn : IORef (List (String, IO ())) -> Server ts -> WsConnection -> String -> IO () */
function IfuiServer_Server_onMessageFn($0, $1, $2, $3) {
 return IfuiServer_Server_case__onMessageFn_2916($3, $2, $1, $0, Language_JSON_parse($3));
}

/* IfuiServer.Http.serve : HasIO io => StaticServer -> Req -> Res -> io () */
function IfuiServer_Http_serve($0, $1, $2, $3) {
 const $4 = $1;
 const $5 = $2;
 const $6 = $3;
 return $0.a2(undefined)($c => IfuiServer_Http_prim__serve($4, $5, $6, $c));
}

/* IfuiServer.Http.listen : HasIO io => HttpServer -> Int -> io () */
function IfuiServer_Http_listen($0, $1, $2) {
 const $3 = $1;
 return $0.a2(undefined)($9 => IfuiServer_Http_prim__listen($3, $2, $9));
}

/* IfuiServer.Http.createStaticServer : HasIO io => String -> io StaticServer */
function IfuiServer_Http_createStaticServer($0, $1) {
 return Prelude_Interfaces_x3cx24x3e($0.a1.a1.a1, $8 => $8, $0.a2(undefined)($f => IfuiServer_Http_prim__createStaticServer($1, $f)));
}

/* IfuiServer.Http.createHttpServer : HasIO io => (Req -> Res -> IO ()) -> io HttpServer */
function IfuiServer_Http_createHttpServer($0, $1) {
 const $e = $f => {
  const $11 = req => res => {
   const $12 = $1(req)(res);
   return $12;
  };
  return IfuiServer_Http_prim__createHttpServer($11, $f);
 };
 const $9 = $0.a2(undefined)($e);
 return Prelude_Interfaces_x3cx24x3e($0.a1.a1.a1, $8 => $8, $9);
}

/* IfuiServer.WebSockets.wsSend : HasIO io => WsConnection -> String -> io () */
function IfuiServer_WebSockets_wsSend($0, $1, $2) {
 const $3 = $1;
 return $0.a2(undefined)($9 => IfuiServer_WebSockets_prim__wsSend($3, $2, $9));
}

/* IfuiServer.WebSockets.startWebSocketsServer : HasIO io => Int -> io WsServer */
function IfuiServer_WebSockets_startWebSocketsServer($0, $1) {
 return Prelude_Interfaces_x3cx24x3e($0.a1.a1.a1, $8 => $8, $0.a2(undefined)($f => IfuiServer_WebSockets_prim__startWebSocketsServer($1, $f)));
}

/* IfuiServer.WebSockets.setOnMessageStr : WsConnection -> (String -> IO ()) -> IO () */
function IfuiServer_WebSockets_setOnMessageStr($0, $1) {
 const $2 = $0;
 return $3 => {
  const $6 = msg => {
   const $7 = $1(msg);
   return $7;
  };
  return IfuiServer_WebSockets_prim__setOnMessageStr($2, $6, $3);
 };
}

/* IfuiServer.WebSockets.setOnConnection : WsServer -> (WsConnection -> IO ()) -> IO () */
function IfuiServer_WebSockets_setOnConnection($0, $1) {
 const $2 = $0;
 return $3 => {
  const $6 = ptr => {
   const $7 = $1(ptr);
   return $7;
  };
  return IfuiServer_WebSockets_prim__setOnConnection($2, $6, $3);
 };
}


try{__mainExpression_0()}catch(e){if(e instanceof IdrisError){console.log('ERROR: ' + e.message)}else{throw e} }
