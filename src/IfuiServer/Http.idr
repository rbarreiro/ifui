module IfuiServer.Http

export
data HttpServer = MkHttpServer AnyPtr

export
data StaticServer = MkStaticServer AnyPtr

export
data Req = MkReq AnyPtr

export
data Res = MkRes AnyPtr

%foreign "node:lambda: requestListener => require('http').createServer((req, res) => requestListener(req)(res)())"
prim__createHttpServer : (AnyPtr -> AnyPtr -> PrimIO ())  -> PrimIO AnyPtr 
export
createHttpServer : HasIO io => (Req -> Res -> IO ())  -> io HttpServer
createHttpServer requestListener =
  MkHttpServer <$> (primIO $ prim__createHttpServer  (\req, res => toPrim $ requestListener (MkReq req)  (MkRes res)))

%foreign "node:lambda: folder => new(require('node-static').Server)(folder)"
prim__createStaticServer : String -> PrimIO AnyPtr
export
createStaticServer : HasIO io => String -> io StaticServer
createStaticServer folder =
  MkStaticServer <$> (primIO $ prim__createStaticServer folder)

%foreign "node:lambda: (server, port) => server.listen(port)"
prim__listen : AnyPtr -> Int -> PrimIO ()
export
listen : HasIO io => HttpServer -> Int -> io ()
listen (MkHttpServer x) i = 
  primIO $ prim__listen x i

%foreign "node:lambda: (res, i) => res.writeHead(i) "
prim__writeHead : AnyPtr -> Int -> PrimIO ()
export
writeHead : HasIO io => Res -> Int -> io ()
writeHead (MkRes x) i = primIO $ prim__writeHead x i

%foreign "node:lambda: (res, s) => res.end(s) "
prim__end : AnyPtr -> String -> PrimIO ()
export
end : HasIO io => Res -> String -> io ()
end (MkRes x) s = primIO $ prim__end x s

%foreign "node:lambda: (staticServer, req, res) => staticServer.serve(req,res)"
prim__serve : AnyPtr -> AnyPtr -> AnyPtr -> PrimIO ()
export
serve : HasIO io => StaticServer ->  Req -> Res -> io ()
serve (MkStaticServer x) (MkReq y) (MkRes z) =
  primIO $ prim__serve x y z
