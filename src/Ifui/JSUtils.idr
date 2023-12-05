module Ifui.JSUtils

%foreign "javascript:lambda: x => {throw x}"
prim__throw : AnyPtr -> PrimIO b
export
throw : HasIO io => a -> io b
throw x = primIO $ prim__throw (believe_me x)

%foreign "javascript:lambda:(callback, delay)=>setTimeout(callback, delay)"
prim__setTimeout : (PrimIO ()) -> Int -> PrimIO ()
export
setTimeout : HasIO io => IO () -> Int -> io ()
setTimeout callback delay = primIO $ prim__setTimeout (toPrim callback) delay
