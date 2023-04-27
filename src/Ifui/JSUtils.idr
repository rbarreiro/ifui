module Ifui.JSUtils

%foreign "javascript:lambda: x => {throw x}"
prim__throw : AnyPtr -> PrimIO b
export
throw : HasIO io => a -> io b
throw x = primIO $ prim__throw (believe_me x)

