module Ifui.Window

%foreign "browser:lambda: () => window.location.hostname"
prim__getLocationHostname :() -> PrimIO String
export
getLocationHostname : HasIO io => io String
getLocationHostname = primIO $ prim__getLocationHostname ()
