module Ifui.Pdfjs

import public Ifui.Promise

export
data Pdfjs = MkPdfjs AnyPtr

%foreign "browser:lambda: () => pdfjsLib = window['pdfjs-dist/build/pdf']"
prim__getPdfjsLib : () -> PrimIO AnyPtr 

%foreign "browser:lambda: x => x.GlobalWorkerOptions.workerSrc = '//mozilla.github.io/pdf.js/build/pdf.worker.js'"
prim__setWorker : AnyPtr -> PrimIO ()

%foreign "browser:lambda: (pdfjsLib, url, callback) => pdfjsLib.getDocument(url).then(callback) "
prim__getDocumentUrl : AnyPtr -> String -> (AnyPtr -> PrimIO ()) -> PrimIO ()

%foreign "browser:lambda: x => x.numPages"
prim__numPages : AnyPtr -> Int

export
loadPDFFromUrl : String -> Promise Pdfjs
loadPDFFromUrl url = 
  MkPromise $ \callback => 
    do
      pdfjsLib <- primIO $ prim__getPdfjsLib ()
      primIO $ prim__setWorker pdfjsLib
      primIO $ prim__getDocumentUrl pdfjsLib url (\ptr => toPrim $ callback $ MkPdfjs ptr)
      pure $ MkPromiseHandler (pure ())

export
numPages : Pdfjs -> Nat
numPages (MkPdfjs x) = cast $ prim__numPages x
