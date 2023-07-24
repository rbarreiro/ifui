module Ifui.Pdfjs

import Ifui.Widget

export
data Pdfjs = MkPdfjs AnyPtr

%foreign "browser:lambda: () => pdfjsLib = window['pdfjs-dist/build/pdf']"
prim__getPdfjsLib : () -> PrimIO AnyPtr 

%foreign "browser:lambda: x => x.GlobalWorkerOptions.workerSrc = '//mozilla.github.io/pdf.js/build/pdf.worker.js'"
prim__setWorker : AnyPtr -> PrimIO AnyPtr 

export
loadPDFFromUrl : String -> Widget Pdfjs
loadPDFFromUrl x = ?arsd
