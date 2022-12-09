idris2  --install ifui.ipkg
idris2 -p contrib -p ifui --codegen javascript examples/counter.idr -o counter.html
idris2 -p contrib -p ifui --codegen javascript examples/bulma.idr -o bulma.js
