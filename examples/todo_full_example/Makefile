all: frontend backend

backend:
	idris2 -p ifui -p contrib --codegen node src/backend.idr -o backend.js

frontend: build-frontend
	cp build/exec/frontend.js www/

build-frontend:
	idris2 --codegen javascript src/frontend.idr -o frontend.js


