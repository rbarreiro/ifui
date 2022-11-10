all: frontend backend

backend: build-backend
	cp build/exec/backend.js .

frontend: build-frontend
	cp build/exec/frontend.js www/

build-backend:
	idris2 -p ifui -p contrib --codegen node backend.idr -o backend.js

build-frontend:
	idris2 -p ifui -p contrib --codegen javascript frontend.idr -o frontend.js


