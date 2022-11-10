FROM node

RUN apt-get update && apt-get -y install build-essential git clang chezscheme libgmp3-dev vim curl
WORKDIR /deps
RUN git clone https://github.com/idris-lang/Idris2.git
WORKDIR /deps/Idris2
RUN git checkout v0.6.0
ENV SCHEME=scheme
ENV PATH="${PATH}:/root/.idris2/bin"
RUN make bootstrap && make install && make clean && make all && make install && make install-api
RUN echo 'export PATH="/root/.idris2/bin/:${PATH}"' >> "${HOME}/.bashrc" && echo 'alias idris=idris2' >> "${HOME}/.bashrc"
RUN echo 'export NODE_PATH="`npm root -g`"' >> "${HOME}/.bashrc"
RUN npm install -g node-static
WORKDIR /deps
ARG CACHE_DATE=2022-11-01
RUN git clone https://github.com/rbarreiro/ifui.git
WORKDIR /deps/ifui
RUN idris2  --install ifui.ipkg
WORKDIR /app
COPY . .
RUN make
CMD export NODE_PATH="`npm root -g`";node backend.js