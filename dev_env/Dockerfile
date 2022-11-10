FROM node

WORKDIR /src
RUN apt-get update && apt-get -y install build-essential git clang chezscheme libgmp3-dev vim curl
RUN git clone https://github.com/idris-lang/Idris2.git
WORKDIR /src/Idris2
RUN git checkout v0.6.0
ENV SCHEME=scheme
ENV PATH="${PATH}:/root/.idris2/bin"
RUN make bootstrap && make install && make clean && make all && make install && make install-api
RUN echo 'export PATH="/root/.idris2/bin/:${PATH}"' >> "${HOME}/.bashrc" && echo 'alias idris=idris2' >> "${HOME}/.bashrc"
RUN mkdir -p ~/.vim/autoload ~/.vim/bundle && curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
RUN mkdir -p /root/.vim/bundle && cd /root/.vim/bundle && git clone https://github.com/edwinb/idris2-vim.git
COPY .vimrc /root
RUN echo 'export NODE_PATH="`npm root -g`"' >> "${HOME}/.bashrc"
RUN npm install -g node-static
RUN npm install -g ws
WORKDIR /app
CMD ["bash"]
