FROM ubuntu

WORKDIR /src
RUN apt-get update && apt-get -y install build-essential git clang chezscheme libgmp3-dev
RUN git clone https://github.com/idris-lang/Idris2.git
WORKDIR /src/Idris2
RUN git checkout v0.5.1
ENV SCHEME=scheme
ENV PATH="${PATH}:/root/.idris2/bin"
RUN make bootstrap && make install && make clean && make all && make install && make install-api
RUN echo 'export PATH="/root/.idris2/bin/:${PATH}"' >> "${HOME}/.bashrc" && echo 'alias idris=idris2' >> "${HOME}/.bashrc"
RUN apt-get update && apt-get -y install nodejs vim curl
RUN mkdir -p ~/.vim/autoload ~/.vim/bundle && curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
RUN mkdir -p /root/.vim/bundle && cd /root/.vim/bundle && git clone https://github.com/edwinb/idris2-vim.git
COPY .vimrc /root
WORKDIR /app
CMD ["bash"]