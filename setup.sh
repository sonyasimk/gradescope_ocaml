#!/usr/bin/env bash

add-apt-repository ppa:avsm/ppa
apt update

apt install make
apt-get install -y \
    libgmp-dev \
    m4 \
    pkg-config \
    opam \

opam init --disable-sandboxing
opam switch create 5.3.0

eval $(opam env --switch=5.3.0)

opam install -y dune atdgen ounit2 qcheck zarith ppx_jane ppx_deriving
cd /autograder/source
dune build util/ --profile release
