#!/usr/bin/env bash
ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
opam init
opam install core async
eval `opam config env`

# build ocaml-raft
./build.sh

