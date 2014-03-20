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


# testing ocaml-raft
./cmdstart.byte discrete -termOnClient -nodes 2 -d -follower Uniform-150-300 -candidate Fixed-50 -leader Fixed-50 -delay Fixed-6
./cmdstart.byte discrete -termOnClient -nodes 2 -d -follower Uniform-150-300 -candidate Fixed-50 -leader Fixed-50 -delay Fixed-6 -failure Uniform-400-500 -recover Uniform-5-10

