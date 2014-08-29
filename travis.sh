#!/usr/bin/env bash
ppa=avsm/ocaml42+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam 

export OPAMYES=1
opam init
opam install core async ounit
eval `opam config env`

# spl
# git clone https://github.com/avsm/melange.git
# cd melange/tools/spl
# make all
# cd ~

# build ocaml-raft
./build.sh


# testing ocaml-raft
for i in {1..20}
do
   echo "Test $i "
./config.byte
./cmdstart.byte discrete -termOnClient -nodes 2 -follower Uniform-150-300 -candidate Fixed-50 -leader Fixed-50 -delay Fixed-6 -failure Uniform-400-500 -recover Uniform-5-10 -d > output.log
./cmdstart.byte discrete -nodes 30  -follower Uniform-150-155 -candidate Uniform-11-22 -leader Fixed-75 -delay Fixed-7 -termOnTimeout 50000 -termOnElec -cmds 0 -backoff -d > output.log
./cmdstart.byte discrete -termOnClient -nodes 5 -follower Uniform-150-300 -candidate Uniform-150-300 -leader Fixed-75 -delay Normal-7-3 -d > output.log
done 


# unit testing
./test_splaytree.byte
./test_spl.byte


