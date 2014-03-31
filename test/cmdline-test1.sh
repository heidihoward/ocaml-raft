cd ~/ocaml-raft

./cmdstart.byte discrete -nodes 4 -d -follower Uniform-150-300 -candidate Uniform-150-300 -leader Fixed-50 -delay Exp-7-1 -termOnTimeout 500 -termOnClient
