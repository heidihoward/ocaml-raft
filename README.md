ocaml-raft Simulator
==========
[![Build Status](https://travis-ci.org/heidi-ann/ocaml-raft.svg?branch=master)](https://travis-ci.org/heidi-ann/ocaml-raft)

This is a complete implementation of base Raft consensus algorithm (not inc. membership changes and log compation). This project is described in detail in this [tech report](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-857.pdf)

The simulator runs as either a discrete event simulator or a realtime simulator, in a range of network conditions. 

To build run ```build.sh  ``` and to learn how to use the command line run ``` ./cmdstart.byte --help ```.
If you are familiar with OCaml, using an OCaml configure file for your simualation is your best bet.

### JS trace viewer ###
The following is an example of how to generate a trace from the command line and run through the output in javascript
```bash
$ cd ocaml-raft
$ make
$ /cmdstart.byte discrete -nodes 3  -follower Uniform-150-155 -candidate Uniform-11-22 -leader Fixed-75 -delay Fixed-7 -termOnTimeout 50000 -termOnElec -cmds 0 -backoff -json > output.json
$ cat output.json | python vis/parseJSON.py > vis/data.json
$ cd vis
$ python -m SimpleHTTPServer 8000
```
