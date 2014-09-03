ocaml-raft Simulator
==========
[![Build Status](https://travis-ci.org/heidi-ann/ocaml-raft.svg?branch=master)](https://travis-ci.org/heidi-ann/ocaml-raft)

This is a complete implementation of base Raft consensus algorithm (not inc. membership changes and log compation). This project is described in detail in this [tech report](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-857.pdf)

The simulator runs as either a discrete event simulator or a realtime simulator, in a range of network conditions. 

To build run ```build.sh  ``` and to learn how to use the command line run ``` ./cmdstart.byte --help ```.
If you are familiar with OCaml, using an OCaml configure file for your simualation is your best bet.
