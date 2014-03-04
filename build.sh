rm -r _build/*
rm *.{byte,native}


corebuild -Is lib,test,slp lib/common.byte lib/client.byte lib/eventlst.byte lib/splaytree.byte lib/rpcs.byte lib/statemach.byte lib/clock.byte lib/env.byte lib/simulator.byte lib/cmdstart.byte

corebuild -Is lib,test test/test_splaytree.byte
