rm -r _build/*
rm *.{byte,native}


corebuild -Is lib,test,spl spl/raftMonitor.byte spl/raftMonitorWrapper.byte lib/common.byte lib/client.byte lib/eventlst.byte lib/splaytree.byte lib/rpcs.byte lib/statemach.byte lib/clock.byte lib/env.byte lib/simulator.byte lib/parser.byte lib/cmdstart.byte

corebuild -Is lib,test,spl test/test_splaytree.byte test/test_spl.byte test/test_spl_wrapper.byte
