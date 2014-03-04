cd lib

rm _build/*
rm *.{byte,native}


corebuild common.byte client.byte eventlst.byte rpcs.byte statemach.byte clock.byte env.byte simulator.byte cmdstart.byte

