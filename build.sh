rm *.log
rm *.{byte,native}

corebuild role.byte
corebuild lg.byte
corebuild state.byte
corebuild node.byte

