rm *.data
rm *.{byte,native}

corebuild common.byte
corebuild env.byte
corebuild clock.byte
corebuild simulator.byte
corebuild cmdstart.byte

