64tass -a ./src/edit128.asm -l ./target/edit128.lst -o ./target/edit128

c1541 -format "edit128,sh" d64 ./target/edit128.d64
c1541 -attach ./target/edit128.d64 -write ./target/edit128 edit128