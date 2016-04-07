# Required FPGA development board

This design is intended to be instantiated on the [Terasic/Altera DE1 Cyclone II development board] (http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=53&No=83)

# How to generate a bit-file:

- `clash --vhdl DE1.hs`
- Start Quartus II (no higher than 13.0sp1)
- Open the `DE1TOP/DE1TOP.qpf` project
- Add all `*.vhdl` files from `vhdl/DE1` to the project
- Compile (Ctrl+L)
