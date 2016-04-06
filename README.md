# How to generate a bit-file:

- `clash --vhdl DE1.hs`
- Start Quartus II (no higher than 13.0sp1)
- Open the `DE1TOP/DE1TOP.qpf` project
- Add all `*.vhdl` files from `vhdl/DE1` to the project
- Compile (Ctrl+L)
