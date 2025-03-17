  $ riscv64-unknown-elf-as -march=rv64gcv -o ../examples/rvv/rvv.o ../examples/rvv/rvv.s
  $ riscv64-unknown-elf-ld -o ../examples/rvv/rvv ../examples/rvv/rvv.o
  $ qemu-riscv64 ../examples/rvv/rvv <<EOF
  11
  $ ../repl/REPL.exe -eval -filepath="../examples/rvv/rvv.s" <<EOF
  11
