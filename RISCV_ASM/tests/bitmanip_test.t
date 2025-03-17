  $ riscv64-unknown-elf-as -march=rv64gc_zbb_zba -o ../examples/bitmanip/bitmanip.o ../examples/bitmanip/bitmanip.s
  $ riscv64-unknown-elf-ld -o ../examples/bitmanip/bitmanip ../examples/bitmanip/bitmanip.o
  $ qemu-riscv64 ../examples/bitmanip/bitmanip <<EOF
  > 5351
  5631
  $ ../repl/REPL.exe -eval -filepath="../examples/bitmanip/bitmanip.s" <<EOF
  > 5351
  5631
