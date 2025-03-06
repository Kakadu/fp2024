  $ riscv64-unknown-elf-as -o ../examples/io/io.o ../examples/io/io.s
  $ riscv64-unknown-elf-ld -o ../examples/io/io ../examples/io/io.o
  $ qemu-riscv64 ../examples/io/io <<EOF
  > 5
  5
  $ ../repl/REPL.exe -eval -filepath="../examples/io/io.s" <<EOF
  > 5
  5
