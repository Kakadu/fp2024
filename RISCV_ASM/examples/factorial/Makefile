CC=riscv64-linux-gnu-gcc
CFLAGS=-S -O2 -Wall -Wextra -pedantic -static

QEMU=qemu-riscv64

TARGET=factorial

SRC=factorial.c
ASM=factorial.s

all: $(TARGET)

$(ASM): $(SRC)
	@$(CC) $(CFLAGS) $<

$(TARGET): $(ASM)
	@$(CC) -o $@ $< -static

run: $(TARGET)
	$(QEMU) ./$(TARGET)

clean:
	@rm -f $(ASM) $(TARGET)
