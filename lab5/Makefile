SRC = $(wildcard src/*.c)
INPUT ?= $(wildcard 447inputs/*/*.x)

.PHONY: all verify clean

all: sim

sim: $(SRC)
	gcc -std=c99 -g -O2 $^ -o $@

verify: sim
	@./verify $(INPUT)

clean:
	rm -rf *.o *~ sim
