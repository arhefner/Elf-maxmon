# Build a combined ROM image containing the BIOS initialization (at INIT),
# the monitor (at MONSTART), and the resident BIOS ($F800-$FFFF).
#
#   make            build maxrom.hex and maxrom.bin
#   make monitor    build only the monitor (max_mon.hex)
#   make test       build a test image to load into RAM (build-test/test.hex)
#   make clean      remove build products
#
# The test image has the initialization at TEST_INIT, the monitor at
# TEST_MON, and the resident BIOS at TEST_BIOS, and does not switch the
# banked RAM. Load it with the existing monitor and run it at TEST_INIT.
# Programs booted from it still call the BIOS at $F800-$FFFF.
#
# The BIOS source is taken from MBIOS; it is copied into the build
# directory to assemble so that no build products are left behind there.

ASM      ?= asm02
PYTHON   ?= python3

MBIOS    ?= ../EDOS-mbios
CONFIG   ?= 1802MAX
INIT     ?= 8000h
MONSTART ?= 0f000h

TEST_INIT ?= 9000
TEST_MON  ?= b000
TEST_BIOS ?= b800
TEST_DEFS ?=

BUILD    := build
TBUILD   := build-test
ROM      := maxrom

MONSRC   := max_mon.asm max_mon.inc bios.inc sysconfig.inc opcodes.def

all: $(ROM).hex

monitor: max_mon.hex

$(ROM).hex $(ROM).bin: $(BUILD)/mbios.hex max_mon.hex mkrom.py
	$(PYTHON) mkrom.py -o $(ROM) $(BUILD)/mbios.hex max_mon.hex

max_mon.hex: $(MONSRC)
	$(ASM) -L -i -D$(CONFIG) -DMONSTART=$(MONSTART) max_mon.asm

$(BUILD)/mbios.hex: $(MBIOS)/mbios.asm $(MBIOS)/sysconfig.inc $(wildcard $(MBIOS)/fast_uart*.asm) | $(BUILD)
	cp $(MBIOS)/mbios.asm $(BUILD)/mbios.asm
	cd $(BUILD) && $(ASM) -L -i -I$(abspath $(MBIOS)) -D$(CONFIG) \
	  -DINIT=$(INIT) -DMONITOR=$(MONSTART) mbios.asm

$(BUILD) $(TBUILD):
	mkdir -p $@

test: $(TBUILD)/test.hex

$(TBUILD)/test.hex: $(TBUILD)/mbios.hex $(TBUILD)/max_mon.hex mkrom.py
	$(PYTHON) mkrom.py --bios $(TEST_BIOS) -o $(TBUILD)/test \
	  $(TBUILD)/mbios.hex $(TBUILD)/max_mon.hex

$(TBUILD)/max_mon.hex: $(MONSRC) | $(TBUILD)
	cp max_mon.asm $(TBUILD)/max_mon.asm
	cd $(TBUILD) && $(ASM) -L -i -I$(CURDIR) -D$(CONFIG) \
	  -DMONSTART=0$(TEST_MON)h -DEBIOS=0$(TEST_BIOS)h \
	  -DBIOS=0$(TEST_BIOS)h+0700h max_mon.asm

$(TBUILD)/mbios.hex: $(MBIOS)/mbios.asm $(MBIOS)/sysconfig.inc $(wildcard $(MBIOS)/fast_uart*.asm) | $(TBUILD)
	cp $(MBIOS)/mbios.asm $(TBUILD)/mbios.asm
	cd $(TBUILD) && $(ASM) -L -i -I$(abspath $(MBIOS)) -D$(CONFIG) \
	  -DINIT=0$(TEST_INIT)h -DMONITOR=0$(TEST_MON)h \
	  -DRESIDENT=0$(TEST_BIOS)h -DNO_EXP_MEMORY $(TEST_DEFS) mbios.asm

clean:
	rm -rf $(BUILD) $(TBUILD) $(ROM).hex $(ROM).bin max_mon.hex \
	  max_mon.lst max_mon.build

.PHONY: all monitor test clean
