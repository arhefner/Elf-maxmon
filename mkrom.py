#!/usr/bin/env python3
"""Combine Intel hex files into a single ROM image for $8000-$FFFF.

The inputs (initialization, monitor, and BIOS) are merged, checking that
none of them overlap and that all lie within the ROM. The BIOS checksum
bytes at $FFFC-$FFFF are then set so that the checksum verified by the
BIOS initialization at reset passes.

Two outputs are written:

  <out>.hex  Intel hex containing only the bytes supplied by the inputs,
             plus all of $F800-$FFFF, so that loading it, e.g. with the
             ELF-DOS EEPROM utility, leaves the rest of the ROM alone.

  <out>.bin  A full 32K binary image of $8000-$FFFF with unused bytes
             filled with $FF, for use with an external programmer.

The checksum covers only the resident BIOS at $F800-$FFFF, so whatever
else is in the ROM (such as a ROM-resident kernel) does not affect it.

Usage: mkrom.py [--bios <addr>] -o <out> <in.hex> [<in.hex> ...]

--bios gives the start of the 2K resident BIOS (default f800); a different
address is only for test builds that are loaded into RAM.
"""

import argparse
import sys

ROM_START = 0x8000
ROM_END = 0x10000
BIOS_SIZE = 0x800                       # checksum covers only the BIOS
CHECKSUM = 0x7FC                        # offset of the four checksum bytes
FILL = 0xFF


def read_hex(path):
    """Return a dict of address -> byte from an Intel hex file."""
    data = {}
    with open(path) as f:
        for lineno, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            if not line.startswith(':'):
                sys.exit(f"{path}:{lineno}: not an Intel hex record")
            rec = bytes.fromhex(line[1:])
            if sum(rec) & 0xFF:
                sys.exit(f"{path}:{lineno}: bad record checksum")
            count, addr, rtype = rec[0], (rec[1] << 8) | rec[2], rec[3]
            if rtype == 0x00:
                for i, b in enumerate(rec[4:4 + count]):
                    data[addr + i] = b
            elif rtype == 0x01:
                break
            else:
                sys.exit(f"{path}:{lineno}: unsupported record type {rtype}")
    return data


def rom_checksum(image):
    """Compute the checksum exactly as the BIOS initialization does.

    Returns (sum, fletcher): the 16-bit sum of all bytes, and the running
    sum of the low byte of the partial sums, modulo 256. The BIOS passes
    the checksum if the high byte of the sum equals the byte at $FFFE and
    the fletcher sum is zero; it displays the bytes at $FFFE-$FFFF.
    """
    total = 0
    fletcher = 0
    for b in image:
        total = (total + b) & 0xFFFF
        fletcher = (fletcher + total) & 0xFF
    return total, fletcher


def set_checksum(image):
    """Choose the four checksum bytes so that the BIOS test passes and the
    displayed value at $FFFE-$FFFF is the 16-bit sum of the BIOS. The image
    passed is the BIOS region."""
    off = CHECKSUM
    image[off:off + 4] = b'\0\0\0\0'
    base, fpre = rom_checksum(image[:off])

    # With the four bytes a, b, x, y and s the sum before them, the
    # fletcher sum is fpre + 4s + 4a + 3b + 2x + y (mod 256). Search for
    # a, b such that x:y equals the resulting total sum and fletcher is 0.
    inv3 = pow(3, -1, 256)
    for a in range(256):
        for total in range(base + a, base + a + 3 * 255 + 1):
            x, y = (total >> 8) & 0xFF, total & 0xFF
            b = (-(fpre + 4 * base + 4 * a + 2 * x + y) * inv3) & 0xFF
            if base + a + b + x + y == total:
                image[off:off + 4] = bytes((a, b, x, y))
                total, fletcher = rom_checksum(image)
                assert fletcher == 0 and total >> 8 == x and total & 0xFF == y
                return total
    sys.exit("unable to find checksum bytes")


def write_hex(path, data):
    with open(path, 'w') as f:
        addrs = sorted(data)
        i = 0
        while i < len(addrs):
            start = addrs[i]
            chunk = [data[start]]
            i += 1
            while (i < len(addrs) and addrs[i] == start + len(chunk)
                   and len(chunk) < 16 and (start + len(chunk)) & 0xF):
                chunk.append(data[addrs[i]])
                i += 1
            rec = bytes((len(chunk), start >> 8, start & 0xFF, 0)) + bytes(chunk)
            f.write(':' + (rec + bytes(((-sum(rec)) & 0xFF,))).hex() + '\n')
        f.write(':00000001ff\n')


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('-o', '--output', required=True,
                    help="output base name (.hex and .bin are appended)")
    ap.add_argument('--bios', type=lambda x: int(x, 16), default=0xF800,
                    help="start address of the resident BIOS, in hex")
    ap.add_argument('inputs', nargs='+', help="Intel hex input files")
    args = ap.parse_args()

    bios_start = args.bios
    bios_end = bios_start + BIOS_SIZE
    if bios_start & 0xFF or not ROM_START <= bios_start < bios_end <= ROM_END:
        sys.exit(f"invalid BIOS address {bios_start:04x}")

    merged = {}
    owner = {}
    for path in args.inputs:
        for addr, b in read_hex(path).items():
            if not ROM_START <= addr < ROM_END:
                sys.exit(f"{path}: address {addr:04x} is outside of ROM")
            if addr in merged:
                sys.exit(f"{path}: address {addr:04x} overlaps {owner[addr]}")
            merged[addr] = b
            owner[addr] = path

    image = bytearray([FILL]) * (ROM_END - ROM_START)
    for addr, b in merged.items():
        image[addr - ROM_START] = b

    # The checksum depends on every byte of the BIOS region, so include the
    # unused bytes there in the output too, so that they are always written.

    for addr in range(bios_start, bios_end):
        merged.setdefault(addr, FILL)

    bios = image[bios_start - ROM_START:bios_end - ROM_START]
    total = set_checksum(bios)
    image[bios_start - ROM_START:bios_end - ROM_START] = bios
    for addr in range(bios_start + CHECKSUM, bios_end):
        merged[addr] = image[addr - ROM_START]

    write_hex(args.output + '.hex', merged)
    with open(args.output + '.bin', 'wb') as f:
        f.write(image)

    print(f"{args.output}.hex: {len(merged)} bytes, checksum {total:04x}")
    print(f"{args.output}.bin: {len(image)} bytes, $8000-$FFFF")


if __name__ == '__main__':
    main()
