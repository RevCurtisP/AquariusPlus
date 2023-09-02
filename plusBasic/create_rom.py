#!/usr/bin/env python3
import argparse

#parser = argparse.ArgumentParser(description="Generate Aquarius+ system ROM")
#parser.add_argument("basic_rom", help="Base ROM")
#parser.add_argument("-patch", help="Patch base ROM", action="store_true")
#args = parser.parse_args()

# aquarius.rom Memory Map
# aquarius.rom Aquarius+     
# 0000 - 1FFF  0000 - 1FFF    S3 BASIC  
# 2000 - 2FFF  2000 - 2FFF    plusBASIC
# 3000 - 3FFF  3000 - 3FFF    Char ROM (Overlaid)
# 4000 - 7FFF  C000 - FFFF    plusBASIC 16k
# 8000 - A000  8000 - A000    plusBASIC 8k (swapped in)

with open("zout/aquarius.rom", "wb") as f:
    # Aquarius S3 BASIC ROM with Aquarius+ options
    with open("..\s3basic\s3basic-aqplus.cim", "rb") as fbasic:
        s3rom = bytearray(fbasic.read())
        f.write(s3rom)

    # Extended ROM $2000 - $2FFF
    with open("zout/aqplusbas.cim", "rb") as fplus:
        plusrom = bytearray(fplus.read())
        f.write(plusrom[:4096])

    # Aquarius Character ROM
    with open("../charROMs/aqpluschar.rom", "rb") as fchar:
        charrom = bytearray(fchar.read())
        f.write(charrom)

    # Extended ROM $C000 -
        f.write(plusrom[4096:])
