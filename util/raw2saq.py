#!/usr/bin/env python3
#-----------------------------------------------------------------------------
# Convert headerless unsigned 8-bit PCM sound file to Aquarius .SAQ file
#-----------------------------------------------------------------------------

import argparse
import struct
import sys

rates = ['4000', '6000', '8000', '11000', '12000', '14000', '16000', '20000']

parser = argparse.ArgumentParser(
    description="Convert raw digitized sound file to Aq+ sample"
)
parser.add_argument("input", help="Input file", type=argparse.FileType("rb"))
parser.add_argument("output", help="Output file", type=argparse.FileType("wb"))
parser.add_argument("rate", choices=rates, help="Sample rate")

args = parser.parse_args()

infile = args.input
outfile = args.output

data = infile.read()
length = len(data)
if length > 0xFFF0:
    print("File too large")
    exit(1)

len_bytes = length.to_bytes(3,'little')
rate_byte = rates.index(args.rate).to_bytes(1,'little')

outfile.write(len_bytes + rate_byte + data)

infile.close()
outfile.close()
