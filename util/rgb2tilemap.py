#!/usr/bin/env python3
#-----------------------------------------------------------------------------
# Copyright (C) 2023 Curtis F Kaylor
#
# Converts raw 8-bit RGB file to Aquarius+ Tiles and Tile Map
#
# First, convert PNG to RGB ImageMagick command line to create the rgb file
#   'png2rgb image' (without the .png extension)


import argparse
import struct 
import sys

parser = argparse.ArgumentParser(
    description="Convert Aquarius BASIC .BAS file to text file"
)
parser.add_argument("input", help="RGB Input file (without .rgb extension")

debug = False

args = parser.parse_args()

rgb_name = args.input + ".rgb"
pal_name = args.input + ".pal"
til_name = args.input + ".til"
map_name = args.input + ".map"
txt_name = args.input + ".txt"

rgb_pxllen = 3
rgb_linlen = rgb_pxllen * 8
rgb_tillen = rgb_linlen * 8

tm_fliph = 0x0200
tm_flipv = 0x0400
tm_pal = 0x1000

pal_index = 0
pal_dict = {} 

tile_index = 128
tile_dict = {}

tile_map = []
tile_count = {}
tile_cell = {}

cell_no = 0

with open(rgb_name, "rb") as rgb_file:
  while (rgb_tile := rgb_file.read(rgb_tillen)):
    tile = b''
    tilev = b''
    tileh = b''
    tilevh = b''
    for line_offset in range(0, rgb_tillen, rgb_linlen):
      rgb_line = rgb_tile[line_offset:line_offset+rgb_linlen]
      rgb_hex = ""
      til_hex = ""
      tfh_hex = ""
      if debug: print("rgb_line:", rgb_line.hex())
      for offset in range(0, rgb_linlen, rgb_pxllen):
        (rr, rb, rg) = struct.unpack('BBB', rgb_line[offset:offset+rgb_pxllen])
        pal_entry = ((rr & 0xE0) << 4) | (rb & 0xE0) | ((rg & 0xE0) >> 4)
        rgb_hex = rgb_hex + " " + hex(pal_entry)[2:]
        if not pal_entry in pal_dict:
          pal_dict[pal_entry] = pal_index
          if debug: print ("pal_index:", pal_index, "; pal_entry:", hex(pal_entry))
          pal_index += 1
        pxl_index = pal_dict[pal_entry]
        pxl_hex = hex(pxl_index)[2:]
        #if debug: print("pal_entry:",hex(pal_entry),"; pxl_index:",pxl_index, "; pxl_hex:", pxl_hex)  
        til_hex = til_hex + pxl_hex
        tfh_hex = pxl_hex + tfh_hex
      if debug: print("rgb_hex:", rgb_hex)
      if debug: print("til_hex:", til_hex)
      til_line = bytes.fromhex(til_hex)
      #if debug: print("til_line:", til_line)
      tfh_line = bytes.fromhex(tfh_hex)
      tile = tile + til_line
      tilev = til_line + tilev
      tileh = tileh + tfh_line
      tilevh = tfh_line + tilevh 
    #if debug: print("tile:",tile)
    if tile in tile_dict:
      tm_cell = tile_dict[tile] 
    elif tileh in tile_dict:
      tm_cell = tile_dict[tileh] | tm_fliph
    elif tilev in tile_dict:
      tm_cell = tile_dict[tilev] | tm_flipv
    elif tilevh in tile_dict:
      tm_cell = tile_dict[tilevh] | tm_fliph | tm_flipv
    else:
      tile_dict[tile] = tile_index
      tm_cell = tile_index
      tile_count[tile_index] = 0
      tile_cell[tile_index] = cell_no
      tile_index += 1
    tile_count[tm_cell & 0x1FF] += 1
    if debug: print("cell#:",cell_no)
    if debug: print("tm_cell:", hex(tm_cell | tm_pal))
    tile_map.append((tm_cell | tm_pal))  
    cell_no += 1

# Generate Aquarius+ tilemap file
with open(map_name, "wb") as map_file:
  for cell in tile_map:
    map_file.write(cell.to_bytes(2,"little"))

# Generate Aquarius+ palette file
with open(pal_name, "wb") as pal_file:
  for key in pal_dict:
    pal_file.write(key.to_bytes(2,"little"))
 
# Generate Aquarius+ tile definition file
with open(til_name, "wb") as til_file:
  for key in tile_dict:
    til_file.write(key)

# Open descriptive text file
with open(txt_name, "w") as txt_file:
  # Write text tilemap
  text_div = "+---" * 64 + "+"
  for cell_no in range(0, len(tile_map)):
    cell = tile_map[cell_no]
    if (cell_no & 63) == 0: 
      if cell_no:
        txt_file.write(text_div + "\n")
        txt_file.write(num_line + "\n")
        txt_file.write(flp_line + "\n")
      num_line = "|"
      flp_line = "|"
    num_line += "{:>3}|".format(cell & 511)
    flp_horz = "H" if cell & tm_fliph else " "
    flp_vert = "V" if cell & tm_flipv else " "
    flp_line += flp_horz + " " + flp_vert + "|"
  txt_file.write(text_div + "\n")
  txt_file.write(num_line + "\n")
  txt_file.write(flp_line + "\n")
  txt_file.write(text_div + "\n\n")
  # Write tile usage counts
  txt_file.write("Unique tiles: %d\n\n" % len(tile_dict))
  txt_file.write("Tile# Count Position\n")
  for index, count in tile_count.items():
    txt_file.write(" " + str(index) + "{:>7}".format(count))
    if count == 1: 
      cell = tile_cell[index]
      txt_file.write(" (%d,%d)" % (cell & 63, cell / 64))
    txt_file.write("\n")