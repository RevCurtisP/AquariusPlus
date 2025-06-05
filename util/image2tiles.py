# Image to Aquarius Tilemap Converter

import argparse
from os import path
from PIL import Image
import sys

def exit_err(err_no, err_text):
    print(err_text, file=sys.stderr)
    exit(err_no)
    
def warning(warn_text):
    print("Warning: " +warn_text, file=sys.stderr)
    
def hex_nybble(num):
    return hex(num)[2:]

def build_tile(pixels, tcol, trow):
    return None

argdesc = "Image to tileset, tilemap, and tileclip converter"
argnotes = """The verbose option outputs image size, tilemap size, number of entires in the tileset, and palette,
The debug option outputs textual representations of the tilemap and tileset.
Specifying both verbose and debug adds a textual represntation of the pixels in each tile.
  This greatly increases the run time of the utility.
"""

parser = argparse.ArgumentParser(prog="image2tiles", description=argdesc, epilog=argnotes, formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument("filename",help="Image filename")
parser.add_argument('-d','--debug', action="store_true", help="Write debug info to stdout")
parser.add_argument('-v','--verbose', action="store_true", help="Write image info to stdout")
args = parser.parse_args()

debug = args.debug
verbose = args.verbose

img_name = args.filename
base_name, extension = path.splitext(img_name)

start_tile = 128

palt = []   # Paleette entries
tset = []   # Tileset entries

img = Image.open(img_name)

(img_width, img_height) = img.size

if debug: print("Image:",img)

if verbose: print("Image size: %s x %s" % (img_width, img_height))

if img_width % 8 != 0 or img_height % 8 != 0:
   exit_err(255,"Image width and height must be multiple of 8.")

tmap_width = int(img_width / 8)
tmap_height = int(img_height / 8)

tmap = [ [0] * tmap_width] * tmap_height
dmap = ""
pmap = "" if debug and verbose else None

if verbose: print("Tilemap size: %s x %s" % (tmap_width, tmap_height))
if tmap_width > 64: warning("Tilmap width is more than 64 tiles")
if tmap_height > 32: warning("Tilmap height is more than 64 tiles")

img_pixels = img.load()

for tmap_row in range(0,tmap_height):
    for tmap_col in range(0,tmap_width):
        if pmap != None: pmap += "%d,%d\n" % (tmap_col, tmap_row)
        tile_noflip = ""
        tile_hflip = ""
        tile_vflip = ""
        tile_hvflip = ""
        for ofs_y in range(0,8):
            tline_noflip = ""
            tline_hflip = ""
            for ofs_x in range(0,8):
                img_x = tmap_col * 8 + ofs_x
                img_y = tmap_row * 8 + ofs_y
                pxl = img_pixels[img_x,img_y]
                if len(pxl)==3: (rr,gg,bb) = pxl
                else: (rr,gg,bb,aa) = pxl
                rgb = hex_nybble(gg >> 4) + hex_nybble(bb >> 4) + "0" + hex_nybble(rr >> 4)
                if pmap != None: pmap += "%3d,%-3d %s " % (img_x, img_y, rgb[3:]+rgb[:2])
                if not rgb in palt:
                    palt.append(rgb)
                    if len(palt) > 16: exit_err(255,"Too many palette colors generated")
                pxl = hex_nybble(palt.index(rgb))
                tline_noflip += pxl
                tline_hflip = pxl + tline_hflip
            tile_noflip += tline_noflip
            tile_hflip += tline_hflip
            tile_vflip = tline_noflip + tile_vflip
            tile_hvflip = tline_hflip + tile_hvflip
            if pmap != None: pmap += "\n"
        if pmap != None: pmap += "  %s\n  %s\n  %s\n  %s\n" % (tile_noflip, tile_hflip, tile_vflip, tile_hvflip)
        if tile_noflip in tset:
            tset_ofs = tset.index(tile_noflip)
            tile_props = 0x0000
            flip = "  "
        elif tile_hflip in tset:
            tset_ofs = tset.index(tile_hflip)
            tile_props = 0x0200
            flip = "h "
        elif tile_vflip in tset:
            tset_ofs = tset.index(tile_vflip)
            tile_props = 0x0400
            flip = "v "
        elif tile_hvflip in tset:
            tset_ofs = tset.index(tile_hvflip)
            tile_props = 0x0600
            flip = "hv"
        else:
            tset.append(tile_noflip)
            tset_ofs = len(tset) - 1
            tile_props = 0x0000
            flip = "  "
        tile_no = start_tile + tset_ofs
        tcell = tile_no | tile_props
        dmap += str(tile_no) + flip + " "
        tmap[tmap_row][tmap_col] = tcell
    dmap += "\n"

if pmap != None: print("\n"+pmap)
if debug: print("\nTilemap: \n"+dmap)

with open(base_name + ".tset","wb") as tset_file:
    thex = "TileSet:\n"
    for i in range(0,len(tset)):
        tdef = tset[i]
        thex += "%3d %s\n" % (start_tile+i, tdef)
        tbytes = bytes.fromhex(tdef)
        tset_file.write(tbytes)
    if debug: print("\n"+thex)

with open(base_name + ".vmap","wb") as vmap_file:
    for trow in tmap:
        for tcell in trow:
            vmap_file.write(tcell.to_bytes(2,"little"))
    
ptxt = ""
phex = ""

for rgb in palt:
    phex += rgb
    ptxt += "  %s\n" % (rgb)

if verbose:
    print("\n%d unique tiledefs" % (len(tset)))
    print("\nPalette (%d entries)" % (len(palt)))
    print(ptxt)
    
phex = phex.ljust(64,"0")
pbytes = bytes.fromhex(phex)
with open(base_name + ".palt","wb") as palt_file:
    palt_file.write(pbytes)

#tmap = open(base_name + ".tmap","wb")
#with open(vmap_file,"rb" as vmap_file:
    