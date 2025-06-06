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
    
def hex_byte(num):
    x = hex(num)[2:]
    if len(x) == 1: x = "0" + x
    return x

def hex_int(num):
    return hex_byte(num & 0xFF) + hex_byte(num >> 8)

def hex_nybble(num):
    return hex(num)[2:]

def build_tile(pixels, tcol, vrow):
    return None

argdesc = "Image to tileset, tilemap, and tileclip converter"
argnotes = ""

docs = """'--verbose' outputs image size, tilemap size, number of entires in the tileset, and palette,
'--debug' outputs textual representations of the tilemap and tileset.
Specifying '--verbose' and '--debug' adds a textual represntation of the pixels in each tile.
  This greatly increases the run time of the utility.
If '--starttile' is not specified, the generated tile definitions start at tile 128.
The first tile generated will be blank (color indes 0 in all pixels) unless '--blanktile' is sprecitied.
  If the specified tile ID falls within the the range of geberated tiles, then that tile will be defined as blank.
"""

parser = argparse.ArgumentParser(prog="image2tiles", description=argdesc, epilog=argnotes, formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument("imagename",help="Image filename")
parser.add_argument("-b","--blanktile", default=None, type=int, help="Tile ID of pre-existing blank tile")
parser.add_argument("-c","--clipsize", default=None, help="Tileclip size WIDTHxHEIGHT[r]")
parser.add_argument("-d","--debug", action="store_true", help="Write debug info to stdout")
#parser.add_argument("--docs",action="store_true", help="Display documentation")
parser.add_argument("-p","--palette", default=2, type=int, help="Palette number (0-3)")
parser.add_argument("-s","--starttile", default=128, type=int, help="Starting tile number")
parser.add_argument("-v","--verbose", action="store_true", help="Write image info to stdout")
parser.add_argument("-x","--suffix", default="", help="Tileclip size WIDTHxHEIGHT[r]")
args = parser.parse_args()

blank_tile = args.blanktile
clip_size = args.clipsize
debug = args.debug
palette_num = args.palette
start_tile = args.starttile
verbose = args.verbose
suffix = args.suffix

palt_props = palette_num << 12

#if args.docs:
#    print(docs)
#    exit()

if clip_size:
    if clip_size[-1] == "r":
        size = clip_size[:-1]
        clip_rotate = True
    else:
        size = clip_size
        clip_rotate = False
    size = size.split("x")
    if len(size) == 2:
        clip_width = int(size[0])
        clip_height = int(size[1])
    else:
        exit_err(255,"Invalid clip size %s" % (clip_size))

img_name = args.imagename
if img_name == None:
    exit_err(255, "Image filename not specified")
base_name, extension = path.splitext(img_name)
if suffix: base_name += suffix

palt = []   # Paleette entries
tset = []   # Tileset entries

img = Image.open(img_name)

(img_width, img_height) = img.size

if debug: print("Image:",img)

if verbose: print("Image size: %s x %s" % (img_width, img_height))

if img_width % 8 != 0 or img_height % 8 != 0:
   exit_err(255,"Image width and height must be multiple of 8.")

vmap_width = img_width // 8
vmap_height = img_height // 8

vmap = []
dmap = ""
pmap = "" if debug and verbose else None

if verbose: print("Tilemap size: %s x %s" % (vmap_width, vmap_height))
if vmap_width > 64: warning("Tilmap width is more than 64 tiles")
if vmap_height > 32: warning("Tilmap height is more than 64 tiles")

if clip_size:
    if verbose: 
        print("Tileclip size: %d x %d" % (clip_width, clip_height))
        print("Rotate tileclip = %s" % (clip_rotate))
    if vmap_width % clip_width != 0: exit_err(255, "Tilemap width is not multiple of tileclip width")
    if vmap_height % clip_height != 0: exit_err(255, "Tilemap height is not multiple of tileclip height")

bdef = "0" * 64
if blank_tile == None:
    tset.append(bdef)
    bdef = ""

img_pixels = img.load()

for vmap_row in range(0,vmap_height):
    vrow = []
    for vmap_col in range(0,vmap_width):
        if pmap != None: pmap += "%d,%d\n" % (vmap_col, vmap_row)
        tile_noflip = ""
        tile_hflip = ""
        tile_vflip = ""
        tile_hvflip = ""
        for ofs_y in range(0,8):
            tline_noflip = ""
            tline_hflip = ""
            for ofs_x in range(0,8):
                img_x = vmap_col * 8 + ofs_x
                img_y = vmap_row * 8 + ofs_y
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
        tile_props = 0x0000
        flip = "  "
        if tile_noflip == bdef:
            tset_ofs = blank_tile - start_tile
        elif tile_noflip in tset:
            tset_ofs = tset.index(tile_noflip)
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
            if blank_tile != None:
              if len(tset) == blank_tile - start_tile: tset.append("0" * 64)
            tset.append(tile_noflip)
            tset_ofs = len(tset) - 1
        tile_id = start_tile + tset_ofs
        vcell = tile_id | tile_props | palt_props
        dmap += str(tile_id) + flip + " "
        vrow.append(vcell)
    vmap.append(vrow)
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
    for vrow in vmap:
        for vcell in vrow:
            vmap_file.write(vcell.to_bytes(2,"little"))
    
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

bcell = blank_tile if blank_tile else start_tile
with open(base_name + ".tmap","wb") as tmap_file:
    for row in range(0,32):
        if row < len(vmap): vrow = vmap[row]
        else: vrow = []
        for col in range(0,64):
            if col < len(vrow): tcell = vrow[col]
            else: tcell = bcell | palt_props
            tmap_file.write(tcell.to_bytes(2,"little"))

if clip_size:
    if debug: print("\nTileClips:")
    clips = bytes()
    clip_len = clip_width * clip_height * 2 + 2
    hclips = vmap_width // clip_width
    vclips = vmap_height // clip_height
    if clip_rotate:
        for clipx in range(0,hclips):
            for clipy in range(0,vclips):
                clip = hex_byte(clip_len) + hex_byte(clip_width) + hex_byte(clip_height)
                for ofsy in range(0,clip_height):
                    for ofsx in range(0,clip_width):
                        tmapx = clipx * clip_width + ofsx
                        tmapy = clipy * clip_height + ofsy
                        tcell = vmap[tmapy][tmapx]
                        clip += hex_int(tcell)
                if debug: print(clip)
                clips += bytes.fromhex(clip)
    else:
        for clipy in range(0,vclips):
            for clipx in range(0,hclips):
                clip = hex_byte(clip_len) + hex_byte(clip_width) + hex_byte(clip_height)
                for ofsy in range(0,clip_height):
                    for ofsx in range(0,clip_width):
                        tmapx = clipx * clip_width + ofsx
                        tmapy = clipy * clip_height + ofsy
                        tcell = vmap[tmapy][tmapx]
                        clip += hex_int(tcell)
                if debug: print(clip)
                clips += bytes.fromhex(clip)
    with open(base_name + ".clip", "wb") as clip_file:
        clip_file.write(clips)