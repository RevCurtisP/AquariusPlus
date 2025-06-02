// Palette Editor Functions

function save_click() {
  name = prompt("Palette name", "")
  if (name == "null") return
  p = get_palette_entries()
  s = palette_to_text(p)
  download_file(name, s)
}

function download_file(fname,fdata) {
  const xf = new File([fdata], fname, {type: 'text/plain',})
  a = document.getElementById("expfile")
  u = URL.createObjectURL(xf)
  a.href = u
  a.download = xf.name
  a.click()
}

function fix_palette_color(hex) {
  red = hex.substr(1,1)
  green = hex.substr(3,1)
  blue = hex.substr(5,1)
  return "#" + red + red + green + green + blue + blue
}

function fix_palette_entry(pe) {
  pe.value = fix_palette_color(pe.value)
}

function export_click() {
  p = get_palette_entries()
  s = palette_to_text(p)
  alert(s)
}

function import_click() {
  s = prompt("Text")
  import_palette(s)
}

//Arg: "rrggbb"
//Returns: [r,g,b]
function hex_to_rgb(hex_color) {
    red = Number("0x"+hex_color.slice(1,3))
    green = Number("0x"+hex_color.slice(3,5))
    blue = Number("0x"+hex_color.slice(5,7))
    return [red,green,blue]
}

function get_palette_entries() {
  p = []
  for (i=0;i<16;i++) {
    p[i] = document.getElementById("pe"+i).value
  }
  return p
}

//Arg: Array of palette entries #rrggbb
//Returns: Formatted palette entries string
function palette_to_text(p) {
  format = document.getElementById("format").value
  if (format == "gpl") return palette_to_gimp(p)
  if (format == "hex") return p.join("\r\n")
  if (format == "pbhex") return palette_to_pbhex(p)
  if (format == "pnet") return palette_to_pnet(p)
  if (format == "pspro") return palette_to_pspro(p)
  return null
}

function palette_to_gimp(palette) {
  rgb_text = "GIMP Palette\r\n#Colors: " + palette.length + "\r\n"
  for (hex_color of palette) 
    rgb_text += hex_to_rgb(hex_color).join("\t") + "\t" + hex_color.slice(1) + "\r\n"
  return rgb_text
}

function palette_to_pbhex(palette) {
  pbhex = ""
  for (line of palette) {
    pbhex += line.slice(3,4) + line.slice(5,6) + "0" + line.slice(1,2)
  }
  return pbhex
}

function palette_to_pnet(palette) {
  rgb_text = ";paint.net Palette File\r\n;" + palette.length + "\r\n"
  for (hex_color of palette) 
    rgb_text += "FF" + hex_color.slice(1) + "\r\n"
  return rgb_text
}

function palette_to_pspro(palette) {
  rgb_text = "JASC-PAL\r\n0100\r\n" + palette.length + "\r\n"
  for (hex_color of palette) 
    rgb_text += hex_to_rgb(hex_color).join(" ") + "\r\n"
  return rgb_text
}

function load_click() {
  document.getElementById("impfile").click()
}

function import_palette(text) {
  palette = parse_palette(text)
  if (palette == null) {alert("Unkown file type"); return}
  set_palette(palette)
}

function load_palette() {
  const pf = document.getElementById("impfile").files[0]
  fr = new FileReader()
  fr.onload = function() {import_palette(fr.result)}
  fr.onerror = function() {alert(fr.error)}
  fr.readAsText(pf)
}

function init_palette() {
  const defpal = new Array("#111111", "#ff1111", "#11ff11", "#ffff11", "#2222ee", "#ff11ff", "#33cccc", "#ffffff",
                           "#cccccc", "#33bbbb", "#cc22cc", "#441199", "#ffff77", "#22dd44", "#bb2222", "#333333")
  set_palette(defpal)
}

// Convert ASCII palette to palette array
// Args: palette string
// Returns: palette array
function parse_palette(s) {
  //console.log(s)
  if (s.length==64) return pbhex_to_palette(s)
  a = []
  for (l of s.split(/[\r\n]/))
    if (l.length) a.push(l)
  if (a.length==16) return hex_to_palette(a)
  if (a[0]=="GIMP Palette") return gpl_to_palette(a)
  if (a[0]=="JASC-PAL") return pspro_to_palette(a)
  if (a[0]==";paint.net Palette File") return pnet_to_palette(a)
  return null
}

function pspro_to_palette(t) {
  p = []
  h = true
  for (s of t) {
    if (h) {
      if  (s=="16") h = false
      continue
    }
    rgb = s.split(" ")
    if (rgb.length < 3) continue
    e = "#" + to_hex(rgb[0]) + to_hex(rgb[1]) + to_hex(rgb[2])
    p.push(e)
  }
  if (p.length != 16) p = null
  return p
}

function gpl_to_palette(t) {
  p = []
  for (i=1; i<t.length; i++) {
    l = t[i]
    if (l.length==0) continue
    if (l.substr(0,1)=="#") continue
    r = l.split("\t")
    if (r[3].length==6) p.push("#" + r[3])
  }
  if (p.length != 16) p = null
  return p
}

function hex_to_palette(t) {
  p = []
  for (i=0; i<t.length; i++) {
    l = t[i]
    if (l.slice(0,1)=="#") l = l.slice(1)
    if (l.slice(0,1)=="$") l = l.slice(1)
    if (l.length==6) p.push("#" + l)
  }
  if (p.length != 16) p = null
  return p
}

function pbhex_to_palette(text) {
  palette = []
  for (i=0; i<text.length; i+=4) {
    red = text.slice(i+3,i+4)
    blue = text.slice(i,i+1)
    green = text.slice(i+1,i+2)
    palette.push("#" + red + red + blue + blue + green + green)
  }
  return palette
}

function pnet_to_palette(text) {
  palette = []
  for (line of text) {
    if (line.slice(0,1)==";") continue
    if (line.length==8) palette.push("#" + line.slice(2))
  }
  return palette
}

function to_hex(n) {
  x = parseInt(n).toString(16)
  if (x.length < 2) x = "0" + x
  return x
}

function hex_nybble(h) {
  n = n.slice(2,4) / 16
  return n.fromCharCode()
}

function gimp_to_palette(a) {
  p = new Array()
  for (s of a) {
    m = s.match(/^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]/i)
    if (m) p.push(m[0])
  }
  if (p.length != 16) p = null
  return p
}

function set_palette(p) {
  for (i=0; i<16; i++) {
    e = document.getElementById("pe"+i)
    e.value = p[i]
  }
}
