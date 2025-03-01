

var palfile = "palette.txt"
var cbutton = null

function InitPalette() {
  BuildPalette()
  const p = new Array("#111111", "#ff1111", "#11ff11", "#ffff11", "#2222ee", "#ff11ff", "#33cccc", "#ffffff",
                      "#cccccc", "#33bbbb", "#cc22cc", "#441199", "#ffff77", "#22dd44", "#bb2222", "#333333")
  SetPalette(p)
  b = document.getElementById("p0")
  SelectColor(b)
}

function BuildPalette() {
  const pd = document.getElementById("palette")
  d = PaletteDiv(pd,0,7)
  b = PaletteButton(d,"","","Import")
  b.onclick = function() {ImportPalette()}
  d = PaletteDiv(pd,8,15)
  b = PaletteButton(d,"","","Export")
  b.onclick = function() {ExportPalette()}
  d = document.createElement("div");
  d.hidden = true
  pd.appendChild(d)
  a = document.createElement("a");
  a.id = "expal"
  d.appendChild(a)
  f = document.createElement("input");
  f.type = "file"
  f.id = "impal"
  f.onchange = function() {LoadPalette()}
  d.appendChild(f)
  c = document.createElement("input");
  c.type = "color"
  c.id = "pentry"
  c.onchange = function() {UpdateEntry(this)}
  d.appendChild(c)
}

function PaletteDiv(pd, s,e) {
  d = document.createElement("div");
  pd.appendChild(d)
  for (p=s;p<=e;p++) {
    b = PaletteButton(d,"p"+p,"color","","SelectColor","PickColor","" )
    b.onclick = function() {SelectColor(this)}
    b.ondblclick = function() {PickColor(this)}
  }
  return d
}

function PaletteButton(d,id,cl,h) {
  b = document.createElement("button");
  b.id = id
  b.className = cl
  b.innerHTML = h
  d.appendChild(b)
  return b
}

function PickColor(b) {
  cbutton = b
  p = document.getElementById("pentry")
  p.click()
}

function SelectColor(b) {
  if (cbutton) cbutton.style.boxShadow = ""
  b.style.boxShadow = "gray 3px 2px"
  cbutton = b
}

function UpdateEntry(p) {
  cbutton.style.backgroundColor = p.value
  cbutton.hexcolor = p.value
}

function ExportPalette() {
  pf = prompt("Export to file",palfile)
  if (pf == null) return
  palfile = pf
  xp = ""
  for (p=0;p<16;p++) {
    xp += document.getElementById("p"+p).hexcolor + "\r\n"
  }
  const xf = new File([xp], palfile, {type: 'text/plain',})
  a = document.getElementById("expal")
  u = URL.createObjectURL(xf)
  a.href = u
  a.download = xf.name
  a.click()
}

function ImportPalette() {
  document.getElementById("impal").click()
}

function LoadPalette() {
  const pf = document.getElementById("impal").files[0]
  fr = new FileReader()
  fr.onload = function() {WritePalette(fr.result)}
  fr.onerror = function() {alert(fr.error)}
  fr.onerror = function() {alert(fr.error)}
  fr.readAsText(pf)
}

function WritePalette(paltext) {
  pa = paltext.split(/[\r\n]/)
  pl = new Array()
  for (pe of pa) {
    pt = pe.match(/^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]/i)
    if (pt) pl.push(pt[0])
  }
  if (pl.length != 16) {
    alert("Invalid palette file")
    return
  }
  SetPalette(pl)
}

function SetPalette(a) {
  for (p=0; p<16; p++) {
    b = document.getElementById("p"+p)
    b.style.backgroundColor = a[p]
    b.hexcolor = a[p]
  }
}
