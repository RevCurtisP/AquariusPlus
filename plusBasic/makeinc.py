# Make include file from aqplusbas.lst

lstfile = open("zout/aqplusbas.lst","r")
incfile = open("../dev/aqplus.inc","w")
asmfile = open("../dev/aqplus_asm.inc","w")

inc_lines = [";Aquarius+ Kernel Routines\n"]
asm_lines = [";Aquarius+ Kernel Routines\n"]

in_jump_tbl = False

jp_lines = 0

for lst_line in lstfile.readlines():
    
    inc_line = ""
    asm_line = ""
    
    if "_end_jump_table:" in lst_line: 
        in_jump_tbl = False
        continue

    if "_jump_table:" in lst_line: 
        in_jump_tbl = True
        continue
    
    if not in_jump_tbl:
       continue

    if lst_line.find("; .") == 2:
       inc_lines.append("\n")
       asm_lines.append("\n")
       continue

    if lst_line.find("; *") == 2:
       inc_lines.append(lst_line[2:])
       asm_lines.append(lst_line[2:])
       continue

    if lst_line.find(";;") == 2:
        continue

    if "just_ret" in lst_line:
       continue

    if "<<" in lst_line and ">>" in lst_line:
        s = lst_line.index("<<")+2
        e = lst_line.index(">>")
        inc_line = "\n; " + lst_line[s:e]
        
    if "  	    jp      " in lst_line:
        address = lst_line[0:4]
        label = lst_line[27:].rstrip()
        semi = label.find(";")
        if semi > 0:
            comment = label[semi:]
            label = label[:semi].rstrip()
        else:
            comment = ""
        label = label.replace("__","_")
        asmlbl = "asm_" + label
        inc_line = ("%-22s equ $%s %s" % (label, address, comment))
        asm_line = ("%-26s equ $%s %s" % (asmlbl, address, comment))
        jp_lines += 1
        
    if inc_line:
        inc_lines.append(""+inc_line + "\n")
        asm_lines.append(""+asm_line + "\n")
        
incfile.writelines(inc_lines)
asmfile.writelines(asm_lines)
   
print("%d total jumps" % (jp_lines))
