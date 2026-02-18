# Reformat README.md into quickref.md

import re

md_in = open("README.md")
md_out = open("quickref.md","w")
wiki_out = open("quickref_wiki.md","w")
#html_out = open("quickref.html","w")
links_in = open("quickref-links.tab")

html_header = [
'<!DOCTYPE html>\n',
'<html>\n',
'  <head>\n',
'    <meta charset="UTF-8" />\n',
'    <meta name="author" content="Curtis F Kaylor" />\n',
'    <title>plusBASIC Quick Reference Gude</title>\n',
'    <style type="text/css">\n',
'      /* Avoid page breaks inside the most common attributes, especially for exports (i.e. PDF) */\n',
'      td, h1, h2, h3, h4, h5, p, ul, ol, li {page-break-inside: avoid;}\n',
'    </style>\n',
'  </head>\n',
'  <body>\n'
]

html_trailer = [
'  </body>\n',
'</html>\n'
]

# Build wiki links dictionary
links = {}
for line in links_in:
    line = line.strip()
    (key, value) = line.split()
    links[key] = value

out_lines = []

#html_out.writelines(html_header)

kwords = []

list_level = 0

skipc = False

for line in md_in.readlines():
    line = line.rstrip()
    key = 0

    if line.find("<!---") == 0: 
        skipc = True

    if line.find("--->") == 0: 
        skipc = False
        continue
    
    if skipc: continue

    # Strip checkboxes
    i = line.find("- [ ] ")
    if i < 0: i = line.find("- [.] ")   # Stub
    if i < 0: i = line.find("- [-] ")   # Incomplete
    if i < 0: i = line.find("- [x] ")   # Complete
    if i < 0: i = line.find("- [!] ")   # Bug fix required
    if i > 0: line = line[:i] + "- " + line[i+6:]
    
    # Get statement/function name
    kword = ""
    if line[:5] == " - **":
        l = line[5:]
        b = l.find(" .. ")
        e = l.find(" .. ", b+4)
        if b and e: l = l[:b] + "--" + l[e+4:]
        m = re.search(r"^(.+?)[(_*#{&=«\\]",l)
        if m: kword = m.group(1)
        i = kword.find("ON|OFF")
        if i > 0: kword = kword[:i]
        i = kword.find(" OFF")
        if i > -1 and i == len(kword)-4: kword = kword[:i]
        kword = kword.rstrip().replace(" ","-")
        n = len(kword) - 1
        if kword[n] == "$": 
            kword = kword[:n]
            if kword in kwords: kword = kword + "-1"
        else:
            kwords.append(kword)
    
    # Strip html comments and get embedded key
    s = line.find("<!--")
    if s > 0:
        e = line.find("-->",s)
        key = line[s+4:e]
        line = line[:s] + line[s+e+1:]
        i = key.find(':')
        if i > -1:
            kword = key[i+1:]
            key = key[:i]
            is_operator = True
        else:
            is_operator = False

    # Insert wiki link into line
    wiki = line
    if key and key in links:
        link = links[key]
        if kword: link = link + "#" + kword.lower()
        if is_operator:
           b = wiki.find(kword)
           e = b + len(kword)
        else:
            b = wiki.find(" **")+3
            e = wiki.find("** ")
        wiki = wiki[:b] + "[" + wiki[b:e] + "](" + link + ")" + wiki[e:]
        wiki = wiki.replace(" __ ", " ")
        wiki = wiki.replace(" .. ", " ")

    line = line.replace(" __ ", " ")
    line = line.replace(" .. ", " ")

    md_out.write(line + "\n")
    wiki_out.write(wiki + "\n")

#   # Convert to html
#   html = line
#   # headings
#   if len(line) > 3 and line[:2] == "# ":
#       html = "    <h1>%s</h1>" % line[2:].strip()
#   if len(line) > 4 and line[:3] == "## ":
#       html = "    <h2>%s</h2>" % line[3:].strip()
#   # list elements
#   if len(line) > 4 and line[:4] == "  - ":
#       if list_level < 1:
#           html_out.write("    <ul>\n")
#           list_level = 1
#       html = "    <li>%s</li>" % (html)
#       if list_level > 1:
#           html_out.write("      </ul>\n")
#           list_level = 1
#   elif len(line) > 6 and line[:6] == "    - ":
#       if list_level < 2:
#           html_out.write("      <ul>\n")
#           list_level = 2
#       html = "      <li>%s</li>" % (html)
#   else:
#       html_out.write("    </ul>\n")
#       list_level = 0
#  
#   if html != line:
#       html_out.write(html + "\n")

#html_out.writelines(html_trailer)


# Inserting wiki links into .md
# - **syntax** - description<!--link_id>
# **[syntax](BASIC-Expressions-and-Operators#arithmetic-operators)**