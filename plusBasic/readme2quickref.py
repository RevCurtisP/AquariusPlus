# Reformat README.md into quickref.md

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

list_level = 0

for line in md_in.readlines():
    line = line.rstrip()
    key = 0
    
    # Strip checkboxes
    i = line.find("- [ ] ")
    if i < 0: i = line.find("- [x] ")
    if i > 0: line = line[:i] + "- " + line[i+6:]
        
    # Strip html comments and get embedded key
    s = line.find("<!--")
    if s > -1:
        e = line.find("-->",s)
        key = line[s+4:e]
        line = line[:s] + line[e+1:]

    # Insert wiki link into line
    wiki = line
    if key and key in links:
        link = links[key]
        b = wiki.find(" **")+3
        e = wiki.find("** ")
        wiki = wiki[:b] + "[" + wiki[b:e] + "](" + link + ")" + wiki[e:]

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