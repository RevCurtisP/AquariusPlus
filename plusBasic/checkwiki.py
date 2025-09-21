# Search for BASIC keyword titles in wiki

import os

out_filename = "zout/wiki-keyword_list.txt"
anchor_filename = "zout/wiki-anchor_list.txt"
broken_filename = "zout/wiki-broken_links.txt"
wiki_path = 'C:/Users/comit/Dropbox/github/aquarius-plus.wiki'

out_file = open(out_filename, 'w')
anchor_file = open(anchor_filename, 'w')
broken_file = open(broken_filename, 'w')

dir_list = os.listdir(wiki_path)

page_list = {}
keyword_list = {}
link_list = {}
anchor_list = []

for filename in dir_list:
    if filename[0] == '.': continue
    page = filename[0:-3]
    page_list[page] = {}
    with open(wiki_path + '/' + filename, encoding='utf-8', errors='ignore') as in_file:
        lines = in_file.readlines()
        for line in lines:
            if line[0:2] == "# " or line[0:3] == "## ":
                title = line[line.find(" "):].strip()
                anchor = title.replace(' ','-').lower()
                anchor = page + "#" + anchor
                if title.upper() == title:
                    if not title in keyword_list: keyword_list[title] = []
                    if not page in keyword_list[title]: keyword_list[title].append(page)
                    if anchor[-1:] == '$':
                        anchor = anchor[:-1]
                        if anchor in anchor_list: 
                            anchor = anchor + "-1"
                    if not anchor in anchor_list: anchor_list.append(anchor)
                elif anchor.find("BASIC-") == 0:
                    if not anchor in anchor_list: anchor_list.append(anchor)
                continue
            kline = line
            while True:
                i = kline.find("](BASIC-")
                ## ToDo: Check local anchor links "](#KEY WORD)"
                if i < 0: break
                link = kline[i+2:]
                i = link.find(')')
                if i > -1:
                    kline = link[i:]
                    link = link[:i]
                    if not link in link_list: link_list[link] = []
                    if not page in link_list[link]: link_list[link].append(page)
                else:
                    break

for keyword in sorted(keyword_list):
    out_file.write(keyword + '\n')
    for page in keyword_list[keyword]:
        out_file.write('\t' + page + '\n')
        anchor = keyword.replace(' ','-')
        anchor = page + "#" + anchor
        if anchor[-1:] == '$' and anchor[:-1] in anchor_list: 
            anchor = anchor[:-1] + "-1"
        if anchor in link_list:
            for page in link_list[anchor]:
                out_file.write('\t\t' + page + '\n')

for link in link_list:
    if link.find('#') > -1:
        found =  link in anchor_list
    else:
        found = link in page_list
    if not found:
        broken_file.write(link + "\n")
        for page in link_list[link]:
            broken_file.write("  " + page + "\n")

for anchor in sorted(anchor_list):
    anchor_file.write(anchor + '\n')
