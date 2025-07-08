# plusBASIC Unit Tests

Run the .baq file to run the associated tests.

## Unit Tests

| Program | Tests              |
| :------ | :----------------  |
| bt.baq  | BIT                |
| cp.baq  | COPY and COMPARE   |
| cs.baq  | COPY SCREEN        |
| cv.baq  | Convert functions  |
| dr.baq  | DIM and READ       |
| gr.baq  | Misc. Graphics     |
| pp.baq  | POKE and PEEK      |
| ps.baq  | POKE/POKE SCREEN   |
| sb.baq  | String Slicing     |
| sg.baq  | Screen Graphics    |
| sj.baq  | SPLIT and JOIN     |
| sl.baq  | SAVE and LOAD      |
| sp.baq  | SAVE/LOAD PALETTE  |
| ss.baq  | SAVE/LOAD SCREEN   |
| sx.baq  | SAVE/LOAD extended |
| tf.baq  | FILL TILE          |
| tr.baq  | TRIM functions     |
| ts.baq  | Tiles and Sprites  |
| tx.baq  | Tile/Sprite Errors |
| vl.baq  | Variables/Literals |

## Test Details

- bt.baq 
  - BIT(_long_,_bitno_)
  - BIT(_string_,_bitno_)
- cp.baq 
  - COPY _fromaddr_,_count_ TO _toaddr_
  - COMPARE(_fromaddr_,_toaddr_,_count_)
  - COPY _fromaddr_,_count_ TO @_topage_,_toaddr_
  - COMPARE(_fromaddr_,@_topage_,_toaddr_,_count_)
  - COPY @_frompage_,_fromaddr_,_count_ TO _toaddr_
  - COMPARE(@_frompage_,_fromaddr_,_toaddr_,_count_)
  - COPY @_frompage_,_fromaddr_,_count_ TO @_topage_,_toaddr_
- cs.baq 
  - COPY SCREEN TO @_page_,_offset_
  - COPY @_page_,_offset_ TO SCREEN
  - COPY SCREEN CHR TO @_page_,_offset_
  - COPY @_page_,_offset_ TO SCREEN CHR
  - COPY SCREEN ATTR TO @_page_,_offset_
  - COPY @_page_,_offset_ TO SCREEN ATTR
- cv.baq
  - ASC(_string_)
  - ASC(_string_,byte)
  - ASC$(_string_)
  - BYTE(_string_)
  - BYTE(_string_,byte)
  - CHR$(_byte)
  - DEC(_string_)
  - HEX$(_long_)
  - HEX$(_string_)
- dr.baq
  - DIM _var_, _var_$
  - DIM _array_()_, _array_$()
  - DIM _array_() = _list_
  - DIM _array_$() = _list_
  - READ _var_, _var_$
  - READ \*_array_
  - READ \*_array_$
- gr.baq 
  - RGB(_red_,_green_,_blue_)
  - RGB$(_red_,_green_,_blue_)
  - RGB("RGB")
  - RGB$("RGB")
- pp.baq
  - POKE _addr_,_byte_
  - POKE _addr,_string_
  - POKE @_page_,_addr_,_byte_
  - POKE @_page_,_addr,_string_
  - PEEK(_addr_)
  - PEEK$(_addr_,_len_)
  - PEEK(@_page_,_addr_)
  - PEEK$(@_page_,_addr_,_len_)
  - DOKE _addr_,_int_
  - DOKE @_page_,_addr_,_int_
  - DEEK(_addr_)
  - DEEK(@_page_,_addr_)
  - **_ToDo:_**
    - POKE !_ext_addr_,_byte_
    - POKE @_page_,_addr,_string_
    - PEEK(!_ext_addr_)
    - PEEK$(!_ext_addr_,_len_)
    - DOKE !_ext_addr,_int_
    - DEEK(!_ext_addr_)
- ps.baq
  - POKE SCREEN _offset_,_byte_
  - POKE SCREEN _offset_,_string_
  - POKE COLOR _offset_,_byte_
  - POKE COLOR _offset_,_string__
  - PEEKSCREEN(_addr_)
  - PEEKSCREEN$(_addr_,_len_)
  - PEEKCOLOR(_addr_)
  - PEEKCOLOR$(_addr_,_len_)
- sb.baq
  - _string_\[_index_\]
  - _string_\[_index_ TO _index_\]
  - MID$(_strvar_,_pos_,_len_)=_string_
- sg.baq
   - FILL SCREEN CHR _char_
   - RESET BORDER
   - SET BORDER CHR _char_
   - SET BORDER COLOR _fg_, _bg
   - SET BORDER CHR _char_ COLOR _fg_, _bg
   - SET CURSOR ON
   - SET CURSOR OFF
   - CLEAR CURSOR
 - sl.baq
  - SAVE/LOAD binary
  - SAVE/LOAD paged
  - SAVE/LOAD page
  - SAVE/LOAD numeric array
  - SAVE/LOAD binary string array
  - LOAD text string array
  - SAVE/LOAD string variable
  - LOAD DIR binary
  - LOAD DIR text
- sp.baq
  - LOAD PALETTE
  - LOAD PALETTE ... ,ASC
  - LOAD PALETTE ... ,HEX
  - LOAD PALETTE ... ,RGB
  - SAVE PALETTE ...
  - SAVE PALETTE ... ,ASC
  - SAVE PALETTE ... ,HEX
  - SAVE PALETTE ... ,RGB
- ss.baq
  - LOAD SCREEN
  - LOAD SCREEN ATTR
  - LOAD SCREEN CHR
- sx.baq
  - SAVE/LOAD BITMAP
  - SAVE/LOAD CHRSET
  - SAVE/LOAD FNKEYS
  - **_ToDo:_**
    - LOAD TILESET
- tf.baq
  - DEF RGBLIST _var_$ = _r_,_g_,_b_; ...
  - SET PALETTE _palette_ TO _rgb_list_
  - SET TILE _tile_index_ TO _tile_data_
  - FILL TILEMAP TILE _tile_index_ ATTR _attr_list_ PALETTE _palette_
- tr.baq
  - TRIM$(_string_)
  - TRIM$(_string_,_string_)
  - TRIML$(_string_)
  - TRIML$(_string_,_string_)
  - TRIMR$(_string_)
  - TRIMR$(_string_,_string_)
  - PAD$(_string_,_byte_)
  - PAD$(_string_,_byte_,_char_)
  - FILEEXT$(_filespec_)
  - FILEDIR$(_filespec_)
  - TRIMDIR$(_filespec_)
  - TRIMEXT$(_filespec_)
- ts.baq
  - DEF ATTRLIST _var_$ = _attr_, ...
  - DEF PALETTELIST  _var_$ = _palette_, ...
  - DEF RGBLIST _var_$ = _r_,_g_,_b_; ...
  - DEF SPRITE _var_$ = _spritle_,_x_ofs_,_y_ofs_; ...
  - DEF TILELIST _var_$ = _tile_index_, ...
  - FILL TILEMAP TILE _tile_index_ PALETTE _palette_
  - FILL TILEMAP (_left_,_top_) - (_right_,_bottom_) TILE )_tile_index_ PALETTE _palette_ 
  - GET TILEMAP (_left_,_top_)-(_right_,_bottom_),*_var_
  - GET TILEMAP (_left_,_top_)-(_right_,_bottom_),^_var_$
  - GETTILE$(_tile_index_)
  - PUT TILEMAP (_x_,_y_),*_var_
    - **ToDo: Add asserts**
  - PUT TILEMAP (_x_,_y_),^_var_$
    - **ToDo: Add asserts**
  - SET PALETTE _palette_ TO _rgb_list_
  - SET TILE _tile_index_ TO _tile_data_
  - SET TILEMAP (_x_,_y_) TO TILE _tile_index_ PALETTE _palette_
  - TILEMAPX 
    - **ToDo: Add tests**
  - TILEMAPY
    - **ToDo: Add tests**
  - TILEMAP(_x_,_y_)
    - **ToDo: Add tests**
  - TILEOFFSET
  - TILEOFFSET(_gfxmode_)
- tx.baq
  - SET PALETTE _palette_ TO _rgb_list_
  - SET TILE _tile_index_ TO _tile_data_
- vl.baq
  - Extended variable names (tilde)
  - Character literals (single quote)

## Asset Files

| Asset File | Tests                        |
| :------    | :--------------------------- |
| gray.pal   | plusBASIC binary palette     |
| gray.asc   | ASCII palette: RRGGBB        |
| gray.hex   | plusBASIC hex string palette |
| gray.jasc  | Paintshop Pro palette: R G B |
| cga.asc    | ASCII palette: #RRGGBB       |
| c64.asc    | ASCII palette: $RRGGBB       |
| cga.hex    | plusBASIC hex string palette |
| c64.hex    | plusBASIC hex string palette |
| win.gpl    | Gimp palette: R\tG\tB        |
| win.asc    | Paint.net palette: FFRRGGBB  |

