# plusBASIC Unit Tests

Run the .baq file to run the associated tests.

## Unit Tests

| Program | Tests              |
| :------ | :----------------  |
| cp.baq  | COPY and COMPARE   |
| pp.baq  | POKE and PEEK      |
| sb.baq  | String Slicing     |
| sj.baq  | SPLIT and JOIN     |
| sl.baq  | SAVE and LOAD      |
| ss.baq  | SAVE/LOAD SCREEN   |
| sx.baq  | SAVE/LOAD extended |
| tf.baq  | FILL TILE          |
| tr.baq  | TRIM functions     |
| ts.baq  | Tiles and Sprites  |

## Test Details

- cp.baq 
  - COPY _fromaddr_,_count_ TO _toaddr_
  - COMPARE(_fromaddr_,_toaddr_,_count_)
  - COPY _fromaddr_,_count_ TO @_topage_,_toaddr_
  - COMPARE(_fromaddr_,@_topage_,_toaddr_,_count_)
  - COPY @_frompage_,_fromaddr_,_count_ TO _toaddr_
  - COMPARE(@_frompage_,_fromaddr_,_toaddr_,_count_)
  - COPY @_frompage_,_fromaddr_,_count_ TO @_topage_,_toaddr_
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
- slx.baq
  - SAVE/LOAD BITMAP
  - SAVE/LOAD CHRSET
  - SAVE/LOAD FNKEYS
  - SAVE/LOAD PALETTE
- tf.baq
  - DEF RGBLIST _var_$ = _r_,_g_,_b_; ...
  - SET PALETTE _palette_ TO _rgb_list_
  - SET TILE _tile_index_ TO _tile_data_
  - FILL TILEMAP TILE _tile_index_ ATTR _attr_list_ PALETTE _palette_
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
