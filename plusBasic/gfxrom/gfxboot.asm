; GfxROM system initialization routines

gfx_init:
    call    bitmap_init_vars
    call    spritle_reset_all
    ret