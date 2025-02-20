; AuxROM system initialization routines

;-----------------------------------------------------------------------------
; Character ROM buffers initialization
;-----------------------------------------------------------------------------
init_chrsets:
; Copy standard character set into buffer
    ld      hl,.defdesc
    call    file_load_defchrs
    ld      hl,.altdesc
    jp      file_load_altchrs
.defset:
    byte    "esp:default.chr"
.defdesc:
    word    $-.defset,.defset
.altset:
    byte    "esp:latin1b.chr"
.altdesc:
    word    $-.altset,.altset
