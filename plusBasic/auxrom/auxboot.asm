; AuxROM system initialization routines

aux_init:
    call    chrset_init
    call    file_load_pt3play
    ret
