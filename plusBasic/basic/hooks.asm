;====================================================================
; plusBASIC Hook Handlers
;====================================================================

;-----------------------------------------------------------------------------
; Hook 2 - READY (Enter Direct Mode
;-----------------------------------------------------------------------------
direct_mode:
    call    text_screen

    ld      a,(BASYSCTL)
    and     KB_REPEAT
    or      KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn on keybuffer, ASCII mode, no repeat
    call    close_bas_fdesc       ; Clean up after aborted file commnds
    jp      HOOK2+1            

text_screen:
    in      a,(IO_VCTRL)
    and     a,~VCTRL_MODE_MC
    or      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    ret

;-----------------------------------------------------------------------------
; Hook 5: Push MAIN onto stack so modified CHEAD will return to it
;-----------------------------------------------------------------------------
linker_hook:
    ld    de,MAIN                 ; DE will get trashed anyway
    push  de                      ; Make MAIN the return address
    jp    LINKIT                  ; Link the lines
    
    
;-----------------------------------------------------------------------------
; Hook 12 - SCRTCH (Execute NEW statement)
;-----------------------------------------------------------------------------
new_hook:
	  call    page_restore_plus
	  call    clear_all_errvars
	  ld      c,0
	  call    spritle_toggle_all    ; Disable all sprite
	  jp      HOOK12+1

close_bas_fdesc:
    ld      a,(BAS_FDESC)         ; Get File Descriptor in Use
    or      a                     ; If Valid Descriptor
    ret     m
    call    dos_close_file        ;   Close the File
init_bas_fdesc:
    ld      a,128
    ld      (BAS_FDESC),a         ;   Set to No File

;-----------------------------------------------------------------------------
; Hook 13 - OUTDO 
;-----------------------------------------------------------------------------
outdo_hook:
  push    af                
  ld      a,(BASYSCTL)            ; Get System Control Bits
  rra                             ; Output to Buffer into Carry
  jr      nc,.not_buffered        ; If bit set
  jp      output_to_buffer        ;   Write to buffer 
.not_buffered:
  pop     af
  jp      HOOK13+1


;-----------------------------------------------------------------------------
; Hook 19 - TTYCHR (Print Character to Screen)
;-----------------------------------------------------------------------------
ttychr_hook:
    push    af
    cp      11
    jr      nz,.not_cls
    ld      a,(BASYSCTL)
    and     $FE
    ld      (BASYSCTL),a
.not_cls
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    jp      nz,ttychr80pop
    pop     af
    jp      TTYCH
    
