;====================================================================
; Character Set kernel functions
;====================================================================

;-----------------------------------------------------------------------------
; Initialize Character ROM buffers
;-----------------------------------------------------------------------------
chrset_init:
; Copy standard character set into buffer
    ld      hl,.defdesc
    call    file_load_defchrs
    ld      hl,.altdesc
    call    file_load_altchrs
    ld      a,BAS_BUFFR
    call    page_map_bank1
    ld      hl,BANK1_BASE+DEFCHRSET ; HL = SrcAdr
    ld      de,BANK1_BASE+SWPCHRSET ; DE = DstAdr
    ld      bc,2048
    ldir
    jp      page_restore_bank1

.defset:
    byte    "esp:default.chr"
.defdesc:
    word    $-.defset,.defset
.altset:
    byte    "esp:latin1b.chr"
.altdesc:
    word    $-.altset,.altset

chrset_default:
   xor      a
;-----------------------------------------------------------------------------
; Copy selected character set into Character RAM
; Input: A: Character set (0: Default, 1: Custom)
; Clobbered: AF',BC,DE,HL,IX
;-----------------------------------------------------------------------------
chrset_select:
    ld      hl,DEFCHRSET          ; If A = 0
    or      a                     ;   Copy standard character set
    jr      z,.copy               ; Else
.alt
    ld      hl,ALTCHRSET          ;   Copy Custom Character Set
.copy
    ld      a,BAS_BUFFR
    ex      af,af'
    ld      a,CHAR_RAM
    ld      bc,2048
    ld      de,0
    jp      page_fast_copy_sys    ; Copy It

chrset_stash:
    ld    a,BAS_BUFFR             ; A = DstPg
_stash:
    ex    af,af'                  ; A' = DstPg
    ld    a,CHAR_RAM              ; A = SrcPg
    ex    af,af'                  ; A = DstPg, A' = SrcPg
    ld    hl,0                    ; SrcAdr = 0
    ld    de,SWPCHRSET            ; DstAdr = SwpBuf
    jr    _copy_chrset            ; Copy and return

chrset_swap:
    ld    a,TMP_BUFFR             ; A = DstPg
    call  _stash                  ; Copy ChrRAM to TmpBuf
    call  chrset_restore          ; Copy ChrSwp to ChrRAM
    ld    a,TMP_BUFFR
    ex    af,af'                  ; A' = SrcPg
    ld    a,BAS_BUFFR             ; A = DstPg
    ld    hl,SWPCHRSET            ; DstAdr = SwpBuf
    ld    de,SWPCHRSET            ; DstAdr = TmpBuf
    jr    _copy_chrset            ; Copy and return
    
chrset_restore:
    ld    a,BAS_BUFFR             ; A = SrcPg
    ex    af,af'                  ; A' = DstPg
    ld    a,CHAR_RAM              ; A = DstPg
    ld    hl,SWPCHRSET            ; SrcAdr = SwpBuf
    ld    de,0                    ; DstAdr = 0
_copy_chrset:
    ld    bc,2048
    jp    page_fast_copy_sys      ; Copy and return


