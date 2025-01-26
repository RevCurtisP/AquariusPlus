;====================================================================
; File Related String Routines
;====================================================================

;-----------------------------------------------------------------------------
; Return file extension
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: extension length
;         DE: extension address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_get_ext:
    ld      l,'.'                 ; Delimiter
_chop_filespec:
    ex      de,hl                 ; HL = SpcAdr; E = Delim
    add     hl,bc                 ; HL = SpcEnd + 1
    ld      b,c                   ; B = Counter
    ld      c,0                   ; C = ExtLen
.loop
    dec     hl                    ; Move left
    ld      a,(hl)                ; Get character
    cp      e
    jr      z,.found              ; If not Delim
    inc     c                     ;   Bump length
    djnz    .loop                 ;   Count down
    ex      de,hl                 ;   Restore hl
    ld      c,b                   ;   BC = 0
    jr      .return               ; Else
.found
    ex      de,hl                 ; DE = DotAdr
    inc     de                    ; DE = ExtAdr
    ld      b,0                   ; BC = ExtLen
.return
    ld      a,c
    or      a
    ret

;-----------------------------------------------------------------------------
; Return filespec without directory
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: filename length
;         DE: filename address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_trim_dir:
    call    _chop_dir
    jr      z,.no_dir             ; If NamAdr <> StrAdr
    ld      a,c
    or      a
    jr      pop2hlr_aux 

.no_dir
    pop     bc                    ; BC = StrLen; ; Stack = StrAdr, RtnAdr
    pop     de                    ; HL = StrAdr; ; Stack = RtnAdr
    ld      a,c                   ; A = StrLen
    or      a
    ret
    
;-----------------------------------------------------------------------------
; Return directory portion of filespec
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: directory length
;         DE: directory address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_get_dir:
    call    _chop_dir
    jr      z,.no_dir             ; If NamAdr <> StrAdr
    pop     hl                    ; HL = OldLen; Stack = StrAdr, RtnAdr
    dec     bc
    jr      _no_ext
.no_dir
    xor     a                     ; Else
    ld      b,a                   ;   A,BC = 0
    ld      c,a
pop2hlr_aux:
    pop     hl                    ; Stack = StrAdr, RtnAdr
    pop     hl                    ; Stack = RtnAdr
    ret

_chop_dir:
    pop     ix                    ; IX = RtnAdr
    ld      a,c                   ; A = StrLen
    or      a                     ; If null string
    ret     z                     ;   Return
    push    de                    ; Stack = StrAdr, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, RtnAdr
    push    de                    ; Stack = StrAdr, StrLen, StrAdr, RtnAdr
    ld      l,'/'                 ; Delimiter
    call    _chop_filespec        ; DE = NamAdr, A,BC = NamLen
    pop     hl                    ; HL = StrAdr; Stack = StrLen, StrAdr, RtnAdr
    rst     COMPAR                ;
    jp      (ix)                  ; Return
;-----------------------------------------------------------------------------
; Return filespec without extension
; Input: BC: filespec length
;        DE: filespec address
; Output: A: extension length
;         BC: trimmed length
;         DE: filespec address
; Set Flags: NZ if file had extension, Z if it did not
; Clobbered: HL
;-----------------------------------------------------------------------------
file_trim_ext:
    ld      l,'.'                 ; Delimiter
    push    de                    ; Stack = StrAdr, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, RtnAdr
    call    _chop_filespec        ; DE = ExtAdr, A,BC = ExtLen
    pop     hl                    ; HL = OldLen; Stack = StrAdr, RtnAdr
    jr      z,_no_ext             ; If extension found
    inc     bc                    ;   BC = ExtLen + 1
_no_ext:
    sbc     hl,bc                 ; HL = ChpLen
    ld      b,h
    ld      c,l                   ; BC = ChpLen
    ld      a,c                   ; A = ChpLen
    or      a                     ; Set flags
    pop     de                    ; DE = StrAdr; Stack = RtnAdr
    ret

