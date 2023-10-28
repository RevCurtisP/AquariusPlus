;=====================================================================================
; S3BASIC Enhancement Hooks 
; These routines are located in the 8K ROM in Bank 2 and neither call nor are 
; called by routines in Bank 1 ROM
; The routines are called through a jump table at $2000, then routed through 
; routine _call_bank2_routine which destroys AF'
;=====================================================================================

;-----------------------------------------------------------------------------
; Save MAIN Line Number Flag
;-----------------------------------------------------------------------------
s3_main_ext:
    pop     de                    ; Pop Return Address
    pop     bc                    ; C = Line Number Flag
    ld      (TEMP3),bc            ; Save it
    push    bc                    ; Flag back on stack
    push    de                      ; Return address back on stack
    jp      SCNLIN                ; Continue to SCNLIN

;-----------------------------------------------------------------------------
; Don't tokenize unquoted literal string after DOS command in direct mode
;-----------------------------------------------------------------------------
s3_stuffh_ext:
    cp      DATATK-':'            ; If DATA
    jp      z,COLIS               ;   Continue STUFFH
    ex      af,af'
    ld      a,(TEMP3)             ; Get Line# Flag
    and     $01                   ; If carry set
    jr      nz,.exaf_nodatt       ;   Continue  STUFFH
    ex      af,af'
    cp      DIRTK-':'             ; If Not DIRTK through CDTK

    jp      c,NODATT              ;
    cp      CDTK-':'+1            ;
    jp      nc,NODATT             ;  Continue STUFFH
.space_loop
    ld      a,(hl)                ; Eat Spaces
    cp      ' '
    jr      nz,.not_space
    call    STUFFS
    jr      .space_loop
.not_space
    ld      b,a                   ; Set up delimiter for STRNG
    cp      '"'                   ; If quotes
    jp      z,STRNG               ;   Go stuff quoted string
.string_loop
    ld      a,(hl)                ; Get character
    or      a                     ; If end of line
    jp      z,CRDONE              ;   Finish it up
    cp      ' '                   ; If space
    jp      z,KLOOP               ;   Stuff it and continue
    cp      ':'                   ; If colon
    jp      z,KLOOP               ;   Stuff it and continue
    call    STUFFS                ; Else Stiff it
    jr      .string_loop          ;   and check next character
.exaf_nodatt:
    ex      af,af'
    jp      NODATT

