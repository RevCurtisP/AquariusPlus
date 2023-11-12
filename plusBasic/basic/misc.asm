;====================================================================
; Miscellaneous Assembly Calls used by BASIC
;====================================================================

;-----------------------------------------------------------------------------
; Write string to Function Key buffer
; Input: A: Function Key (0-15)
;       BC: String length
;       DE: String Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
fnkey_write_buffer:
    ex      de,hl                 ; HL = String Address
    ld      d,FKEYBASE/512        ; 011??000 
    and     $0F                   ; 011??000 0 0000XXXX
    rla                           ; 011??000 0 000XXXX0
    rla                           ; 011??000 0 00XXXX00
    rla                           ; 011??000 0 0XXXX000
    rla                           ; 011??000 0 XXXX0000
    rla                           ; 011??000 X XXX00000
    rl      d                     ; 11??000X 0 XXX00000   
    ld      e,a                   ; DE = Buffer Address
    ld      a,BAS_BUFFR
    call    page_write_bytes
    ld      a,BAS_BUFFR
    inc     de
    jp      page_write_byte
    
;-----------------------------------------------------------------------------
; S3 BASIC patch
; Skip Label in ON GOSUB
;-----------------------------------------------------------------------------
skip_on_label:
    rst     CHRGET            ;;Get first character of LineRef            
    cp      '_'               ;;If not a label                            
    jp      nz,SCNLIN         ;;  Scan Line Number                        
.loop                                                                              
    rst     CHRGET            ;;Get next character                        
    ret     z                 ;;Return if end of line                     
    cp      ','                                                           
    ret     z                 ;;Return if comma                           
    cp      ':'                                                           
    ret     z                 ;;Return if colon                           
    jr      .loop             ;;Check next character                      
