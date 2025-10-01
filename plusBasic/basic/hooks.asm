;====================================================================
; plusBASIC Hook Handlers
;====================================================================

;-----------------------------------------------------------------------------
; Hook 2 - READY (Enter Direct Mode
;-----------------------------------------------------------------------------
;; ToDo: Always enable key repeat in direct mode?
direct_mode:
    call    text_screen

    ld      a,(BASYSCTL)
    and     ~BASBRKOFF            ; Enable Ctrl-C break
    ld      (BASYSCTL),a
    and     KB_REPEAT
    or      KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn on keybuffer, ASCII mode, no repeat
    call    set_cursor_on
    call    ferr_flag_on
    call    close_bas_fdesc       ; Clean up after aborted file commnds
    jp      HOOK2+1            

set_cursor_on:
    ld      a,$FF
set_cursor_mode:    
    xor     $FF                   ; 0 = On, $FF = Off
    and     CRSR_OFF
    ld      b,a
    ld      a,$7F
    jr      z,.disp_cursor
    ld      a,(CURCHR)
.disp_cursor
    ld      de,(CURRAM)
    ld      (de),a
    ld      a,(SCREENCTL)
    and     $FF-CRSR_OFF
    or      b
    ld      (SCREENCTL),a
    ret
    
get_cursor_mode:
    ld      a,(SCREENCTL)
    and     CRSR_OFF
    ld      a,$FF
    ret     z
    cpl
    ret

ferr_flag_on:
    ld      a,FERR_FLAG
    jp      set_ferr_flag
    
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
	  call    page_set_plus
    call    clear_run_args
	  call    clear_all_errvars
	  ld      c,0
	  call    spritle_toggle_all    ; Disable all sprite
	  jp      HOOK12+1

close_bas_fdesc:
    ld      a,(BAS_FDESC)         ; Get File Descriptor in Use
    or      a                     ; If Valid Descriptor
    ret     z
    ld      iy,dos_close           
    jp      p,.notdir
    ld      iy,dos_close_dir
.notdir
    and     $7F
    dec     a
    call    aux_call              ; Close the File
init_bas_fdesc:
    xor     a
    ld      (BAS_FDESC),a         ;   Set to No File
    ret

;-----------------------------------------------------------------------------
; Hook 30 - Check for label at beginning of line, then search for it's line
;-----------------------------------------------------------------------------
;; Proposed: GOTO/GOSUB ( expression )
scan_label:
    dec     hl                    ; Back up in case of space
    rst     CHRGET                ; Reget current character
    cp      '_'
    jr      nz,.not_label         ; If not underscore
    ex      de,hl                 ; DE = Text Pointer
    ld      (TEMP8),de            ; Save it
    ld      hl,(TXTTAB)           ; HL = start of program,
.line_loop:
    ld      (TEMP2),hl            ; Save pointer to line
    ld      c,(hl)                ; BC = link to next line
    inc     hl
    ld      b,(hl)
    inc     hl
    ld      (TEMP3),bc            ; Save for later
    ld      c,(hl)                ; BC = line#
    inc     hl
    ld      b,(hl)
    inc     hl
    ld      (OLDLIN),bc           ; Save it
    ld      a,(hl)                ; Get first character
    cp      '_'                   ; If not underscore
    jr      nz,.next_line         ;   Skip to next lines
.label_loop:
    ld      a,(de)                ; Next character from label
.not_lspace
    call    .check_colon          ; Treat colon as terminator
    ld      b,a                   ; Put in B for compare
.text_loop:
    ld      a,(hl)                ; Get character from line
.not_tspace
    call    .check_colon          ; Treat colon as terminator
    cp      b                     ; If characters don't match
    jp      nz,.no_match          ;
    or      a                     ; If both are terminators
    jr      z,.found_it           ;   Finish up
    inc     hl                    ; HL is Text Pointer
    inc     de                    ; Move it on up
    jr      .label_loop           ; Check next character
.no_match
    ld      de,(TEMP8)            ; Restore pointer to label
.next_line
    ld      hl,(TEMP3)            ; Get Link to next line
    ld      a,h                   ; If link to next line is $0000
    or      l                     ;   End of program
    jp      z,ULERR               ;   So Undefined Line error
    jr      .line_loop            ; Scan the next line
.found_it
    pop     af                    ; Get return address
    cp      $06
    jr      nz,.not_goto          ; If we came from GOTO
    ld      bc,(OLDLIN)           ;   Retrieve the line #
    ld      (CURLIN),bc           ;   and make it the current line
    ret                           ;   Return to NEWSTT
.not_goto
    cp      $05
    jr      nz,.not_list          ; If we came from LIST
    ld      bc,(TEMP2)            ;   BC = Pointer to Line
    jp      LIST3                 ;   Start listing
.not_list
    cp      $0C                   ; If we came from RESTORE
    jp      z,BGNRST              ;   Load DATPTR, HL = Text Pointer, and Return
    ex      de,hl                 ; HL = New text pointer
    ld      de,(TEMP2)            ; DE = Pointer to Line
    jp      reset_trap            ; Finish ON ERROR GOTO

.not_label:
    jp      SCNLIN                ;   Scan line number and return to GOTO

.check_colon:
    cp      ' '                   ; If space
    jr      z,.ret_zero           ;
    cp      ','                   ; or comma
    jr      z,.ret_zero           ;
    cp      ':'                   ; or colon
    ret     nz
.ret_zero
    xor     a                     ;   Treat like terminator
    ret

;-----------------------------------------------------------------------------
; Extension to CLEAR command
;-----------------------------------------------------------------------------
clear_extension:
    call    esp_close_all         ; Close all files
    xor     a
    ld      (BAS_FDESC),a         ; Clear currently open file
    ld      hl,(STRSPC)           ; Set temp buffer pointer
    ld      (TMPBUFTOP),hl        ; to start of string space
    ld      hl,(VARTAB)
    jp      CLEARV

;-----------------------------------------------------------------------------
; Hook 33: Save MAIN Line Number Flag
;-----------------------------------------------------------------------------
main_ext:
    pop     de                    ; Pop Return Address
    pop     bc                    ; C = Line Number Flag
    ld      (TEMP3),bc            ; Save it
    push    bc                    ; Flag back on stack
    push    de                      ; Return address back on stack
    jp      SCNLIN                ; Continue to SCNLIN

;-----------------------------------------------------------------------------
; Hook 37: Skip label at begin of line                                           
;-----------------------------------------------------------------------------
skip_label: 
    ld      (CURLIN),hl           ; Save the Line #                        
    ld      a,(EXT_FLAGS)
    rla     
    jr      nc,.not_trace
    push    de                    ; Save text pointer
    ld      a,'['
    call    OUTDO
    call    LINPRT
    ld      a,']'
    call    OUTDO
    pop     de
.not_trace
    ex      de,hl                 ; DE = Line#, HL = Text Pointer          
    rst     CHRGET                ; Get first character                    
    cp      '_'                   ; If not underscore                      
    jr      nz,.gone              ;   Execute rest of line                 
.loop      
    rst     CHRGET                ; Get next charcter                      
    or      a                     ; If end of line                         
    jr      z,.gone               ;   done                                 
    cp      ':'                   ; If not a colon                         
    jr      nz,.loop              ;   Keep going                           
.gone    
    dec     hl                    ; Back up text pointer                   
    jp      GONE                  ; Execute rest of line                   

;-----------------------------------------------------------------------------
; Hook 38: Skip Label in ON GOSUB
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

;-----------------------------------------------------------------------------
; Hook 40: Double SOUNDS delay timer if in Turbo Mode
;-----------------------------------------------------------------------------
sounds_hook:
    in      a,(IO_SYSCTRL)
    and     SYSCTRL_TURBO
    jr      z,.not_turbo
    ld      hl,2
    add     hl,de
    add     hl,de                 ; HL = DE + DE + 2
    ex      de,hl                 ; DE = HL
.not_turbo
    ; Normal: 1102 Hz [3.579545 MHz / (149 + 3100) cycles] - Divisor 3249 
    ; New Turbo: 1105 Hz [7.157090 Mhz / (149 + 6324) cycles] - Divisor 6473
    ; Old Turbo: 1127 Hz [7.157090 Mhz / (149 + 6200) cycles] - Divisor 6349
    ; Unmodified: 2203 Hz  [7.157090 Mhz / (149 + 3100) cycles] - Divisor 3249 
    jp      SOUNDS                ; Total wavelength: 148 + DE * 62 cycles 

