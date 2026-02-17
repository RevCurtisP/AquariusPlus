;=====================================================================================
; Keyword Table and Routines to parse and expand tokens
; plusBASIC and MX-BASIC cross-refernce at bottom
;=====================================================================================

;-----------------------------------------------------------------------------
; plusBASIC keyword list
;-----------------------------------------------------------------------------
BTOKEN:     equ $CB               ; Our first token number

TBLCMDS:

    db $80 + 'X',"OR"             ; $CB  
    db $80 + 'P',"UT"             ; $CC
    db $80 + 'G',"ET"             ; $CD           
    db $80 + 'D',"RAW"            ; $CE          
    db $80 + ' '                  ; $CF           
    db $80 + 'L',"INE"            ; $D0           
    db $80 + 'S',"WAP"            ; $D1          
    db $80 + 'D',"OKE"            ; $D2  
    db $80 + 'T',"IME"            ; $D3   Replaces MX-BASIC keyword SDTM
    db $80 + 'E',"DIT"            ; $D4   Not Implemented
    db $80 + 'C',"LS"             ; $D5
    db $80 + 'L',"OCATE"          ; $D6
    db $80 + 'O',"UT"             ; $D7
    db $80 + 'P',"SG"             ; $D8
    db $80 + 'M',"OUSE"           ; $D9
    db $80 + 'C',"ALL"            ; $DA
    db $80 + 'L',"OAD"            ; $DB
    db $80 + 'S',"AVE"            ; $DC
    db $80 + 'D',"IR"             ; $DD
    db $80 + 'M',"KDIR"           ; $DE   Replaces USB-BASIC/MX-BASIC keyword CAT
    db $80 + 'D',"EL"             ; $DF
    db $80 + 'C',"D"              ; $E0
    db $80 + 'I',"N"              ; $E1
    db $80 + 'J',"OY"             ; $E2
    db $80 + 'H',"EX$"            ; $E3
    db $80 + 'R',"ENAME"          ; $E4
    db $80 + 'D',"ATE"            ; $E5   Replaces MX-BASIC keyword DTM$
    db $80 + 'D',"EC"             ; $E6             
    db $80 + 'M',"OD"             ; $E7             
    db $80 + 'D',"EEK"            ; $E8   
    db $80 + 'E',"RR"             ; $E9             
    db $80 + 'S',"TRING"          ; $EA             
    db $80 + 'B',"IT"             ; $EB             
    db $80 + ' '                  ; $EC             
    db $80 + 'E',"VAL"            ; $ED             
    db $80 + 'P',"AUSE"           ; $EE   Renamed MX-BASIC keyword SLEEP             
    db $80 + 'E',"LSE"            ; $EF             
    db $80 + 'T',"ILE"            ; $F0             
    db $80 + 'R',"GB"             ; $F1             
    db $80 + 'M',"AP"             ; $F2             
    db $80 + 'F',"ILE"            ; $F3             
    db $80 + 'R',"ESUME"          ; $F4             
    db $80 + 'C',"OL"             ; $F5             
    db $80 + 'S',"CREEN"          ; $F6             
    db $80 + 'S',"ET"             ; $F7             
    db $80 + 'W',"RITE"           ; $F8             
    db $80 + 'U',"SE"             ; $F9             
    db $80 + 'O',"PEN"            ; $FA             
    db $80 + 'C',"LOSE"           ; $FB             
    db $80 + ' '                  ; $FC             
    db $80 + ' '                  ; $FD             
    db $80             ; End of table marker

BXTOKEN = $80           ; First Extended Token

EXTCMDS:
    db $80 + 'A',"TTR"            ; $80
    db $80 + 'P',"ALETTE"         ; $81            
    db $80 + 'O',"FF"             ; $82             
    db $80 + ' '                  ; $83 unused (DATA)             
    db $80 + 'S',"PRITE"          ; $84
    db $80 + 'C',"HR"             ; $85            
    db $80 + 'K',"EY"             ; $86             
    db $80 + 'D',"EX"             ; $87             
    db $80 + 'F',"AST"            ; $88            
    db $80 + 'T',"EXT"            ; $89             
    db $80 + 'A',"RGS"            ; $8A             
    db $80 + 'S',"AMPLE"          ; $8B             
    db $80 + 'T',"RACK"           ; $`
    db $80 + 'P',"IXEL"           ; $8D             
    db $80 + ' '                  ; $8E unused (REM)             
    db $80 + 'N',"AME"            ; $8F             
    ; Primary Tokens grouped together, so extended dispatch can use DEC A
    db $80 + 'R',"ESET"           ; $90             
    db $80 + 'E',"XT"             ; $91             
    db $80 + 'V',"ER"             ; $92             
    db $80 + 'F',"ILL"            ; $93
    db $80 + 'C',"OMPARE"         ; $94
    db $80 + 'P',"LAY"            ; $95
    db $80 + 'A',"PPEND"          ; $96
    db $80 + 'T',"RIM"            ; $97
    db $80 + 'S',"TASH"           ; $98
    db $80 + 'T',"RO"             ; $99             
    db $80 + 'B',"REAK"           ; $9A             
    db $80 + 'L',"OOP"            ; $9B             
    db $80 + 'S',"TR"             ; $9C             
    db $80 + 'V',"AR"             ; $9D             
    db $80 + 'E',"RASE"           ; $9E
    db $80 + 'S',"PLIT"           ; $9F
    
    db $80 + 'P',"AD"             ; $A0
    db $80 + 'W',"ORD"            ; $A1
    db $80 + 'C',"LIP"            ; $A2
    db $80 + 'P',"TR"             ; $A3
    db $80 + 'S',"TATUS"          ; $A4
    db $80 + 'B',"YTE"            ; $A5
    db $80 + 'C',"AQ"             ; $A6
    db $80 + 'M',"EM"             ; $A7
    db $80 + 'J',"OIN"            ; $A8
    db $80 + 'W',"AIT"            ; $A9
    db $80 + 'C',"UR"             ; $AA
    db $80 + 'H',"EX"             ; $AB
    db $80 + 'B',"IN"             ; $AC
    db $80 + 'M',"IN"             ; $AD
    db $80 + 'M',"AX"             ; $AE
    db $80 + 'U',"PR"             ; $AF
    db $80 + 'L',"WR"             ; $B0
    db $80 + 'S',"PEED"           ; $B1
    db $80 + 'L',"ONG"            ; $B2
    db $80 + 'F',"LOAT"           ; $B3
    db $80 + 'P',"ATH"            ; $B4
    db $80 + 'D',"UMP"            ; $B5
    db $80 + 'B',"ORDER"          ; $B6
    db $80 + 'C',"HECK"           ; $B7
    db $80 + 'R',"EPEAT"          ; $B8
    db $80 + 'U',"NTIL"           ; $B9
    db $80 + 'R',"AN"             ; $BA
    db $80 + 'D',"OM"             ; $BB
    db $80 + 'E',"XT"             ; $BC
    db $80 + 'S',"CROLL"          ; $BD
    db $80 + 'R',"ECT"            ; $BE
    
    db $80                        ; End of table marker

EXTOKEN = $BF                     ; Last Token + 1

;-----------------------------------------------------------------------------
; plusBASIC tokens
;-----------------------------------------------------------------------------
XORTK     equ     $CB
PUTTK     equ     $CC
GETTK     equ     $CD
SWAPTK    equ     $D1
TIMETK    equ     $D3    
OUTTK     equ     $D7
MOUSTK    equ     $D9
DIRTK     equ     $DD
MKDTK     equ     $DE
DELTK     equ     $DF
CDTK      equ     $E0
INTK      equ     $E1
HEXSTK    equ     $E3
DECTK     equ     $E6
MODTK     equ     $E7
DATETK    equ     $E5
ERRTK     equ     $E9
STRNTK    equ     $EA
BITTK     equ     $EB
ELSETK    equ     $EF
LINETK    equ     $D0
SAVETK    equ     $DC
TILETK    equ     $F0
RGBTK     equ     $F1
MAPTK     equ     $F2
FILETK    equ     $F3
COLTK     equ     $F5
SCRNTK    equ     $F6
SETTK     equ     $F7
USETK     equ     $F9

;-----------------------------------------------------------------------------
; Extended tokens
;-----------------------------------------------------------------------------
ATTRTK    equ     $80
PALETK    equ     $81
OFFTK     equ     $82
SPRITK    equ     $84
CHRTK     equ     $85
KEYTK     equ     $86
DEXTK     equ     $87
FASTK     equ     $88
TEXTK     equ     $89
ARGSTK    equ     $8A
SAMPTK    equ     $8B
TRKTK     equ     $8C
PIXTK     equ     $8D
NAMETK    equ     $8F
RESETK    equ     $90
EXTTK     equ     $91
VERTK     equ     $92
FILLTK    equ     $93
COMPTK    equ     $94
PLAYTK    equ     $95
APNDTK    equ     $96
TRIMTK    equ     $97
BRKTK     equ     $9A
LOOPTK    equ     $9B
STRTK     equ     $9C
VARTK     equ     $9D
SPLITK    equ     $9F
PADTK     equ     $A0 
WORDTK    equ     $A1
CLIPTK    equ     $A2
PTRTK     equ     $A3
STATK     equ     $A4
BYTETK    equ     $A5
CAQTK     equ     $A6
MEMTK     equ     $A7
JOINTK    equ     $A8
WAITK     equ     $A9
CURTK     equ     $AA
HEXTK     equ     $AB
BINTK     equ     $AC
MINTK     equ     $AD
MAXTK     equ     $AE
UPRTK     equ     $AF
LWRTK     equ     $B0
SPEEDTK   equ     $B1
LONGTK    equ     $B2
FLOATK    equ     $B3
PATHTK    equ     $B4
DUMPTK    equ     $B5
BORDTK    equ     $B6
CHECKTK   equ     $B7
REPEATK   equ     $B8
UNTILTK   equ     $B9
RANTK     equ     $BA
DOMTK     equ     $BB
EXTK      equ     $BC
SCROLTK   equ     $BD
RECTK     equ     $BE

;-----------------------------------------------------------------------------
keyword_to_token:
    ld      a,b                   ; A = current index
    
    cp      BTOKEN                ; If < BTOKEN then keyword was found in BASIC table
    jp      c,HOOK10+1            ;   So go do it

; Use plusBASIC simple token table
    ld      ix,.ext_tokens
    push    ix                    ; Make CRUNCX return to extended token routine

    ; Set our own keyword table and let BASIC code use that instead
    ex      de,hl                 ; HL = Line buffer
    ld      de,TBLCMDS - 1        ; DE = our keyword table
    ld      b,BTOKEN - 1          ; B = our first token
        
    jp      CRUNCX                ; Continue searching using our keyword table
    
.ext_tokens:    
    ld      a,b                   ; A = current index
    cp      XTOKEN
    jp      c,HOOK10+1

    ex      de,hl                 ; HL = Line buffer
    ld      de,EXTCMDS - 1        ; DE = our keyword table
    ld      b,BXTOKEN - 1         ; First Extended Token

    ld      ix,.ext_done          ;   Make CRUNCX skip hook when done
    push    ix
    jp      CRUNCX                ; Continue searching using our keyword table
    
.ext_done:
    ld      a,b                   ; A = current index
    cp      EXTOKEN               ;
    jp      nc,HOOK10+1
    ld      a,c                   ; Get token
    ex      af,af'                ; Save token
    ex      de,hl                 
    pop     bc                    
    pop     de                    
    ld      a,XTOKEN
    ld      (de),a
    inc     de                    
    inc     c
    ex      af,af'
    jp      STUFFH


;-----------------------------------------------------------------------------
; Convert token to keyword - hook 22
;
; This function will check if the passed token is one of the stock BASIC or
; our extra commands. If it one of our commands, we pass our command table
; to the ROM code.
;-----------------------------------------------------------------------------
token_to_keyword:
    cp      BTOKEN              ; Is it one of our tokens?
    jr      nc, .expand_token   ; Yes, expand it
    jp      HOOK22+1            ; No, return to system for expansion

.expand_token:
    cp      XTOKEN             ; If extended token prefix
    jr      nz,.not_xtoken
    ld      a,(hl)              ; Get extended token
    inc     hl
    sub     BXTOKEN - 1
    ld      de,EXTCMDS
    jr      .expand_it

.not_xtoken
    sub     BTOKEN - 1
    ld      de, TBLCMDS         ; DE = table of AquBASIC command names
.
.expand_it:
    ld      c, a                ; C = offset to AquBASIC command
    jp      RESSRC              ; Print keyword indexed by C


; * = Likely to be replaced
;
;     plusBASIC   MX-BASIC             Conversion
; $CB XOR         INSTR            IN STRING <--> INSTR  
; $CC PUT         PUT    
; $CD GET         GET    
; $CE             DRAW   
; $CF             CIRCLE  
; $D0             LINE                compatible
; $D1 SWAP        SWAP   
; $D2 FILL        DOKE              POKE! <--> DOKE
; $D3 TIME        SDTM   
; $D4             EDIT   
; $D5 CLS         CLS    
; $D6 LOCATE      LOCATE 
; $D7 OUT         OUT    
; $D8 PSG         PSG    
; $D9 MOUSE       DEBUG  
; $DA CALL        CALL   
; $DB LOAD        LOAD   
; $DC SAVE        SAVE   
; $DD DIR         DIR    
; $DE CAT         MKDIR    
; $DF DEL         DEL                  compatible
; $E0 CD          CD                   compatible
; $E1 IN          IN                   compatible
; $E2 JOY         JOY                  compatible
; $E3 HEX         HEX                  compatible
; $E4 RENAME      VER
; $E5 DATE        DTM           DATETIME$ <--> DTM$(0)
; $E6 DEC         DEC
; $E7 MOD         KEY             
; $E8 DEEK        DEEK              
; $E9 ERR         ERR [OR]            ERR <--> ERR(0), ERRLINE <--> ERR(1), ERR$ <--> ERR$(1)
; $EA STRING      STRING
; $EB BIT         XOR           (a XOR b) <--> XOR(a,b)
; $EC             MENU
; $ED EVAL        EVAL                 compatible
; $EE PAUSE       SLEEP
; $EF ELSE        MKDIR
; $F0 TILE        RMDIR               DEL <--> RMDIR
; $F1 RGB         OFF
; $F2 MAP         WAIT
; $F3 FILE        FILE
; $F4 RESUME      RESUME
; $F5 COL         COL [OR]

; $FE extended token prefix
; $FF (pseudovar prefix)
