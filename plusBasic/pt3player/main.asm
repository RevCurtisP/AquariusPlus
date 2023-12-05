;----------------------------------------------------
; Project: pt3standalone.zdsp
; Modified to be interrupt driven by Mack Wharton
; Main File: main.asm
;----------------------------------------------------

; Generates a binary file to be included in the plusBASIC ROM

SongData  = $5000               ; (For now)

 org     0xD800                 ; Last 2k of Auxilliary ROM

;Jump table
      jp      StartPlayer       ; Initialize Player
      jp      PlayQuark         ;
      jp      CHECKLP
      jp      MUTE


StartPlayer:
      push    hl
      LD      HL,SongData
      CALL    PT3_PLAY
      pop     hl
      RET

PlayQuark:
      push    hl
      LD      HL,SongData
      CALL    PLAY        
      ld      hl,SETUP
      bit     7,(hl)            ; quit if end of song
      pop     hl
      jr      nz,StartPlayer
      RET




include  "macros.i"   ; structure macros
include   PTPlayer.asm
 
      dc $E000-$,$76