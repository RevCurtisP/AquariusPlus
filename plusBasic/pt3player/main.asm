;----------------------------------------------------
; Project: pt3standalone.zdsp
; Modified to be interrupt driven by Mack Wharton
; Main File: main.asm
;----------------------------------------------------

; Generates a binary file to be included in the plusBASIC ROM

SongData  = $4400               ; (For now)

 org     0xD800                 ; Last 2k of Auxilliary ROM

;Jump table
      jp      StartPlayer       ; Initialize Player
      jp      PlayQuark         ;
      jp      CHECKLP
      jp      MUTE

StartPlayer:
      ld      (CountDn),a       ; $FF = 60Hz, 0 = 50Hz
      LD      HL,SongData
      CALL    PT3_PLAY
      or      a                 ; Return Zero set
      RET

; Interrupt call
; Sets Non-Zero when song has finished.
PlayQuark:
      ld      a,(CountDn)
      or      a
      jp      m,.play
      jr      nz,.decrement
      ld      a,6
.decrement
      dec     a
      ld      (CountDn),a
.play
      LD      HL,SongData
      CALL    PLAY        
      ld      hl,SETUP
      bit     7,(hl)            ; quit if end of song
      ret
      jr      nz,StartPlayer
      RET




include  "macros.i"   ; structure macros
include   PTPlayer.asm
 
      dc $E000-$,$76