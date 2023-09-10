;; Read byte from tape

;zmac --zmac -o readtape.cim -o monfc.lst readtape.asm

        org     $BF00             ; CALL 48896
        push    hl

_cls:   ld      a,$06
        ld      hl,$3000
        ld      c,25
.line:  ld      b,40
.char:  ld      (hl),' '
        set     2,h
        ld      (hl),a
        res     2,h
        inc     hl
        djnz    .char
        dec     c
        jr      nz,.line

.next:  ld      hl,$3000+40       ; Starting position for trace
        
.read:  
        call    $1B4D             ; RDBYTE
        ld      (hl),a
        inc     hl
        ld      a,h
        cp      $34
        jr      c,.read
        jr      .next

        pop     hl
        ret

