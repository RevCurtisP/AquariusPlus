; Hello World Aquarius+ Executable

print_string_immd   equ  $2112

prog_len = end_addr - load_addr

    org     0
    byte    "AQPLUS"                ; File Header

    phase   0
    byte    "EXEC"                  ; Resource Type
    word    prog_len,0              ; Resource Length
    byte    "hello"                 ; Resource ID
    dc      24-$,0                  ; Pad ID
    word    load_addr               ; Resource Header
    word    exec_addr
    dc      64-$,0                  ; Pad Header
    dephase

; Program   
    phase   $4000
load_addr:
exec_addr:
    call    print_string_immd
    byte    "Hello world!",13,10,0
    ret
end_addr:
    dephase
    