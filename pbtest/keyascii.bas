100 REM Test Alternate Port Keyboard Read
110 CLS 7,0:LOAD CHRSET "latin1cc.rom"

120 SR=$3000+284:CR=SR+1024
122 POKE SR+000,$"200102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F"
126 POKE SR+080,$"202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F"
128 POKE SR+120,$"404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F"
130 POKE SR+160,$"606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F"
132 POKE SR+240,$"808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9F"
134 POKE SR+320,$"A0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF"
136 POKE SR+360,$"C0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF"
138 POKE SR+400,$"E0E1E2E3E4E5E6E7E8E9EAEBECEFEEEFF0F1F2F3F4F5F6F7F8F9FAFBFEFDFEFF"

200 _LOOP:K=GETKEY
210 L=INT(K/32):C=K AND 31
220 L=L-1*(L>0)-1*(L>3)-1*(L>4)
230 POKE P,$70
240 A=CR+L*40+C:POKE A,$13
250 P=A
260 GOTO _LOOP
