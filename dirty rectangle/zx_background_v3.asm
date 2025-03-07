; This command tells Zeus where to put the code it generates. As a szx file... Alter the path to suit your system

 zeusemulate "48K"
 output_szx "spi.szx",32768,START     ; The szx file

; output_bin "c:\spi.bin",$0000,$10000    ; The binary file ; If for some reason you want binary, uncomment this line
 ORG 32768

SCREEN          EQU 16384

; STACK                 EQU 63*1024     ; the stack
TMP_BUF EQU 49*1024 ; the temp buffer overwritten by the back buffer and sprites
BCK_BUF EQU TMP_BUF+(8*1024) ; the back buffer

SCREEN_ROW      EQU 32
SCREEN_LINE     EQU 192
SCREEN_SIZE     EQU SCREEN_ROW*SCREEN_LINE

ATTRIB          EQU 22528
ATTRIB_ROW      EQU 32
ATTRIB_LINE     EQU 24
ATTRIB_SIZE     EQU ATTRIB_ROW*ATTRIB_LINE

START
 DI                      ; interrupts off
 LD SP,MEMTOP            ; set stack to end STACK?

 LD HL,ATTRIB            ; attr start                             ; Clear the attributes
 LD DE,ATTRIB+1          ; sttr start +1
 LD BC,ATTRIB_SIZE-1     ; attr size -1
 LD (HL),7               ; clear first attr to white
 LDIR                    ; copy

; CALL CLEAR_SC
 CALL CLEAR_SB

; LD HL,BCK_BUF            ; attr start                             ; Clear the attributes
; LD DE,SCREEN          ; sttr start +1
; LD BC,SCREEN_SIZE     ; attr size -1
; LDIR                    ; copy


 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

; LD A,3         ; bottom three bits of A contain the border color
; OUT (254),A

MAIN_LOOP
 CALL V_BLANK

 LD A,4         ; bottom three bits of A contain the border color
 OUT (254),A

 LD A,(py)
 LD L,A
 LD A,(px)
 LD E,A
 CALL BB_TB_6X6_V2  ; SB_TB_6X6

 LD A,(py)
 LD L,A
 LD A,(px)
 LD E,A
 CALL TB_SN_6X6_V2 ; TB_SN_6X6

 LD A,3         ; bottom three bits of A contain the border color
 OUT (254),A

; LD E, 20
; LD C, 30
; LD A, %0010101
; CALL CLEAR_CHAR_SB

 CALL MOVE_POINTS

 CALL KEYBOARD
JP MAIN_LOOP

sttx defb 12
stty defb 12
endx defb 95
endy defb 97
sdx defb 1
sdy defb 1
edx defb 1
edy defb 1

px defb 100
py defb 150

; keys = port bit 0   1   2 3 4
;        fefe     sht z   x c v
;        fdfe     a   s   d f g
;        fbfe     q   w   e r t
;        f7fe     1   2   3 4 5
;        effe     0   9   8 7 6
;        dffe     p   o   1 u y
;        bffe     ent l   k j h
;        7ffe     spc sym m n b

KEYBOARD

 LD BC,$FBFE     ; Load BC with the row port address
 IN A,(C)        ; Read the port into the accumulator
 AND $01         ; q
 JP NZ,Q_KEY_N   ; not pressed
  LD A, (px)
  DEC A
  LD (px), A
Q_KEY_N

 LD BC,$DFFE
 IN A,(C)
 AND $01         ; p
 JP NZ,P_KEY_N
  LD A, (px)
  INC A
  LD (px), A
P_KEY_N

  LD BC,$FDFE
 IN A,(C)
 AND $01         ; a
 JP NZ,Z_KEY_N
  LD A, (py)
  DEC A
  LD (py), A
Z_KEY_N

 LD BC,$f7FE
 IN A,(C)
 AND $01         ; 1
 JP NZ,M_KEY_N
  LD A, (py)
  INC A
  LD (py), A
M_KEY_N
 RET

MOVE_POINTS
 LD a, (sttx)
       ld b, a
       LD a, (sdx)
        ld c, a
        LD H, 0
        LD L, 191
        call EDGE
        ld a, b
        ld (sttx), a
        ld a, c
        ld (sdx), a

        LD a, (stty)
        ld b, a
        LD a, (sdy)
        ld c, a
        LD H, 0
        LD L, 255
        call EDGE
        ld a, b
        ld (stty), a
        ld a, c
        ld (sdy), a

        LD a, (endx)
       ld b, a
       LD A, (edx)
        ld c, a
       LD H, 0
       LD L, 191
       call EDGE
        ld a, b
       ld (endx), a
        ld a, c
       ld (edx), a

       LD a, (endy)
       ld b, a
       LD a, (edy)
       ld c, a
       LD H, 0
       LD L, 255
       call EDGE
        ld a, b
       ld (endy), a
        ld a, c
       ld (edy), a
ret

; BC pos dir
; HL limits
EDGE
 LD A,   b
     ADD     a, c
     cp      h
     jp z, REVERSE
     cp l
     jp z, REVERSE
     LD b, a
RET

REVERSE
 LD b, a
        LD a, c
        NEG
        LD c, a
RET

V_BLANK:
 LD DE,$1140       ; attr into D, MSB of port addr into E
FB_LP
  INC HL          ; padding instruction
  LD A,E          ; MSB of port addr into A
  IN A,($ff)      ; read port 0x40FF into A
  CP D            ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
  JP NZ,FB_LP     ; no? keep trying
RET

BK_HOR DEFB 0

SCN_TMP_BUF_POS MACRO () ; L=ver E=hor
 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH TMP_BUF_TAB_HI      ; #7
 LD D, (HL)                     ; TMP BUF HI #7
 INC H                          ; #4
 ADD A, (HL)                    ; TMP BUF LO #7
 INC H                          ; #4
 LD H, (HL)                     ; SCN BUF HI #7
 LD L, A                        ; #4
 LD E, A                        ; #4
MEND

SCN_TMP_BUF_NEXT_LINE MACRO ()
 ADD HL, SP
 EX DE, HL
 ADD HL, SP
 EX DE, HL
MEND

CLEAR_SB
 ld a,0
 LD B,31
CLEAR_SB_X
  ld c,b
  PUSH BC
  LD B,23
CLEAR_SB_Y
   LD E,B
   PUSH BC
   INC A
   CALL CLEAR_CHAR_SB
   POP BC
   DJNZ CLEAR_SB_Y
  POP BC
  DJNZ CLEAR_SB_X
RET

CLEAR_CHAR_SB
 EX AF,AF'        ; A=CHAR
 LD D, HIGH SBCH2 ; SCREEN CHAR HI
 LD A,(DE)        ; E=VER
 LD H,A
 LD D, HIGH SBCL2 ; SCREEN CHAR LO
 LD A,(DE)
 ADD A,C          ; C=HOR
 LD L,A
 EX AF,AF'

 LD B,8
CLEAR_CHAR_SB_LOOP
 LD (HL),A        ; B=CHAR
 INC H
 DJNZ CLEAR_CHAR_SB_LOOP
RET

CLEAR_SC
 ld a,0
 LD B,31
CLEAR_SC_X
  ld c,b
  PUSH BC
  LD B,23
CLEAR_SC_Y
   LD E,B
   PUSH BC
   INC A
   CALL CLEAR_CHAR_SC
   POP BC
   DJNZ CLEAR_SC_Y
  POP BC
  DJNZ CLEAR_SC_X
RET

CLEAR_CHAR_SC
 EX AF,AF'        ; A=CHAR
 LD D, HIGH SCCH2 ; SCREEN CHAR HI
 LD A,(DE)        ; E=VER
 LD H,A
 LD D, HIGH SCCL2 ; SCREEN CHAR LO
 LD A,(DE)
 ADD A,C          ; C=HOR
 LD L,A
 EX AF,AF'

 LD B,8
CLEAR_CHAR_SC_LOOP
 LD (HL),A        ; B=CHAR
 INC H
 DJNZ CLEAR_CHAR_SC_LOOP
RET

NEXT_LINE_LDI_LDD MACRO () ; going from forwards (ldi) on this line to backwards (ldd) on next line
 DEC E
 DEC L
 INC H ; next src line down
 INC D ; next dst line down
MEND

NEXT_LINE_LDD_LDI MACRO () ; going from backwards (ldd) on this line to forwards (ldi) on next line
 INC E
 INC L
 INC H ; next src line down
 INC D ; next dst line down
MEND

COPY_LINES_0206 MACRO () ; copy 2 linse of 6 bytes in a zigzag from start to end on first line and end to start on second line
 LDI
 LDI
 LDI
 LDI
 LDI
 LDI
 NEXT_LINE_LDI_LDD ()
 LDD
 LDD
 LDD
 LDD
 LDD
 LDD
MEND

COPY_BLOCK_0806 MACRO () ; copy block of 8 by 6 bytes 4 4 blocks in a row
 COPY_LINES_0206 ()
 NEXT_LINE_LDD_LDI ()
 COPY_LINES_0206 ()
 NEXT_LINE_LDD_LDI ()
 COPY_LINES_0206 ()
 NEXT_LINE_LDD_LDI ()
 COPY_LINES_0206 ()
MEND

BCK_TMP_BUF_POS_START MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor
 LD A, E                                                ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH TMP_BUF_TAB_HI      ; #7
 LD D, (HL)                     ; TMP BUF HI #7
 INC H                          ; #4
 ADD A, (HL)                    ; TMP BUF LO #7
 INC H                          ; #4
 LD H, (HL)                     ; BCK BUF HI #7
 LD E, A                        ; TMP BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; BCK BUF LO #4
MEND

BB_TB_6X6_V2 PROC    ; copy from back buffer to temp buffer - L=ver E=hor

 BCK_TMP_BUF_POS_START ()

 COPY_BLOCK_0806 ()

 RET
 ENDP

TMP_SCN_BUF_POS_START MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor

 LD A, E                                                ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH SCN_BUF_TAB_HI      ; #7
 LD D, (HL)                     ; SCN BUF HI #7
 DEC H                          ; #4
 dec h
 ADD A, (HL)                    ; SCN BUF LO #7
 DEC H                          ; #4
 LD H, (HL)                     ; TMP BUF HI #7
 LD E, A                        ; SCN BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; TMP BUF LO #4
MEND

TB_SN_6X6_V2    PROC    ; copy from temp buffer to screen - L=ver E=hor

 TMP_SCN_BUF_POS_START () ; L=ver E=hor

 COPY_BLOCK_0806 ()

 RET
 ENDP

TB_SC_6X6_V2                                    PROC    ; copy from screen buffer to temp buffer
; LD (STK_BUF+1),SP ; store sp

; SCN_TMP_BUF_POS MACRO () ; L=ver E=hor
 LD A, E                                                ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH TMP_BUF_TAB_HI      ; #7
 LD D, (HL)                     ; TMP BUF HI #7
 INC H                          ; #4
 ADD A, (HL)                    ; TMP BUF LO #7
 INC H                          ; #4
 LD H, (HL)                     ; SCN BUF HI #7
 LD E, A                        ; #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; #4
;MEND

 LDI
 LDI
 LDI
 LDI
 LDI
 LDI
 INC H
 INC D

;STK_BUF LD SP,$ABCD
                                               RET
                                               ENDP



SB_TB_5X6_SP LD SP,$ABCD
        RET

ALIGN $100
SCCH2
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))/256

ALIGN $100
SCCL2
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))&255

ALIGN $100
SBCH2
 DEFB (BCK_BUF+(0*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(7*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(7*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(7*32))/256

ALIGN $100
SBCL2
 DEFB (BCK_BUF+(0*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(7*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(7*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(7*32))&255

ALIGN $100
SCN_BUF_TAB_H
 DEFS 8,0
 DEFS 8,1
 DEFS 8,2
 DEFS 8,3
 DEFS 8,4
 DEFS 8,5
 DEFS 8,6
 DEFS 8,7
 DEFS 8,8
 DEFS 8,9
 DEFS 8,10
 DEFS 8,11
 DEFS 8,12
 DEFS 8,13
 DEFS 8,14
 DEFS 8,15
 DEFS 8,16
 DEFS 8,17
 DEFS 8,18
 DEFS 8,19
 DEFS 8,20
 DEFS 8,21
 DEFS 8,22
 DEFS 8,23
 DEFS 8,24
 DEFS 8,25
 DEFS 8,26
 DEFS 8,27
 DEFS 8,28
 DEFS 8,29
 DEFS 8,30
 DEFS 8,31

ALIGN $100
TMP_BUF_TAB_HI
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(0*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(1*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(2*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(3*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(4*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(5*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(6*32)) /256
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(7*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(0*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(1*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(2*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(3*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(4*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(5*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(6*32)) /256
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(7*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(0*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(1*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(2*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(3*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(4*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(5*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(6*32)) /256
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(7*32)) /256

ALIGN $100
BCK_BUF_TAB_LO
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(0*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(1*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(2*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(3*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(4*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(5*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(6*32)) &255
 DEFS 8, (TMP_BUF+(0*2048)+(0*256)+(7*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(0*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(1*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(2*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(3*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(4*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(5*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(6*32)) &255
 DEFS 8, (TMP_BUF+(1*2048)+(0*256)+(7*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(0*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(1*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(2*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(3*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(4*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(5*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(6*32)) &255
 DEFS 8, (TMP_BUF+(2*2048)+(0*256)+(7*32)) &255

ALIGN $100
BCK_BUF_TAB_HI
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(0*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(1*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(2*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(3*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(4*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(5*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(6*32)) /256
 DEFS 8, (BCK_BUF+(0*2048)+(0*256)+(7*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(0*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(1*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(2*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(3*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(4*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(5*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(6*32)) /256
 DEFS 8, (BCK_BUF+(1*2048)+(0*256)+(7*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(0*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(1*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(2*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(3*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(4*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(5*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(6*32)) /256
 DEFS 8, (BCK_BUF+(2*2048)+(0*256)+(7*32)) /256

ALIGN $100
SCN_BUF_TAB_HI
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(0*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(1*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(2*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(3*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(4*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(5*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(6*32)) /256
 DEFS 8, (SCREEN+(0*2048)+(0*256)+(7*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(0*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(1*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(2*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(3*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(4*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(5*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(6*32)) /256
 DEFS 8, (SCREEN+(1*2048)+(0*256)+(7*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(0*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(1*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(2*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(3*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(4*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(5*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(6*32)) /256
 DEFS 8, (SCREEN+(2*2048)+(0*256)+(7*32)) /256

ALIGN $100

TBY
; DEFS 8,4
; DEFS 8,5

 DEFS 8,6
 DEFS 8,7
 DEFS 8,8
 DEFS 8,9
 DEFS 8,10
 DEFS 8,11
 DEFS 8,12
 DEFS 8,13
 DEFS 8,14
 DEFS 8,15
 DEFS 8,16
 DEFS 8,17
 DEFS 8,18
 DEFS 8,19
 DEFS 8,20
 DEFS 8,21
 DEFS 8,22
 DEFS 8,23
 DEFS 8,24
 DEFS 8,25
 DEFS 8,26
 DEFS 8,27
 DEFS 8,28
 DEFS 8,29
 DEFS 8,30
 DEFS 8,31
 DEFS 8,32
 DEFS 8,33
 DEFS 8,34
 DEFS 8,35
 DEFS 8,36
 DEFS 8,37

ALIGN $100

TBX
 DEFS 8,0
 DEFS 8,1
 DEFS 8,2
 DEFS 8,3
 DEFS 8,4
 DEFS 8,5
 DEFS 8,6
 DEFS 8,7
 DEFS 8,8
 DEFS 8,9
 DEFS 8,10
 DEFS 8,11
 DEFS 8,12
 DEFS 8,13
 DEFS 8,14
 DEFS 8,15
 DEFS 8,16
 DEFS 8,17
 DEFS 8,18
 DEFS 8,19
 DEFS 8,20
 DEFS 8,21
 DEFS 8,22
 DEFS 8,23
 DEFS 8,24
 DEFS 8,25
 DEFS 8,26
 DEFS 8,27
 DEFS 8,28
 DEFS 8,29
 DEFS 8,30
 DEFS 8,31

STACK ; workaround as stack was overwriting buffer
 DEFS 4096,0

MEMTOP
 DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.
Zeus_PC EQU START  ; Tell the emulator where to start.
Zeus_SP EQU MEMTOP ; Tell the emulator where to put the stack.


; LD BC,HI BYTE
; LD ED,LO BYTE
;
; LD A,(BC) GET HI BYTE
; EX AF,AF BACKUP HIGH BYTE
; LD A,(DE) GET LO BYTE
; INC C
; INC E
; EXX BACKUP BC DE
; LD L,A LO BYTE
; EX AF,AF
; LD H,A HI BYTE

; ADD A,E
; ADD A,N INSTEAD ?


; l = ver


;SPR_LINE_EVEN MACRO ()
; LD H, HIGH ScrBufEvenL     ; SCREEN V TABLE LO  #7 7
; LD L, C            ; VPOS                       #4 11
; LD A, (HL)         ; LO BYTE POS                #7 18
; DEC H              ;                            #4 22
; LD H, (HL)         ; SCN BUF V HI               #7 29
; ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
; LD L, A            ;                            #4 37
;MEND


; backup sp

; move screen - size setup

; COPY LINE

; LDI ; #16
; LDI ; #16

; NEXT LINE
; ADD HL, SP
; EX HL, DE
; ADD DE, SP
; EX HL, DE

