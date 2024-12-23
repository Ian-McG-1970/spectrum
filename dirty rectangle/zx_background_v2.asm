; This command tells Zeus where to put the code it generates. As a szx file... Alter the path to suit your system

 zeusemulate "48K"
 output_szx "spi.szx",32768,START     ; The szx file

; output_bin "c:\spi.bin",$0000,$10000    ; The binary file ; If for some reason you want binary, uncomment this line
 ORG 32768

SCREEN          EQU 16384

; STACK			EQU 63*1024	; the stack
SCN_BUF	EQU 57*1024 ; the back buffer
TMP_BUF	EQU 49*1024 ; the temp buffer overwritten by the back buffer and sprites

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
 LD b,A
 CALL SB_TB_6X6  ; SB_TB_6X6

 LD A,(py)
 LD L,A
 LD A,(px)
 LD b,A
 CALL TB_SN_6X6 ; TB_SN_6X6

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

CPY_TB_SN_X6 MACRO (TB_ADDR) ; copy from temp buffer to screen
 LD SP,TB_ADDR
 POP AF
 POP BC
 POP DE
 LD SP,HL
 PUSH DE
 PUSH BC
 PUSH AF
MEND

CPY_SB_TB_X6 MACRO (TB_ADDR) ; copy from screen buffer to temp buffer
 LD SP,HL
 POP AF
 POP BC
 POP DE
 LD SP,TB_ADDR
 PUSH DE
 PUSH BC
 PUSH AF
MEND



TB_SN_6X6 ; copy from temp buffer to screen
 LD (TB_SN_6X6_SP+1),SP ; store sp

 LD H, HIGH TBY ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 INC H
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SCCH2 ; SCREEN CHAR HI
 LD D, HIGH SCCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB00)
 INC H
 CPY_TB_SN_X6(TB01)
 INC H
 CPY_TB_SN_X6(TB02)
 INC H
 CPY_TB_SN_X6(TB03)
 INC H
 CPY_TB_SN_X6(TB04)
 INC H
 CPY_TB_SN_X6(TB05)
 INC H
 CPY_TB_SN_X6(TB06)
 INC H
 CPY_TB_SN_X6(TB07)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB10)
 INC H
 CPY_TB_SN_X6(TB11)
 INC H
 CPY_TB_SN_X6(TB12)
 INC H
 CPY_TB_SN_X6(TB13)
 INC H
 CPY_TB_SN_X6(TB14)
 INC H
 CPY_TB_SN_X6(TB15)
 INC H
 CPY_TB_SN_X6(TB16)
 INC H
 CPY_TB_SN_X6(TB17)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB20)
 INC H
 CPY_TB_SN_X6(TB21)
 INC H
 CPY_TB_SN_X6(TB22)
 INC H
 CPY_TB_SN_X6(TB23)
 INC H
 CPY_TB_SN_X6(TB24)
 INC H
 CPY_TB_SN_X6(TB25)
 INC H
 CPY_TB_SN_X6(TB26)
 INC H
 CPY_TB_SN_X6(TB27)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB30)
 INC H
 CPY_TB_SN_X6(TB31)
 INC H
 CPY_TB_SN_X6(TB32)
 INC H
 CPY_TB_SN_X6(TB33)
 INC H
 CPY_TB_SN_X6(TB34)
 INC H
 CPY_TB_SN_X6(TB35)
 INC H
 CPY_TB_SN_X6(TB36)
 INC H
 CPY_TB_SN_X6(TB37)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB40)
 INC H
 CPY_TB_SN_X6(TB41)
 INC H
 CPY_TB_SN_X6(TB42)
 INC H
 CPY_TB_SN_X6(TB43)
 INC H
 CPY_TB_SN_X6(TB44)
 INC H
 CPY_TB_SN_X6(TB45)
 INC H
 CPY_TB_SN_X6(TB46)
 INC H
 CPY_TB_SN_X6(TB47)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB50)
 INC H
 CPY_TB_SN_X6(TB51)
 INC H
 CPY_TB_SN_X6(TB52)
 INC H
 CPY_TB_SN_X6(TB53)
 INC H
 CPY_TB_SN_X6(TB54)
 INC H
 CPY_TB_SN_X6(TB55)
 INC H
 CPY_TB_SN_X6(TB56)
 INC H
 CPY_TB_SN_X6(TB57)

TB_SN_6X6_SP LD SP,$ABCD
RET

TB_SN_5X6 ; copy from temp buffer to screen
 LD (TB_SN_5X6_SP+1),SP ; store sp

 LD H, HIGH TBY ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 INC H
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SCCH2 ; SCREEN CHAR HI
 LD D, HIGH SCCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB00)
 INC H
 CPY_TB_SN_X6(TB01)
 INC H
 CPY_TB_SN_X6(TB02)
 INC H
 CPY_TB_SN_X6(TB03)
 INC H
 CPY_TB_SN_X6(TB04)
 INC H
 CPY_TB_SN_X6(TB05)
 INC H
 CPY_TB_SN_X6(TB06)
 INC H
 CPY_TB_SN_X6(TB07)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB20)
 INC H
 CPY_TB_SN_X6(TB21)
 INC H
 CPY_TB_SN_X6(TB22)
 INC H
 CPY_TB_SN_X6(TB23)
 INC H
 CPY_TB_SN_X6(TB24)
 INC H
 CPY_TB_SN_X6(TB25)
 INC H
 CPY_TB_SN_X6(TB26)
 INC H
 CPY_TB_SN_X6(TB27)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB30)
 INC H
 CPY_TB_SN_X6(TB31)
 INC H
 CPY_TB_SN_X6(TB32)
 INC H
 CPY_TB_SN_X6(TB33)
 INC H
 CPY_TB_SN_X6(TB34)
 INC H
 CPY_TB_SN_X6(TB35)
 INC H
 CPY_TB_SN_X6(TB36)
 INC H
 CPY_TB_SN_X6(TB37)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB40)
 INC H
 CPY_TB_SN_X6(TB41)
 INC H
 CPY_TB_SN_X6(TB42)
 INC H
 CPY_TB_SN_X6(TB43)
 INC H
 CPY_TB_SN_X6(TB44)
 INC H
 CPY_TB_SN_X6(TB45)
 INC H
 CPY_TB_SN_X6(TB46)
 INC H
 CPY_TB_SN_X6(TB47)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB50)
 INC H
 CPY_TB_SN_X6(TB51)
 INC H
 CPY_TB_SN_X6(TB52)
 INC H
 CPY_TB_SN_X6(TB53)
 INC H
 CPY_TB_SN_X6(TB54)
 INC H
 CPY_TB_SN_X6(TB55)
 INC H
 CPY_TB_SN_X6(TB56)
 INC H
 CPY_TB_SN_X6(TB57)

TB_SN_5X6_SP LD SP,$ABCD
RET

TB_SN_4X6 ; copy from temp buffer to screen
 LD (TB_SN_4X6_SP+1),SP ; store sp

 LD H, HIGH TBY ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 INC H
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SCCH2 ; SCREEN CHAR HI
 LD D, HIGH SCCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB00)
 INC H
 CPY_TB_SN_X6(TB01)
 INC H
 CPY_TB_SN_X6(TB02)
 INC H
 CPY_TB_SN_X6(TB03)
 INC H
 CPY_TB_SN_X6(TB04)
 INC H
 CPY_TB_SN_X6(TB05)
 INC H
 CPY_TB_SN_X6(TB06)
 INC H
 CPY_TB_SN_X6(TB07)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB30)
 INC H
 CPY_TB_SN_X6(TB31)
 INC H
 CPY_TB_SN_X6(TB32)
 INC H
 CPY_TB_SN_X6(TB33)
 INC H
 CPY_TB_SN_X6(TB34)
 INC H
 CPY_TB_SN_X6(TB35)
 INC H
 CPY_TB_SN_X6(TB36)
 INC H
 CPY_TB_SN_X6(TB37)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB40)
 INC H
 CPY_TB_SN_X6(TB41)
 INC H
 CPY_TB_SN_X6(TB42)
 INC H
 CPY_TB_SN_X6(TB43)
 INC H
 CPY_TB_SN_X6(TB44)
 INC H
 CPY_TB_SN_X6(TB45)
 INC H
 CPY_TB_SN_X6(TB46)
 INC H
 CPY_TB_SN_X6(TB47)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB50)
 INC H
 CPY_TB_SN_X6(TB51)
 INC H
 CPY_TB_SN_X6(TB52)
 INC H
 CPY_TB_SN_X6(TB53)
 INC H
 CPY_TB_SN_X6(TB54)
 INC H
 CPY_TB_SN_X6(TB55)
 INC H
 CPY_TB_SN_X6(TB56)
 INC H
 CPY_TB_SN_X6(TB57)

TB_SN_4X6_SP LD SP,$ABCD
RET

TB_SN_3X6 ; copy from temp buffer to screen
 LD (TB_SN_3X6_SP+1),SP ; store sp

 LD H, HIGH TBY ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 INC H
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SCCH2 ; SCREEN CHAR HI
 LD D, HIGH SCCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB00)
 INC H
 CPY_TB_SN_X6(TB01)
 INC H
 CPY_TB_SN_X6(TB02)
 INC H
 CPY_TB_SN_X6(TB03)
 INC H
 CPY_TB_SN_X6(TB04)
 INC H
 CPY_TB_SN_X6(TB05)
 INC H
 CPY_TB_SN_X6(TB06)
 INC H
 CPY_TB_SN_X6(TB07)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB10)
 INC H
 CPY_TB_SN_X6(TB11)
 INC H
 CPY_TB_SN_X6(TB12)
 INC H
 CPY_TB_SN_X6(TB13)
 INC H
 CPY_TB_SN_X6(TB14)
 INC H
 CPY_TB_SN_X6(TB15)
 INC H
 CPY_TB_SN_X6(TB16)
 INC H
 CPY_TB_SN_X6(TB17)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB50)
 INC H
 CPY_TB_SN_X6(TB51)
 INC H
 CPY_TB_SN_X6(TB52)
 INC H
 CPY_TB_SN_X6(TB53)
 INC H
 CPY_TB_SN_X6(TB54)
 INC H
 CPY_TB_SN_X6(TB55)
 INC H
 CPY_TB_SN_X6(TB56)
 INC H
 CPY_TB_SN_X6(TB57)

TB_SN_3X6_SP LD SP,$ABCD
RET

TB_SN_2X6 ; copy from temp buffer to screen
 LD (TB_SN_2X6_SP+1),SP ; store sp

 LD H, HIGH TBY ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 INC H
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SCCH2 ; SCREEN CHAR HI
 LD D, HIGH SCCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB00)
 INC H
 CPY_TB_SN_X6(TB01)
 INC H
 CPY_TB_SN_X6(TB02)
 INC H
 CPY_TB_SN_X6(TB03)
 INC H
 CPY_TB_SN_X6(TB04)
 INC H
 CPY_TB_SN_X6(TB05)
 INC H
 CPY_TB_SN_X6(TB06)
 INC H
 CPY_TB_SN_X6(TB07)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_TB_SN_X6(TB50)
 INC H
 CPY_TB_SN_X6(TB51)
 INC H
 CPY_TB_SN_X6(TB52)
 INC H
 CPY_TB_SN_X6(TB53)
 INC H
 CPY_TB_SN_X6(TB54)
 INC H
 CPY_TB_SN_X6(TB55)
 INC H
 CPY_TB_SN_X6(TB56)
 INC H
 CPY_TB_SN_X6(TB57)

TB_SN_2X6_SP LD SP,$ABCD
RET

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









SB_TB_6X6 ; copy from screen buffer to temp buffer
 LD (SB_TB_6X6_SP+1),SP ; store sp

 LD H, HIGH TBX ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SBCH2 ; SCREEN CHAR HI
 LD D, HIGH SBCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB00+6)
 INC H
 CPY_SB_TB_X6(TB01+6)
 INC H
 CPY_SB_TB_X6(TB02+6)
 INC H
 CPY_SB_TB_X6(TB03+6)
 INC H
 CPY_SB_TB_X6(TB04+6)
 INC H
 CPY_SB_TB_X6(TB05+6)
 INC H
 CPY_SB_TB_X6(TB06+6)
 INC H
 CPY_SB_TB_X6(TB07+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB10+6)
 INC H
 CPY_SB_TB_X6(TB11+6)
 INC H
 CPY_SB_TB_X6(TB12+6)
 INC H
 CPY_SB_TB_X6(TB13+6)
 INC H
 CPY_SB_TB_X6(TB14+6)
 INC H
 CPY_SB_TB_X6(TB15+6)
 INC H
 CPY_SB_TB_X6(TB16+6)
 INC H
 CPY_SB_TB_X6(TB17+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB20+6)
 INC H
 CPY_SB_TB_X6(TB21+6)
 INC H
 CPY_SB_TB_X6(TB22+6)
 INC H
 CPY_SB_TB_X6(TB23+6)
 INC H
 CPY_SB_TB_X6(TB24+6)
 INC H
 CPY_SB_TB_X6(TB25+6)
 INC H
 CPY_SB_TB_X6(TB26+6)
 INC H
 CPY_SB_TB_X6(TB27+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB30+6)
 INC H
 CPY_SB_TB_X6(TB31+6)
 INC H
 CPY_SB_TB_X6(TB32+6)
 INC H
 CPY_SB_TB_X6(TB33+6)
 INC H
 CPY_SB_TB_X6(TB34+6)
 INC H
 CPY_SB_TB_X6(TB35+6)
 INC H
 CPY_SB_TB_X6(TB36+6)
 INC H
 CPY_SB_TB_X6(TB37+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB40+6)
 INC H
 CPY_SB_TB_X6(TB41+6)
 INC H
 CPY_SB_TB_X6(TB42+6)
 INC H
 CPY_SB_TB_X6(TB43+6)
 INC H
 CPY_SB_TB_X6(TB44+6)
 INC H
 CPY_SB_TB_X6(TB45+6)
 INC H
 CPY_SB_TB_X6(TB46+6)
 INC H
 CPY_SB_TB_X6(TB47+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB50+6)
 INC H
 CPY_SB_TB_X6(TB51+6)
 INC H
 CPY_SB_TB_X6(TB52+6)
 INC H
 CPY_SB_TB_X6(TB53+6)
 INC H
 CPY_SB_TB_X6(TB54+6)
 INC H
 CPY_SB_TB_X6(TB55+6)
 INC H
 CPY_SB_TB_X6(TB56+6)
 INC H
 CPY_SB_TB_X6(TB57+6)

SB_TB_6X6_SP LD SP,$ABCD
RET

SB_TB_5X6 ; copy from screen buffer to temp buffer
 LD (SB_TB_5X6_SP+1),SP ; store sp

 LD H, HIGH TBX ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SBCH2 ; SCREEN CHAR HI
 LD D, HIGH SBCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB00+6)
 INC H
 CPY_SB_TB_X6(TB01+6)
 INC H
 CPY_SB_TB_X6(TB02+6)
 INC H
 CPY_SB_TB_X6(TB03+6)
 INC H
 CPY_SB_TB_X6(TB04+6)
 INC H
 CPY_SB_TB_X6(TB05+6)
 INC H
 CPY_SB_TB_X6(TB06+6)
 INC H
 CPY_SB_TB_X6(TB07+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB10+6)
 INC H
 CPY_SB_TB_X6(TB11+6)
 INC H
 CPY_SB_TB_X6(TB12+6)
 INC H
 CPY_SB_TB_X6(TB13+6)
 INC H
 CPY_SB_TB_X6(TB14+6)
 INC H
 CPY_SB_TB_X6(TB15+6)
 INC H
 CPY_SB_TB_X6(TB16+6)
 INC H
 CPY_SB_TB_X6(TB17+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB30+6)
 INC H
 CPY_SB_TB_X6(TB31+6)
 INC H
 CPY_SB_TB_X6(TB32+6)
 INC H
 CPY_SB_TB_X6(TB33+6)
 INC H
 CPY_SB_TB_X6(TB34+6)
 INC H
 CPY_SB_TB_X6(TB35+6)
 INC H
 CPY_SB_TB_X6(TB36+6)
 INC H
 CPY_SB_TB_X6(TB37+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB40+6)
 INC H
 CPY_SB_TB_X6(TB41+6)
 INC H
 CPY_SB_TB_X6(TB42+6)
 INC H
 CPY_SB_TB_X6(TB43+6)
 INC H
 CPY_SB_TB_X6(TB44+6)
 INC H
 CPY_SB_TB_X6(TB45+6)
 INC H
 CPY_SB_TB_X6(TB46+6)
 INC H
 CPY_SB_TB_X6(TB47+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB50+6)
 INC H
 CPY_SB_TB_X6(TB51+6)
 INC H
 CPY_SB_TB_X6(TB52+6)
 INC H
 CPY_SB_TB_X6(TB53+6)
 INC H
 CPY_SB_TB_X6(TB54+6)
 INC H
 CPY_SB_TB_X6(TB55+6)
 INC H
 CPY_SB_TB_X6(TB56+6)
 INC H
 CPY_SB_TB_X6(TB57+6)

SB_TB_5X6_SP LD SP,$ABCD
RET


SB_TB_4X6 ; copy from screen buffer to temp buffer
 LD (SB_TB_4X6_SP+1),SP ; store sp

 LD H, HIGH TBX ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SBCH2 ; SCREEN CHAR HI
 LD D, HIGH SBCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB00+6)
 INC H
 CPY_SB_TB_X6(TB01+6)
 INC H
 CPY_SB_TB_X6(TB02+6)
 INC H
 CPY_SB_TB_X6(TB03+6)
 INC H
 CPY_SB_TB_X6(TB04+6)
 INC H
 CPY_SB_TB_X6(TB05+6)
 INC H
 CPY_SB_TB_X6(TB06+6)
 INC H
 CPY_SB_TB_X6(TB07+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB30+6)
 INC H
 CPY_SB_TB_X6(TB31+6)
 INC H
 CPY_SB_TB_X6(TB32+6)
 INC H
 CPY_SB_TB_X6(TB33+6)
 INC H
 CPY_SB_TB_X6(TB34+6)
 INC H
 CPY_SB_TB_X6(TB35+6)
 INC H
 CPY_SB_TB_X6(TB36+6)
 INC H
 CPY_SB_TB_X6(TB37+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB40+6)
 INC H
 CPY_SB_TB_X6(TB41+6)
 INC H
 CPY_SB_TB_X6(TB42+6)
 INC H
 CPY_SB_TB_X6(TB43+6)
 INC H
 CPY_SB_TB_X6(TB44+6)
 INC H
 CPY_SB_TB_X6(TB45+6)
 INC H
 CPY_SB_TB_X6(TB46+6)
 INC H
 CPY_SB_TB_X6(TB47+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB50+6)
 INC H
 CPY_SB_TB_X6(TB51+6)
 INC H
 CPY_SB_TB_X6(TB52+6)
 INC H
 CPY_SB_TB_X6(TB53+6)
 INC H
 CPY_SB_TB_X6(TB54+6)
 INC H
 CPY_SB_TB_X6(TB55+6)
 INC H
 CPY_SB_TB_X6(TB56+6)
 INC H
 CPY_SB_TB_X6(TB57+6)

SB_TB_4X6_SP LD SP,$ABCD
RET


SB_TB_3X6 ; copy from screen buffer to temp buffer
 LD (SB_TB_3X6_SP+1),SP ; store sp

 LD H, HIGH TBX ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SBCH2 ; SCREEN CHAR HI
 LD D, HIGH SBCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB00+6)
 INC H
 CPY_SB_TB_X6(TB01+6)
 INC H
 CPY_SB_TB_X6(TB02+6)
 INC H
 CPY_SB_TB_X6(TB03+6)
 INC H
 CPY_SB_TB_X6(TB04+6)
 INC H
 CPY_SB_TB_X6(TB05+6)
 INC H
 CPY_SB_TB_X6(TB06+6)
 INC H
 CPY_SB_TB_X6(TB07+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB40+6)
 INC H
 CPY_SB_TB_X6(TB41+6)
 INC H
 CPY_SB_TB_X6(TB42+6)
 INC H
 CPY_SB_TB_X6(TB43+6)
 INC H
 CPY_SB_TB_X6(TB44+6)
 INC H
 CPY_SB_TB_X6(TB45+6)
 INC H
 CPY_SB_TB_X6(TB46+6)
 INC H
 CPY_SB_TB_X6(TB47+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB50+6)
 INC H
 CPY_SB_TB_X6(TB51+6)
 INC H
 CPY_SB_TB_X6(TB52+6)
 INC H
 CPY_SB_TB_X6(TB53+6)
 INC H
 CPY_SB_TB_X6(TB54+6)
 INC H
 CPY_SB_TB_X6(TB55+6)
 INC H
 CPY_SB_TB_X6(TB56+6)
 INC H
 CPY_SB_TB_X6(TB57+6)

SB_TB_3X6_SP LD SP,$ABCD
RET


SB_TB_2X6 ; copy from screen buffer to temp buffer
 LD (SB_TB_2X6_SP+1),SP ; store sp

 LD H, HIGH TBX ; HOR BYTES / L=HOR
 LD A,(HL)      ; HPOS
 LD I,A         ; STORE HOR CHAR
 LD L,B         ; VPOS

 LD C,(HL)      ; LO BYTE POS
 LD E,C         ; LO BYTE POS

 LD B, HIGH SBCH2 ; SCREEN CHAR HI
 LD D, HIGH SBCL2 ; SCREEN CHAR LO

 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
 EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB00+6)
 INC H
 CPY_SB_TB_X6(TB01+6)
 INC H
 CPY_SB_TB_X6(TB02+6)
 INC H
 CPY_SB_TB_X6(TB03+6)
 INC H
 CPY_SB_TB_X6(TB04+6)
 INC H
 CPY_SB_TB_X6(TB05+6)
 INC H
 CPY_SB_TB_X6(TB06+6)
 INC H
 CPY_SB_TB_X6(TB07+6)

 EXX       ; T4 ; RESTORE BC DE
 LD A,(BC) ; T7 ; GET HI BYTE
 EX AF,AF' ; T4 ; BACKUP HIGH BYTE
 LD A,(DE) ; T7 ; GET LO BYTE
 INC C     ; T4
 INC E     ; T4
; EXX       ; T4 ; BACKUP BC DE
 LD L,A    ; T4
 LD A,I    ; T9 ; HOR
 ADD A,L   ; T4
 LD L,A    ; T4 ; LO BYTE
 EX AF,AF' ; T4
 LD H,A    ; T4 ; HI BYTE

 CPY_SB_TB_X6(TB50+6)
 INC H
 CPY_SB_TB_X6(TB51+6)
 INC H
 CPY_SB_TB_X6(TB52+6)
 INC H
 CPY_SB_TB_X6(TB53+6)
 INC H
 CPY_SB_TB_X6(TB54+6)
 INC H
 CPY_SB_TB_X6(TB55+6)
 INC H
 CPY_SB_TB_X6(TB56+6)
 INC H
 CPY_SB_TB_X6(TB57+6)

SB_TB_2X6_SP LD SP,$ABCD
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
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(7*32))/256

ALIGN $100

SBCL2
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(0*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(0*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(0*256)+(7*32))&255

ALIGN $100
SB_TAB_HI
 DEFB SCN_BUF+(32*000) &255, SCN_BUF+(32*001) &255, SCN_BUF+(32*002) &255, SCN_BUF+(32*003) &255, SCN_BUF+(32*004) &255, SCN_BUF+(32*005) &255, SCN_BUF+(32*006) &255, SCN_BUF+(32*007) &255, SCN_BUF+(32*008) &255, SCN_BUF+(32*009) &255
 DEFB SCN_BUF+(32*010) &255, SCN_BUF+(32*011) &255, SCN_BUF+(32*012) &255, SCN_BUF+(32*013) &255, SCN_BUF+(32*014) &255, SCN_BUF+(32*015) &255, SCN_BUF+(32*016) &255, SCN_BUF+(32*017) &255, SCN_BUF+(32*018) &255, SCN_BUF+(32*019) &255
 DEFB SCN_BUF+(32*020) &255, SCN_BUF+(32*021) &255, SCN_BUF+(32*022) &255, SCN_BUF+(32*023) &255, SCN_BUF+(32*024) &255, SCN_BUF+(32*025) &255, SCN_BUF+(32*026) &255, SCN_BUF+(32*027) &255, SCN_BUF+(32*028) &255, SCN_BUF+(32*029) &255
 DEFB SCN_BUF+(32*030) &255, SCN_BUF+(32*031) &255, SCN_BUF+(32*032) &255, SCN_BUF+(32*033) &255, SCN_BUF+(32*034) &255, SCN_BUF+(32*035) &255, SCN_BUF+(32*036) &255, SCN_BUF+(32*037) &255, SCN_BUF+(32*038) &255, SCN_BUF+(32*039) &255
 DEFB SCN_BUF+(32*040) &255, SCN_BUF+(32*041) &255, SCN_BUF+(32*042) &255, SCN_BUF+(32*043) &255, SCN_BUF+(32*044) &255, SCN_BUF+(32*045) &255, SCN_BUF+(32*046) &255, SCN_BUF+(32*047) &255, SCN_BUF+(32*048) &255, SCN_BUF+(32*049) &255
 DEFB SCN_BUF+(32*050) &255, SCN_BUF+(32*051) &255, SCN_BUF+(32*052) &255, SCN_BUF+(32*053) &255, SCN_BUF+(32*054) &255, SCN_BUF+(32*055) &255, SCN_BUF+(32*056) &255, SCN_BUF+(32*057) &255, SCN_BUF+(32*058) &255, SCN_BUF+(32*059) &255
 DEFB SCN_BUF+(32*060) &255, SCN_BUF+(32*061) &255, SCN_BUF+(32*062) &255, SCN_BUF+(32*063) &255, SCN_BUF+(32*064) &255, SCN_BUF+(32*065) &255, SCN_BUF+(32*066) &255, SCN_BUF+(32*067) &255, SCN_BUF+(32*068) &255, SCN_BUF+(32*069) &255
 DEFB SCN_BUF+(32*070) &255, SCN_BUF+(32*071) &255, SCN_BUF+(32*072) &255, SCN_BUF+(32*073) &255, SCN_BUF+(32*074) &255, SCN_BUF+(32*075) &255, SCN_BUF+(32*076) &255, SCN_BUF+(32*077) &255, SCN_BUF+(32*078) &255, SCN_BUF+(32*079) &255
 DEFB SCN_BUF+(32*080) &255, SCN_BUF+(32*081) &255, SCN_BUF+(32*082) &255, SCN_BUF+(32*083) &255, SCN_BUF+(32*084) &255, SCN_BUF+(32*085) &255, SCN_BUF+(32*086) &255, SCN_BUF+(32*087) &255, SCN_BUF+(32*088) &255, SCN_BUF+(32*089) &255
 DEFB SCN_BUF+(32*090) &255, SCN_BUF+(32*091) &255, SCN_BUF+(32*092) &255, SCN_BUF+(32*093) &255, SCN_BUF+(32*094) &255, SCN_BUF+(32*095) &255, SCN_BUF+(32*996) &255, SCN_BUF+(32*097) &255, SCN_BUF+(32*088) &255, SCN_BUF+(32*099) &255
 DEFB SCN_BUF+(32*100) &255, SCN_BUF+(32*101) &255, SCN_BUF+(32*102) &255, SCN_BUF+(32*103) &255, SCN_BUF+(32*104) &255, SCN_BUF+(32*105) &255, SCN_BUF+(32*106) &255, SCN_BUF+(32*107) &255, SCN_BUF+(32*108) &255, SCN_BUF+(32*109) &255
 DEFB SCN_BUF+(32*110) &255, SCN_BUF+(32*111) &255, SCN_BUF+(32*112) &255, SCN_BUF+(32*013) &255, SCN_BUF+(32*114) &255, SCN_BUF+(32*115) &255, SCN_BUF+(32*116) &255, SCN_BUF+(32*117) &255, SCN_BUF+(32*118) &255, SCN_BUF+(32*119) &255
 DEFB SCN_BUF+(32*120) &255, SCN_BUF+(32*121) &255, SCN_BUF+(32*122) &255, SCN_BUF+(32*023) &255, SCN_BUF+(32*124) &255, SCN_BUF+(32*125) &255, SCN_BUF+(32*126) &255, SCN_BUF+(32*127) &255, SCN_BUF+(32*128) &255, SCN_BUF+(32*129) &255
 DEFB SCN_BUF+(32*130) &255, SCN_BUF+(32*131) &255, SCN_BUF+(32*132) &255, SCN_BUF+(32*033) &255, SCN_BUF+(32*134) &255, SCN_BUF+(32*135) &255, SCN_BUF+(32*136) &255, SCN_BUF+(32*137) &255, SCN_BUF+(32*138) &255, SCN_BUF+(32*139) &255
 DEFB SCN_BUF+(32*140) &255, SCN_BUF+(32*141) &255, SCN_BUF+(32*142) &255, SCN_BUF+(32*043) &255, SCN_BUF+(32*144) &255, SCN_BUF+(32*145) &255, SCN_BUF+(32*146) &255, SCN_BUF+(32*147) &255, SCN_BUF+(32*148) &255, SCN_BUF+(32*149) &255
 DEFB SCN_BUF+(32*150) &255, SCN_BUF+(32*151) &255, SCN_BUF+(32*152) &255, SCN_BUF+(32*053) &255, SCN_BUF+(32*154) &255, SCN_BUF+(32*155) &255, SCN_BUF+(32*156) &255, SCN_BUF+(32*157) &255, SCN_BUF+(32*158) &255, SCN_BUF+(32*159) &255
 DEFB SCN_BUF+(32*160) &255, SCN_BUF+(32*161) &255, SCN_BUF+(32*162) &255, SCN_BUF+(32*063) &255, SCN_BUF+(32*164) &255, SCN_BUF+(32*165) &255, SCN_BUF+(32*166) &255, SCN_BUF+(32*167) &255, SCN_BUF+(32*168) &255, SCN_BUF+(32*169) &255
 DEFB SCN_BUF+(32*170) &255, SCN_BUF+(32*171) &255, SCN_BUF+(32*172) &255, SCN_BUF+(32*073) &255, SCN_BUF+(32*174) &255, SCN_BUF+(32*175) &255, SCN_BUF+(32*176) &255, SCN_BUF+(32*177) &255, SCN_BUF+(32*178) &255, SCN_BUF+(32*179) &255
 DEFB SCN_BUF+(32*180) &255, SCN_BUF+(32*181) &255, SCN_BUF+(32*182) &255, SCN_BUF+(32*083) &255, SCN_BUF+(32*184) &255, SCN_BUF+(32*185) &255, SCN_BUF+(32*186) &255, SCN_BUF+(32*187) &255, SCN_BUF+(32*188) &255, SCN_BUF+(32*189) &255
 DEFB SCN_BUF+(32*190) &255, SCN_BUF+(32*191) &255
 
ALIGN $100
SB_TAB_LO ; todo

ALIGN $100
TB_TABL_HI ; todo

ALIGN $100
TB_TAB_LO ; todo - same as SB_TAB_LO ?

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

ALIGN $100

SCRBUFFER
 DEFS SCREEN_SIZE+32, %01010101

ALIGN $100

TMPBUFFER ; 6*8 across by 6 down
TB00 DEFS 6,%11111111
TB01 DEFS 6,%10000001
TB02 DEFS 6,%10000001
TB03 DEFS 6,%10000001
TB04 DEFS 6,%10000001
TB05 DEFS 6,%10000001
TB06 DEFS 6,%10000001
TB07 DEFS 6,%11111111

TB10 DEFS 6,%01010101
TB11 DEFS 6,%10101010
TB12 DEFS 6,%01010101
TB13 DEFS 6,%10101010
TB14 DEFS 6,%01010101
TB15 DEFS 6,%10101010
TB16 DEFS 6,%01010101
TB17 DEFS 6,%10101010

TB20 DEFS 6,%00000000
TB21 DEFS 6,%01111110
TB22 DEFS 6,%01000010
TB23 DEFS 6,%01000010
TB24 DEFS 6,%01000010
TB25 DEFS 6,%01000010
TB26 DEFS 6,%01111110
TB27 DEFS 6,%00000000

TB30 DEFS 6,%00110011
TB31 DEFS 6,%00110011
TB32 DEFS 6,%11001100
TB33 DEFS 6,%11001100
TB34 DEFS 6,%00110011
TB35 DEFS 6,%00110011
TB36 DEFS 6,%11001100
TB37 DEFS 6,%11001100

TB40 DEFS 6,%11110000
TB41 DEFS 6,%11110000
TB42 DEFS 6,%11110000
TB43 DEFS 6,%11110000
TB44 DEFS 6,%00001111
TB45 DEFS 6,%00001111
TB46 DEFS 6,%00001111
TB47 DEFS 6,%00001111

TB50 DEFS 6,%11111111
TB51 DEFS 6,%10000001
TB52 DEFS 6,%10111101
TB53 DEFS 6,%10100101
TB54 DEFS 6,%10100101
TB55 DEFS 6,%10111101
TB56 DEFS 6,%10000001
TB57 DEFS 6,%11111111

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