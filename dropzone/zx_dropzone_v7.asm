; This command tells Zeus where to put the code it generates. As a szx file... Alter the path to suit your system

 zeusemulate "48K"
 output_szx "spi.szx",32768,START     ; The szx file

; output_bin "c:\spi.bin",$0000,$10000    ; The binary file ; If for some reason you want binary, uncomment this line
 ORG 32768

BORDER MACRO (COLOUR) ; copy from temp buffer to screen
 LD A,COLOUR         ; bottom three bits of A contain the border color
 OUT (254),A
MEND

START
 DI                      ; interrupts off
 LD SP,MEMTOP            ; set stack to end STACK?

 ld bc, (32*24*8)-1
 ld hl, SCREEN
 ld a, 0
 call MEMSET

 ld bc, (32*24)-1
 ld hl, ATTRIB
 ld a, 7
 call MEMSET

 CALL MAP_SETUP

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

MAIN_LOOP
 CALL V_BLANK

 BORDER (2)
 CALL DRAW_LANDSCAPE

 BORDER (4)
 CALL MOVE_POINTS

 BORDER (7)
 CALL KEYBOARD

; BORDER (1)
; LD IX,(temp_byte_lo)
; LD A,3
; CALL HEX16

; LD A,(temp_byte_lo)
; LD IY,$0006
; CALL HEX8

; LD A,(temp_byte_hi)
; LD IY,$0007
; CALL HEX8

; LD A,(temp0)
; LD IY,$0008
; CALL HEX8

 BORDER (3)
JP MAIN_LOOP

POP_MAP_LINE_LEFT MACRO ()
 POP AF
 POP BC
 POP DE
 POP HL
 EXX
 EX AF,AF'
 POP AF
 POP BC
 POP DE
 POP HL
MEND

PUSH_SCN_LINE_LEFT MACRO ()
 PUSH HL
 PUSH DE
 PUSH BC
 PUSH AF
 EXX
 EX AF,AF'
 PUSH HL
 PUSH DE
 PUSH BC
 PUSH AF
MEND

POP_MAP_LINE_RIGHT MACRO ()
 POP AF
 POP BC
 POP DE
 POP HL
 EXX
 POP BC
 POP DE
 POP HL
MEND

PUSH_SCN_LINE_RIGHT MACRO ()
 PUSH HL
 PUSH DE
 PUSH BC
 EXX
 PUSH HL
 PUSH DE
 PUSH BC
 PUSH AF
MEND

TEMP_WORD       DEFW 0

ALIGN $100
LANDSCAPE_SHIFT_LO DEFB MAP_LINE_0_7 &255, MAP_LINE_0_6 &255, MAP_LINE_0_5 &255, MAP_LINE_0_4 &255, MAP_LINE_0_3 &255, MAP_LINE_0_2 &255, MAP_LINE_0_1 &255, MAP_LINE_0_0 &255
ALIGN $100
LANDSCAPE_SHIFT_HI DEFB MAP_LINE_0_7 /256, MAP_LINE_0_6 /256, MAP_LINE_0_5 /256, MAP_LINE_0_4 /256, MAP_LINE_0_3 /256, MAP_LINE_0_2 /256, MAP_LINE_0_1 /256, MAP_LINE_0_0 /256

MAP_SETUP_NEXT_LINE     PROC
                        LD HL, (TEMP_WORD)              ; prev shifted line addr

                        LD IX, (TEMP_WORD)
                        LD DE, LANDSCAPE_SIZE
                        ADD IX, DE              ; inc map line by landscape size
                        LD (TEMP_WORD), IX              ; now curr shifted line addr

                        LD DE, (TEMP_WORD)              ; curr shifted line addr
                        CALL COPY_MAP_LEFT              ; copy from prev (HL) to curr (DE)
                        LD HL, (TEMP_WORD)              ; curr shifted line
                        CALL ROTATE_MAP                 ; shift to new pos
                        LD HL, (TEMP_WORD)              ; curr shifted line
                        CALL COPY_MAP_RIGHT             ; copy from HL to right edge
                        RET
                        ENDP

MAP_SETUP_LINE  PROC
                LD (TEMP_WORD), DE                              ; backup dst
                CALL COPY_MAP_LEFT                              ; #0
                LD HL, (TEMP_WORD)
                CALL COPY_MAP_RIGHT                             ; copy from HL to right edge

                CALL MAP_SETUP_NEXT_LINE ; #1
                CALL MAP_SETUP_NEXT_LINE ; #2
                CALL MAP_SETUP_NEXT_LINE ; #3
                CALL MAP_SETUP_NEXT_LINE ; #4
                CALL MAP_SETUP_NEXT_LINE ; #5
                CALL MAP_SETUP_NEXT_LINE ; #6
                CALL MAP_SETUP_NEXT_LINE ; #7

                RET
                ENDP

MAP_SETUP       PROC
        LD HL, 0 +(128*0)
        LD DE, MAP_LINE_0_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*1)
        LD DE, MAP_LINE_1_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*2)
        LD DE, MAP_LINE_2_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*3)
        LD DE, MAP_LINE_3_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*4)
        LD DE, MAP_LINE_4_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*5)
        LD DE, MAP_LINE_5_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*6)
        LD DE, MAP_LINE_6_0
        CALL MAP_SETUP_LINE

        LD HL, 0 +(128*7)
        LD DE, MAP_LINE_7_0
        CALL MAP_SETUP_LINE
                RET
                ENDP

COPY_MAP_LEFT   PROC
                LD BC, 128
                LDIR
                RET
                ENDP

COPY_MAP_RIGHT  PROC
                LD D, H
                LD E, L
                LD BC, 128
                ADD HL, BC
                PUSH HL
                PUSH DE
                POP HL
                POP DE
                LD BC, 32
                LDIR
                RET
                ENDP

ROTATE_MAP      PROC
                                XOR A ; CLC
                                PUSH HL ; BACKUP START
                                LD A,(HL) ; BACKUP
                                LD B, 128 ; LOOP 128 TIMES
ROT_LOOP                        RR (HL) ; ROTATE RIGHT PUTTING CARRY IN MSB AND LSB IN CARRY
                                        INC HL ; NEXT BYTE
                                        DJNZ ROT_LOOP
                                POP HL ; RESTORE START
                                RR A ; ROTATE ACC RIGHT PUTTING CARRY IN MSB AND LSB IN CARRY
                                LD (HL), A ; STORE ACC AT START
                RET
                ENDP

; 00 00 00 00 00 11 11 11 11 11 22 22 22 22 22 33
; 01 23 45 67 89 01 23 45 67 89 01 23 45 67 89 01
temp_byte_lo defb 0
temp_byte_hi defb 0

temp1 defb 0
temp2 defb 0
temp3 defb 0
temp4 defb 0
temp5 defb 0
temp6 defb 0
temp7 defb 0
temp8 defb 0
temp9 defb 0
temp0 defb 0

 ; L=Ver
; IXL=Hor
; C=Val

HEXCHAR
 LD A,C
 ADD A,A ; *2
 ADD A,A ; *4
 ADD A,C ; *5
 LD C,A

 LD H, HIGH ScrBufH     ; SCREEN V TABLE
 LD B, HIGH HexChar

 LD D,(HL)              ; L=VPOS
 INC H    ; ScrBufL
 LD A,IYH
 ADD A,(HL)              ; DE = SCREEN POS
 LD E,A
 LD A,(BC)
 LD (DE),A
 INC C

 DEC H ; ScrBufH
 INC L
 LD D,(HL)
 INC H
 LD A,IYH
 ADD A,(HL)
 LD E,A
 LD A,(BC)
 LD (DE),A
 INC C

 DEC H
 INC L
 LD D,(HL)
 INC H
 LD A,IYH
 ADD A,(HL)
 LD E,A
 LD A,(BC)
 LD (DE),A
 INC C

 DEC H
 INC L
 LD D,(HL)
 INC H
 LD A,IYH
 ADD A,(HL)
 LD E,A
 LD A,(BC)
 LD (DE),A
 INC C

 DEC H
 INC L
 LD D,(HL)
 INC H
 LD A,IYH
 ADD A,(HL)
 LD E,A
 LD A,(BC)
 LD (DE),A

 RET

; IYL=Ver
; IYH=Hor
; A=Val

HEX8
 LD C,A
 EX AF,AF'

 LD A,IYL
 ADD A,A        ; *2
 ADD A,IYL      ; *3
 ADD A,A        ; *6
 LD IYL,A
 LD L,A

 LD A,C
 SRL A
 SRL A
 SRL A
 SRL A
 LD C,A

 CALL HEXCHAR

 INC IYH

 LD A,IYL
 LD L,A

 EX AF,AF'
 AND A,$0F
 LD C,A
 JP HEXCHAR    ; ret

; IX=Val
; A=Ver

HEX16
 LD I,A ; PUSH AF
 LD IYL,A
 LD IYH,0
 LD A,IXH
 CALL HEX8

 LD A,I ; POP AF
 LD IYL,A
 LD IYH,2
 LD A,IXL
 JP HEX8       ; ret

DRAW_LANDSCAPE  PROC

 ld a, (temp_byte_lo)
 add a, 1
 ld (temp_byte_lo), a
 ld a, (temp_byte_hi)
 adc a, 0
 ld (temp_byte_hi), a

 ld a, (temp_byte_hi) ; get hi
 ld h,a
 ld a, (temp_byte_lo) ; get lo byte
  SRL H
  RRA
  SRL H
  RRA
  SRL H
  RRA   ; divide by 8
 and 127 ; only 128 bytes of map avail
  LD e, A ; char pos

 ld a, (temp_byte_lo)
 and 7 ; a=pixel pos
 ld h, high LANDSCAPE_SHIFT_LO
 ld l, a

 ld a, e ; char pos

 ld (temp0),a ; temp char pos

 add a,(hl) ; map lo + char pos
 inc h
 ld h,(hl)
 jp nc, cont
  inc h
cont:
 ld l, a   ; hl = scroll shift map address

 ld (DRAW_LANDSCAPE2.SMC_LEFT_0 +1),hl
 ld de,16
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_0 +1),hl

 ld bc,1280-16
 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_1 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_1 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_2 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_2 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_3 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_3 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_4 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_4 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_5 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_5 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_6 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_6 +1),hl

 add hl,bc
 ld (DRAW_LANDSCAPE2.SMC_LEFT_7 +1),hl
 add hl,de
 ld (DRAW_LANDSCAPE2.SMC_RIGHT_7 +1),hl

 endp

; d4a0
; d9a0

DRAW_LANDSCAPE2  PROC
                LD (STACK_BK+1), SP

SMC_LEFT_0      LD SP, MAP_LINE_0_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_0 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_0     LD SP, MAP_LINE_0_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_0 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_1      LD SP, MAP_LINE_1_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_1 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_1     LD SP, MAP_LINE_1_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_1 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_2      LD SP, MAP_LINE_2_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_2 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_2     LD SP, MAP_LINE_2_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_2 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_3      LD SP, MAP_LINE_3_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_3 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_3     LD SP, MAP_LINE_3_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_3 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_4      LD SP, MAP_LINE_4_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_4 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_4     LD SP, MAP_LINE_4_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_4 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_5      LD SP, MAP_LINE_5_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_5 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_5     LD SP, MAP_LINE_5_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_5 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_6      LD SP, MAP_LINE_6_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_6 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_6     LD SP, MAP_LINE_6_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_6 +30
                PUSH_SCN_LINE_RIGHT ()

SMC_LEFT_7      LD SP, MAP_LINE_7_0
                POP_MAP_LINE_LEFT ()
                LD SP, SCN_LINE_7 +16
                PUSH_SCN_LINE_LEFT ()
SMC_RIGHT_7     LD SP, MAP_LINE_7_0 +16
                POP_MAP_LINE_RIGHT ()
                LD SP, SCN_LINE_7 +30
                PUSH_SCN_LINE_RIGHT ()

STACK_BK                LD SP, $ABCD

                                RET
                                ENDP

sttx defb 12
stty defb 20
endx defb 95
endy defb 17
sdx defb 1
sdy defb 0 ;1
edx defb 0 ;1
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

MEMSET  PROC
        ld (hl), a
        ld d, h
        ld e, l
;        push hl
;        pop de
; ld de, hl
        inc de
        ldir
        RET
        ENDP

PAUSE   PROC
LP      LD      A, 6
        OUT     (254),A
        LD      A, 7
        OUT     (254),A
        DJNZ    LP
        RET
        ENDP

KEYBOARD PROC
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
 AND $01
 JP NZ,P_KEY_N
  LD A, (px)
  INC A
  LD (px), A
P_KEY_N
  LD BC,$FEFE
 IN A,(C)
 AND $01
 JP NZ,Z_KEY_N
  LD A, (py)
  DEC A
  LD (py), A
Z_KEY_N
 LD BC,$7FFE
 IN A,(C)
 AND $01
 JP NZ,M_KEY_N
  LD A, (py)
  INC A
  LD (py), A
M_KEY_N
 RET
 ENDP

V_BLANK: PROC
         LD DE,$1140       ; attr into D, MSB of port addr into E
FB_LP           INC HL          ; padding instruction
                LD A,E          ; MSB of port addr into A
                IN A,($ff)      ; read port 0x40FF into A
                CP D            ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
                JP NZ,FB_LP     ; no? keep trying
         RET
         ENDP

MOVE_POINT          PROC
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

                    RET
                    ENDP

        LD      A,(sttx)
        LD      D,A ;x
        LD a, (stty)
        LD      E,A ;y
        LD      A,(sdx)
        LD      C,A
        LD      A,(sdy)
        LD      B,C
        CALL    MOVE_POINT
        LD      A,D ;x
        LD      (sttx),A
        LD      A,E ;y
        LD  (stty),a
       LD      A,C ;y
        LD      (sdx),A
        LD      A,B
        LD      (sdy),A


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

Hex0    DB 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1    DB 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2    DB 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3    DB 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4    DB 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

ALIGN $100

ScrBufH DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(0*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(1*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(2*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(3*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(4*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(5*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(6*32))/256

 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(2*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(3*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(4*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(5*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(6*256)+(7*32))/256
 DEFB (SCREEN+(0*2048)+(7*256)+(7*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(0*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(1*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(2*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(3*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(4*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(5*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(6*32))/256

 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(2*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(3*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(4*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(5*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(6*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(7*256)+(7*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(0*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(1*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(2*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(3*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(4*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(5*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(6*32))/256

 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(2*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(3*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(4*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(5*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(6*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(7*256)+(7*32))/256

ALIGN $100

ScrBufL DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(0*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(1*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(2*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(3*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(4*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(5*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(6*32))&255

 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(2*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(3*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(4*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(5*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(6*256)+(7*32))&255
 DEFB (SCREEN+(0*2048)+(7*256)+(7*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(0*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(1*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(2*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(3*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(4*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(5*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(6*32))&255

 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(2*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(3*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(4*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(5*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(6*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(7*256)+(7*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(0*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(1*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(2*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(3*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(4*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(5*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(6*32))&255

 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(2*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(3*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(4*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(5*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(6*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(7*256)+(7*32))&255

ALIGN $100

ScrBufY DEFS 8,0
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
HexChar:
DEFB 00111110b
DEFB 01100011b
DEFB 01100011b
DEFB 01100011b
DEFB 00111110b

DEFB 00001100b
DEFB 00111100b
DEFB 00001100b
DEFB 00001100b
DEFB 01111111b

DEFB 01111110b
DEFB 00000011b
DEFB 00111110b
DEFB 01100000b
DEFB 01111111b

DEFB 01111110b
DEFB 00000011b
DEFB 01111110b
DEFB 00000011b
DEFB 01111110b

DEFB 01100011b
DEFB 01100011b
DEFB 01111111b
DEFB 00000011b
DEFB 00000011b

DEFB 01111111b
DEFB 01100000b
DEFB 01111110b
DEFB 00000011b
DEFB 01111110b

DEFB 00111111b
DEFB 01100000b
DEFB 01111110b
DEFB 01100011b
DEFB 00111110b

DEFB 01111111b
DEFB 00000011b
DEFB 00000011b
DEFB 00000011b
DEFB 00000011b

DEFB 00111110b
DEFB 01100011b
DEFB 00111110b
DEFB 01100011b
DEFB 00111110b

DEFB 00111110b
DEFB 01100011b
DEFB 00111111b
DEFB 00000011b
DEFB 01111110b

DEFB 00111110b
DEFB 01100011b
DEFB 01111111b
DEFB 01100011b
DEFB 01100011b

DEFB 01111110b
DEFB 01100011b
DEFB 01111110b
DEFB 01100011b
DEFB 01111110b

DEFB 00111111b
DEFB 01100000b
DEFB 01100000b
DEFB 01100000b
DEFB 00111111b

DEFB 01111110b
DEFB 01100011b
DEFB 01100011b
DEFB 01100011b
DEFB 01111110b

DEFB 00111111b
DEFB 01100000b
DEFB 01111111b
DEFB 01100000b
DEFB 00111111b

DEFB 00111111b
DEFB 01100000b
DEFB 01111111b
DEFB 01100000b
DEFB 01100000b

DEFB 00111111b
DEFB 00110000b
DEFB 00111111b
DEFB 00110000b
DEFB 00110000b

DEFB 00111111b
DEFB 00000011b
DEFB 00111111b
DEFB 00000011b
DEFB 00111111b


STACK ; workaround as stack was overwriting buffer
 DEFS 4096,0

MEMTOP
 DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.
Zeus_PC EQU START  ; Tell the emulator where to start.
Zeus_SP EQU MEMTOP ; Tell the emulator where to put the stack.


SCREEN          EQU 16384

; STACK                 EQU 63*1024     ; the stack

SCREEN_ROW      EQU 32
SCREEN_LINE     EQU 192
SCREEN_SIZE     EQU SCREEN_ROW*SCREEN_LINE

ATTRIB          EQU 22528
ATTRIB_ROW      EQU 32
ATTRIB_LINE     EQU 24
ATTRIB_SIZE     EQU ATTRIB_ROW*ATTRIB_LINE

LANDSCAPE_LEFT = 128
LANDSCAPE_RIGHT = 32
LANDSCAPE_SIZE = LANDSCAPE_LEFT + LANDSCAPE_RIGHT

MAP_LINE_0_0 EQU MAP_LINE_0_1 -LANDSCAPE_SIZE
MAP_LINE_0_1 EQU MAP_LINE_0_2 -LANDSCAPE_SIZE
MAP_LINE_0_2 EQU MAP_LINE_0_3 -LANDSCAPE_SIZE
MAP_LINE_0_3 EQU MAP_LINE_0_4 -LANDSCAPE_SIZE
MAP_LINE_0_4 EQU MAP_LINE_0_5 -LANDSCAPE_SIZE
MAP_LINE_0_5 EQU MAP_LINE_0_6 -LANDSCAPE_SIZE
MAP_LINE_0_6 EQU MAP_LINE_0_7 -LANDSCAPE_SIZE
MAP_LINE_0_7 EQU MAP_LINE_1_0 -LANDSCAPE_SIZE

MAP_LINE_1_0 EQU MAP_LINE_1_1 -LANDSCAPE_SIZE
MAP_LINE_1_1 EQU MAP_LINE_1_2 -LANDSCAPE_SIZE
MAP_LINE_1_2 EQU MAP_LINE_1_3 -LANDSCAPE_SIZE
MAP_LINE_1_3 EQU MAP_LINE_1_4 -LANDSCAPE_SIZE
MAP_LINE_1_4 EQU MAP_LINE_1_5 -LANDSCAPE_SIZE
MAP_LINE_1_5 EQU MAP_LINE_1_6 -LANDSCAPE_SIZE
MAP_LINE_1_6 EQU MAP_LINE_1_7 -LANDSCAPE_SIZE
MAP_LINE_1_7 EQU MAP_LINE_2_0 -LANDSCAPE_SIZE

MAP_LINE_2_0 EQU MAP_LINE_2_1 -LANDSCAPE_SIZE
MAP_LINE_2_1 EQU MAP_LINE_2_2 -LANDSCAPE_SIZE
MAP_LINE_2_2 EQU MAP_LINE_2_3 -LANDSCAPE_SIZE
MAP_LINE_2_3 EQU MAP_LINE_2_4 -LANDSCAPE_SIZE
MAP_LINE_2_4 EQU MAP_LINE_2_5 -LANDSCAPE_SIZE
MAP_LINE_2_5 EQU MAP_LINE_2_6 -LANDSCAPE_SIZE
MAP_LINE_2_6 EQU MAP_LINE_2_7 -LANDSCAPE_SIZE
MAP_LINE_2_7 EQU MAP_LINE_3_0 -LANDSCAPE_SIZE

MAP_LINE_3_0 EQU MAP_LINE_3_1 -LANDSCAPE_SIZE
MAP_LINE_3_1 EQU MAP_LINE_3_2 -LANDSCAPE_SIZE
MAP_LINE_3_2 EQU MAP_LINE_3_3 -LANDSCAPE_SIZE
MAP_LINE_3_3 EQU MAP_LINE_3_4 -LANDSCAPE_SIZE
MAP_LINE_3_4 EQU MAP_LINE_3_5 -LANDSCAPE_SIZE
MAP_LINE_3_5 EQU MAP_LINE_3_6 -LANDSCAPE_SIZE
MAP_LINE_3_6 EQU MAP_LINE_3_7 -LANDSCAPE_SIZE
MAP_LINE_3_7 EQU MAP_LINE_4_0 -LANDSCAPE_SIZE

MAP_LINE_4_0 EQU MAP_LINE_4_1 -LANDSCAPE_SIZE
MAP_LINE_4_1 EQU MAP_LINE_4_2 -LANDSCAPE_SIZE
MAP_LINE_4_2 EQU MAP_LINE_4_3 -LANDSCAPE_SIZE
MAP_LINE_4_3 EQU MAP_LINE_4_4 -LANDSCAPE_SIZE
MAP_LINE_4_4 EQU MAP_LINE_4_5 -LANDSCAPE_SIZE
MAP_LINE_4_5 EQU MAP_LINE_4_6 -LANDSCAPE_SIZE
MAP_LINE_4_6 EQU MAP_LINE_4_7 -LANDSCAPE_SIZE
MAP_LINE_4_7 EQU MAP_LINE_5_0 -LANDSCAPE_SIZE

MAP_LINE_5_0 EQU MAP_LINE_5_1 -LANDSCAPE_SIZE
MAP_LINE_5_1 EQU MAP_LINE_5_2 -LANDSCAPE_SIZE
MAP_LINE_5_2 EQU MAP_LINE_5_3 -LANDSCAPE_SIZE
MAP_LINE_5_3 EQU MAP_LINE_5_4 -LANDSCAPE_SIZE
MAP_LINE_5_4 EQU MAP_LINE_5_5 -LANDSCAPE_SIZE
MAP_LINE_5_5 EQU MAP_LINE_5_6 -LANDSCAPE_SIZE
MAP_LINE_5_6 EQU MAP_LINE_5_7 -LANDSCAPE_SIZE
MAP_LINE_5_7 EQU MAP_LINE_6_0 -LANDSCAPE_SIZE

MAP_LINE_6_0 EQU MAP_LINE_6_1 -LANDSCAPE_SIZE
MAP_LINE_6_1 EQU MAP_LINE_6_2 -LANDSCAPE_SIZE
MAP_LINE_6_2 EQU MAP_LINE_6_3 -LANDSCAPE_SIZE
MAP_LINE_6_3 EQU MAP_LINE_6_4 -LANDSCAPE_SIZE
MAP_LINE_6_4 EQU MAP_LINE_6_5 -LANDSCAPE_SIZE
MAP_LINE_6_5 EQU MAP_LINE_6_6 -LANDSCAPE_SIZE
MAP_LINE_6_6 EQU MAP_LINE_6_7 -LANDSCAPE_SIZE
MAP_LINE_6_7 EQU MAP_LINE_7_0 -LANDSCAPE_SIZE

MAP_LINE_7_0 EQU MAP_LINE_7_1 -LANDSCAPE_SIZE
MAP_LINE_7_1 EQU MAP_LINE_7_2 -LANDSCAPE_SIZE
MAP_LINE_7_2 EQU MAP_LINE_7_3 -LANDSCAPE_SIZE
MAP_LINE_7_3 EQU MAP_LINE_7_4 -LANDSCAPE_SIZE
MAP_LINE_7_4 EQU MAP_LINE_7_5 -LANDSCAPE_SIZE
MAP_LINE_7_5 EQU MAP_LINE_7_6 -LANDSCAPE_SIZE
MAP_LINE_7_6 EQU MAP_LINE_7_7 -LANDSCAPE_SIZE
MAP_LINE_7_7 EQU 63*1024

SCN_LINE_0 EQU SCREEN+(2*2048)+(0*256)+(7*32)+1
SCN_LINE_1 EQU SCREEN+(2*2048)+(1*256)+(7*32)+1
SCN_LINE_2 EQU SCREEN+(2*2048)+(2*256)+(7*32)+1
SCN_LINE_3 EQU SCREEN+(2*2048)+(3*256)+(7*32)+1
SCN_LINE_4 EQU SCREEN+(2*2048)+(4*256)+(7*32)+1
SCN_LINE_5 EQU SCREEN+(2*2048)+(5*256)+(7*32)+1
SCN_LINE_6 EQU SCREEN+(2*2048)+(6*256)+(7*32)+1
SCN_LINE_7 EQU SCREEN+(2*2048)+(7*256)+(7*32)+1


;plot: ; new - c=hor l=ver
; ld b,high scrbufy     ;               #7
; ld a,(bc)                     ; hor   #7 14

; ld h,high scrbufvlo; verlo    #7 21
; add a,[hl)            ;               #7 28
; inc h                                 ; verhi #4 32
; ld h,(hl)                     ;               #7 39
; ld l,a                                ;               #4 43

; inc b                         ; or    #4 47
; ld a,(bc)                     ;               #7 54
; or (hl)                       ;               #7 61
; ld (hl), a            ;               #7 68
; ret

; const vh IsometricPoint(const xyz &pos)
; {
;       return VH(pos.x + pos.y + pos.z, ISOLEFT + ( (pos.x - pos.y) <<1 ));
; }

;iso ; a = pos.z b = pos.x c = pos.y ; return VH(pos.x + pos.y + pos.z, ISOLEFT + ( (pos.x - pos.y) <<1 )); in a and d
; add a, b               ; pos.z + pos.x                         ; #4
; add a, c               ; pos.z + pos.x + pos.y         ; #4
; ld d, a                ; rc                                            ; #4
; ld a, isoleft  ; isoleft                                       ; #7
; add a, b               ; isoleft + pos.x                       ; #4
; sub a, c               ; isoleft + pos.x - pos.y       ; #4
