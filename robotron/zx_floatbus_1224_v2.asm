; This command tells Zeus where to put the code it generates. As a szx file... Alter the path to suit your system

 zeusemulate "48K"
 output_szx "spi.szx",32768,START     ; The szx file

; output_bin "c:\spi.bin",$0000,$10000    ; The binary file ; If for some reason you want binary, uncomment this line
 ORG 32768

SCREEN          EQU 16384

; STACK                 EQU 63*1024     ; the stack
TMP_BUF EQU 48*1024 ; the temp buffer overwritten by the back buffer and sprites
BCK_BUF EQU TMP_BUF+(8*1024) ; the back buffer

SCREEN_ROW      EQU 32
SCREEN_LINE     EQU 192
SCREEN_SIZE     EQU SCREEN_ROW*SCREEN_LINE

ATTRIB          EQU 22528
ATTRIB_ROW      EQU 32
ATTRIB_LINE     EQU 24
ATTRIB_SIZE     EQU ATTRIB_ROW*ATTRIB_LINE

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

; LD c, 173
; LD a, (endy)
; ld e,a
; LD HL,testspr00 ; SPO16240100
; CALL SPRITE_XOR_1624
   call draw_xor

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

MAIN_LOOP
 CALL V_BLANK

 BORDER (3)
 CALL MOVE_POINTS

 BORDER (4)
 LD E, 180
 LD C, 72
 LD HL,testspr161600
 CALL SPRITE_PUT_1616

 BORDER (5)
 call draw_xor

 BORDER (6)
 call draw_put ; draw_xor

 BORDER (7)
 CALL KEYBOARD

 BORDER (3)
JP MAIN_LOOP

draw_xor: proc
 LD a, (sttx)
 ld c,a
 LD a, (endy)
 ld e,a
 LD HL,testspr00
 CALL SPRITE_XOR_1624

; LD a, (sttx)
; ld c,a
; LD e, 122
; LD HL,testspr00
; CALL SPRITE_XOR_1624

; LD E, 144
; LD C, 73
; LD HL,testspr00
; CALL SPRITE_XOR_1624

 LD E, 180
 LD C, 72
 LD HL,testspr161600
 CALL SPRITE_PUT_1616

 LD E, 200
 LD C, 73
 LD HL,testspr161600
 CALL SPRITE_PUT_1616

 LD E, 200
 LD C, 42
 LD HL,testspr081600
 CALL SPRITE_PUT_0816

 LD E, 230
 LD C, 43
 LD HL,testspr081600
 CALL SPRITE_PUT_0816

 ret
 endp

draw_put: proc
 LD c, 10
 LD a, (endy)
 ld e,a
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD c, 33
 LD a, (endy)
 ld e,a
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD c, 56
 LD a, (endy)
 ld e,a
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD c, 79
 LD a, (endy)
 ld e,a
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD c, 110
 LD a, (endy)
 ld e,a
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD a, (sttx)
 ld c,a
 LD e, 12
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD a, (sttx)
 ld c,a
 LD e, 33
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD a, (sttx)
 ld c,a
 LD e, 66
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD a, (sttx)
 ld c,a
 LD e, 99
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD a, (sttx)
 ld c,a
 LD e, 128
 LD HL,testsprv200
 CALL SPRITE_PUT_1624

 LD C,160
 LD E,160
 LD HL,SPO12240100
 CALL SPRITE_PUT_1224

 LD C,173
 LD E,172
 LD HL,SPO12240100
 CALL SPRITE_PUT_1224

 ret
 endp

draw_clr: proc
 LD c, 173
 LD a, (endy)
 ld l,a
 CALL SPRITE_CLR_1624

 LD a, (sttx)
 ld c,a
 LD l, 122
 CALL SPRITE_CLR_1624

 LD E, 144
 LD l, 73
 CALL SPRITE_CLR_1624

 LD E, 180
 LD l, 72
 CALL SPRITE_CLR_1624

  ret
  endp

; sprite_put_0808
; sprite_put_0816

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
        push hl
        pop de
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

CLR_LINE_EVEN_ODD MACRO ()
 LD A, (BC)         ; HI BYTE POS                #7 7
 LD H, A            ;                            #4 11
 INC B              ;                            #4 15
 LD A, (BC)         ; SCREEN V TABLE LO          #7 22
 ADD A, E           ; LO BYTE POS + HOR BYTE POS #4 26
 LD L, A            ;                            #4 30
MEND

CLR_LONG_EVEN_ODD_24 MACRO ()
 LD     (HL), D ; START
 INC    L
 LD     (HL), D ; MIDDLE
 INC    L
 LD     (HL), D ; END
 INC    H       ; NEXT VER
 LD     (HL), D ; END
 DEC    L
 LD     (HL), D ; MIDDLE
 DEC    L
 LD     (HL), D ; START
MEND

CLR_WORD_EVEN_ODD_24 MACRO ()
 LD     (HL), D ; START
 INC    L
 LD     (HL), D ; MIDDLE
 INC    L
 LD     (HL), D ; END
MEND

CLR_LONG_EVEN_ODD MACRO ()
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
 INC    H       ; NEXT VER
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

CLR_WORD_EVEN_ODD MACRO ()
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

CLEAR_1616      PROC
                LD  D, HIGH ScrBufY                 ; HOR BYTES
                LD  A, (DE)                         ; C=HOR BYTE POS
                LD  E,A

                LD  D,0

                SRL C
                JP  C, SPR_ODD

SPR_EVEN:       LD B, HIGH ScrBufEvenH
                CLR_LINE_EVEN_ODD () ; 0
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 2
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 4
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 6
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 8
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 10
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 12
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 14
                CLR_LONG_EVEN_ODD ()
                RET

SPR_ODD:        LD B, HIGH ScrBufOddH
                CLR_LINE_EVEN_ODD () ; 0
                CLR_WORD_EVEN_ODD ()
                INC C

                LD B, HIGH ScrBufEvenH
                CLR_LINE_EVEN_ODD () ; 1
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 3
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 5
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 7
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 9
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 11
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 13
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 15
                CLR_WORD_EVEN_ODD ()
                RET
                ENDP

; l=x pos
; e=y pos
; HL = sprite address (needs to be put into SP)

SPR_AND_OR_LONG_EVEN MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L               ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_AND_OR_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

; l=x pos
; e=y pos
; HL = sprite address (needs to be put into SP)

SPR_AND_OR_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L               ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

; l=x pos
; e=y pos
; HL = sprite address (needs to be put into SP)

SPR_AND_OR_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_LINE_EVEN MACRO ()
 LD H, HIGH ScrBufEvenL     ; SCREEN V TABLE LO  #7 7
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18
 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND

SPR_LINE_ODD MACRO ()
 LD H, HIGH ScrBufOddL     ; SCREEN V TABLE LO   #7 7
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18
 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND

SPR_OR_LONG_EVEN MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L               ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L               ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPRITE_OR_1616      PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 12
                    SPR_OR_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 14
                    SPR_OR_LONG_EVEN ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_OR_WORD_ODD_START ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 13
                    INC L
                    SPR_OR_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 15
                    INC L
                    SPR_OR_WORD_ODD_END ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPR_PUT_LONG_EVEN_16 MACRO ()
 POP    DE      ; GRAPHIC
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE

 INC    H       ; NEXT VER
 POP    DE      ; GRAPHIC
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

SPR_PUT_LONG_ODD_16 MACRO ()
 POP    DE      ; GRAPHIC
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE

 INC    H       ; NEXT VER
 POP    DE      ; GRAPHIC
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

SPR_PUT_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 DEC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_START_16 MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_END_16 MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; PREV HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

SPRITE_PUT_1616     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 12
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 14
                    SPR_PUT_LONG_EVEN_16 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_PUT_WORD_ODD_START_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 13
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 15
                    SPR_PUT_WORD_ODD_END_16 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_PUT_0816     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_PUT_LONG_EVEN_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_PUT_LONG_EVEN_16 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_PUT_WORD_ODD_START_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    SPR_PUT_LONG_ODD_16 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    SPR_PUT_WORD_ODD_END_16 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPR_PUT_LONG_EVEN_24 MACRO ()
 POP    DE      ; GRAPHIC
 LD     (HL), e ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC
 LD     (HL), e ; PUT SCREEN BYTE
 INC    H       ; NEXT VER
 LD     (HL), d ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC
 LD     (HL), d ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), e ; PUT SCREEN BYTE
MEND

SPR_XOR_LONG_EVEN_24 MACRO ()
 POP    DE      ; GRAPHIC
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_24 MACRO ()
 POP    DE      ; get left and middle
 LD     (HL), e ; PUT left
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT middle
 INC    L       ; NEXT HOR

 POP    DE      ; get right and below
 LD     (HL), e ; put right
MEND

SPR_XOR_WORD_ODD_24 MACRO ()
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 POP    DE      ; get right and below
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_PUT_LONG_ODD_RIGHT_24 MACRO ()
 POP    DE      ; get left and middle
 LD     (HL), e ; PUT left
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT middle
 INC    L       ; NEXT HOR
 LD     (HL), A ; put right
 INC    H       ; NEXT VER

 POP    DE      ; get left and middle
 DEC    L       ; PREV HOR
 LD     (HL), d ; PUT middle
 DEC    L       ; PREV HOR
 LD     (HL), e ; PUT left
 POP    DE      ; get right and below

 INC    L       ; NEXT HOR
 INC    L       ; NEXT HOR
 LD     (HL), e ; PUT right
MEND

SPR_XOR_LONG_ODD_RIGHT_24 MACRO ()
 EX AF,AF'
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 EX AF,AF'
 ld d,a
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT right
 INC    H       ; NEXT VER

 POP    DE      ; get left and middle
 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 POP    DE      ; get right and below

 INC    L       ; NEXT HOR
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_RIGHT_24 MACRO ()
 POP    DE      ; get left and middle
 LD     (HL), e ; PUT left
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT middle
 INC    L       ; NEXT HOR
 LD     (HL), A ; put right
MEND

SPR_XOR_WORD_ODD_RIGHT_24 MACRO ()
 EX AF,AF'
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 EX AF,AF'
 ld d,a
 LD     A, (HL) ; get screen byte
 XOR    D
 LD     (HL), A ; PUT right
 INC    H       ; NEXT VER
MEND

SPRITE_PUT_1624     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 12
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 14
                    SPR_PUT_LONG_EVEN_24 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_PUT_WORD_ODD_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    LD A,D                                                  ; backup below
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 13
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 15
                    LD A,D
                    SPR_PUT_WORD_ODD_RIGHT_24 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_PUT_1224     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_PUT_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_PUT_LONG_EVEN_24 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_PUT_WORD_ODD_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    LD A,D                                                  ; backup below
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    LD A,D
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                                        LD A,D
                                        SPR_PUT_WORD_ODD_RIGHT_24 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_XOR_1624     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 12
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 14
                    SPR_XOR_LONG_EVEN_24 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_XOR_WORD_ODD_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    LD A,D                                                  ; backup below
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 13
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 15
                    LD A,D
                    SPR_XOR_WORD_ODD_RIGHT_24 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_XOR_1224     PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 8
                    SPR_XOR_LONG_EVEN_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 10
                    SPR_XOR_LONG_EVEN_24 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_XOR_WORD_ODD_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    LD A,D                                                  ; backup below
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24 ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    LD A,D
                    SPR_XOR_WORD_ODD_RIGHT_24 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_CLR_1624     PROC   ; l=hor / c=ver
                LD  H, HIGH ScrBufY                 ; HOR BYTES
                LD  E, (HL)                         ; L=HOR BYTE POS

                LD  D,0

                SRL C
                JP  C, SPR_ODD

SPR_EVEN:       LD B, HIGH ScrBufEvenH
                CLR_LINE_EVEN_ODD () ; 0
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 2
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 4
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 6
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 8
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 10
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 12
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 14
                CLR_LONG_EVEN_ODD_24 ()
                RET

SPR_ODD:        LD B, HIGH ScrBufOddH
                CLR_LINE_EVEN_ODD () ; 0
                CLR_WORD_EVEN_ODD_24 ()
                INC C

                LD B, HIGH ScrBufEvenH
                CLR_LINE_EVEN_ODD () ; 1
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 3
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 5
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 7
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 9
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 11
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 13
                DEC B
                CLR_LONG_EVEN_ODD_24 ()
                INC C

                CLR_LINE_EVEN_ODD () ; 15
                CLR_WORD_EVEN_ODD_24 ()
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

ALIGN $100
tst0 defw tst1,tst2,tst3,tst4,tst5,tst6,tst7,tst8

testspr00  defw testspr01,testspr02,testspr03,testspr04,testspr05,testspr06,testspr07,testspr08

testsprv200     defw testsprv201,testsprv202,testsprv203,testsprv204,testsprv205,testsprv206,testsprv207,testsprv208


SPO16160100     DEFW SPO16160101,SPO16160102,SPO16160103,SPO16160104,SPO16160105,SPO16160106,SPO16160107,SPO16160108
SPO16160200     DEFW SPO16160201,SPO16160202,SPO16160203,SPO16160204,SPO16160205,SPO16160206,SPO16160207,SPO16160208

;SPO16240100
                DEFW SPO16240101
SPO16240100
 defw SPO16240111
                DEFW SPO16240102,SPO16240103,SPO16240104,SPO16240105,SPO16240106,SPO16240107,SPO16240108
                DEFW SPO16240109

SPO12240100             DEFW SPO12240101,SPO12240101,SPO12240101,SPO12240101,SPO12240101,SPO12240101,SPO12240101,SPO12240101

testspr161600   defw testspr161601,testspr161601,testspr161601,testspr161601,testspr161601,testspr161601,testspr161601,testspr161601

testspr081600   defw testspr081601,testspr081601,testspr081601,testspr081601,testspr081601,testspr081601,testspr081601,testspr081601

ALIGN $100

ScrBufEvenH
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256,(SCREEN+(0*2048)+(2*256)+(0*32))/256,(SCREEN+(0*2048)+(4*256)+(0*32))/256,(SCREEN+(0*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))/256,(SCREEN+(0*2048)+(2*256)+(1*32))/256,(SCREEN+(0*2048)+(4*256)+(1*32))/256,(SCREEN+(0*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))/256,(SCREEN+(0*2048)+(2*256)+(2*32))/256,(SCREEN+(0*2048)+(4*256)+(2*32))/256,(SCREEN+(0*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))/256,(SCREEN+(0*2048)+(2*256)+(3*32))/256,(SCREEN+(0*2048)+(4*256)+(3*32))/256,(SCREEN+(0*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))/256,(SCREEN+(0*2048)+(2*256)+(4*32))/256,(SCREEN+(0*2048)+(4*256)+(4*32))/256,(SCREEN+(0*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))/256,(SCREEN+(0*2048)+(2*256)+(5*32))/256,(SCREEN+(0*2048)+(4*256)+(5*32))/256,(SCREEN+(0*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))/256,(SCREEN+(0*2048)+(2*256)+(6*32))/256,(SCREEN+(0*2048)+(4*256)+(6*32))/256,(SCREEN+(0*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))/256,(SCREEN+(0*2048)+(2*256)+(7*32))/256,(SCREEN+(0*2048)+(4*256)+(7*32))/256,(SCREEN+(0*2048)+(6*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))/256,(SCREEN+(1*2048)+(2*256)+(0*32))/256,(SCREEN+(1*2048)+(4*256)+(0*32))/256,(SCREEN+(1*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))/256,(SCREEN+(1*2048)+(2*256)+(1*32))/256,(SCREEN+(1*2048)+(4*256)+(1*32))/256,(SCREEN+(1*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))/256,(SCREEN+(1*2048)+(2*256)+(2*32))/256,(SCREEN+(1*2048)+(4*256)+(2*32))/256,(SCREEN+(1*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))/256,(SCREEN+(1*2048)+(2*256)+(3*32))/256,(SCREEN+(1*2048)+(4*256)+(3*32))/256,(SCREEN+(1*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))/256,(SCREEN+(1*2048)+(2*256)+(4*32))/256,(SCREEN+(1*2048)+(4*256)+(4*32))/256,(SCREEN+(1*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))/256,(SCREEN+(1*2048)+(2*256)+(5*32))/256,(SCREEN+(1*2048)+(4*256)+(5*32))/256,(SCREEN+(1*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))/256,(SCREEN+(1*2048)+(2*256)+(6*32))/256,(SCREEN+(1*2048)+(4*256)+(6*32))/256,(SCREEN+(1*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))/256,(SCREEN+(1*2048)+(2*256)+(7*32))/256,(SCREEN+(1*2048)+(4*256)+(7*32))/256,(SCREEN+(1*2048)+(6*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))/256,(SCREEN+(2*2048)+(2*256)+(0*32))/256,(SCREEN+(2*2048)+(4*256)+(0*32))/256,(SCREEN+(2*2048)+(6*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))/256,(SCREEN+(2*2048)+(2*256)+(1*32))/256,(SCREEN+(2*2048)+(4*256)+(1*32))/256,(SCREEN+(2*2048)+(6*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))/256,(SCREEN+(2*2048)+(2*256)+(2*32))/256,(SCREEN+(2*2048)+(4*256)+(2*32))/256,(SCREEN+(2*2048)+(6*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))/256,(SCREEN+(2*2048)+(2*256)+(3*32))/256,(SCREEN+(2*2048)+(4*256)+(3*32))/256,(SCREEN+(2*2048)+(6*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))/256,(SCREEN+(2*2048)+(2*256)+(4*32))/256,(SCREEN+(2*2048)+(4*256)+(4*32))/256,(SCREEN+(2*2048)+(6*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))/256,(SCREEN+(2*2048)+(2*256)+(5*32))/256,(SCREEN+(2*2048)+(4*256)+(5*32))/256,(SCREEN+(2*2048)+(6*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))/256,(SCREEN+(2*2048)+(2*256)+(6*32))/256,(SCREEN+(2*2048)+(4*256)+(6*32))/256,(SCREEN+(2*2048)+(6*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))/256,(SCREEN+(2*2048)+(2*256)+(7*32))/256,(SCREEN+(2*2048)+(4*256)+(7*32))/256,(SCREEN+(2*2048)+(6*256)+(7*32))/256

ALIGN $100

ScrBufEvenL
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255,(SCREEN+(0*2048)+(2*256)+(0*32))&255,(SCREEN+(0*2048)+(4*256)+(0*32))&255,(SCREEN+(0*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))&255,(SCREEN+(0*2048)+(2*256)+(1*32))&255,(SCREEN+(0*2048)+(4*256)+(1*32))&255,(SCREEN+(0*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))&255,(SCREEN+(0*2048)+(2*256)+(2*32))&255,(SCREEN+(0*2048)+(4*256)+(2*32))&255,(SCREEN+(0*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))&255,(SCREEN+(0*2048)+(2*256)+(3*32))&255,(SCREEN+(0*2048)+(4*256)+(3*32))&255,(SCREEN+(0*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))&255,(SCREEN+(0*2048)+(2*256)+(4*32))&255,(SCREEN+(0*2048)+(4*256)+(4*32))&255,(SCREEN+(0*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))&255,(SCREEN+(0*2048)+(2*256)+(5*32))&255,(SCREEN+(0*2048)+(4*256)+(5*32))&255,(SCREEN+(0*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))&255,(SCREEN+(0*2048)+(2*256)+(6*32))&255,(SCREEN+(0*2048)+(4*256)+(6*32))&255,(SCREEN+(0*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))&255,(SCREEN+(0*2048)+(2*256)+(7*32))&255,(SCREEN+(0*2048)+(4*256)+(7*32))&255,(SCREEN+(0*2048)+(6*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))&255,(SCREEN+(1*2048)+(2*256)+(0*32))&255,(SCREEN+(1*2048)+(4*256)+(0*32))&255,(SCREEN+(1*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))&255,(SCREEN+(1*2048)+(2*256)+(1*32))&255,(SCREEN+(1*2048)+(4*256)+(1*32))&255,(SCREEN+(1*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))&255,(SCREEN+(1*2048)+(2*256)+(2*32))&255,(SCREEN+(1*2048)+(4*256)+(2*32))&255,(SCREEN+(1*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))&255,(SCREEN+(1*2048)+(2*256)+(3*32))&255,(SCREEN+(1*2048)+(4*256)+(3*32))&255,(SCREEN+(1*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))&255,(SCREEN+(1*2048)+(2*256)+(4*32))&255,(SCREEN+(1*2048)+(4*256)+(4*32))&255,(SCREEN+(1*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))&255,(SCREEN+(1*2048)+(2*256)+(5*32))&255,(SCREEN+(1*2048)+(4*256)+(5*32))&255,(SCREEN+(1*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))&255,(SCREEN+(1*2048)+(2*256)+(6*32))&255,(SCREEN+(1*2048)+(4*256)+(6*32))&255,(SCREEN+(1*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))&255,(SCREEN+(1*2048)+(2*256)+(7*32))&255,(SCREEN+(1*2048)+(4*256)+(7*32))&255,(SCREEN+(1*2048)+(6*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))&255,(SCREEN+(2*2048)+(2*256)+(0*32))&255,(SCREEN+(2*2048)+(4*256)+(0*32))&255,(SCREEN+(2*2048)+(6*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))&255,(SCREEN+(2*2048)+(2*256)+(1*32))&255,(SCREEN+(2*2048)+(4*256)+(1*32))&255,(SCREEN+(2*2048)+(6*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))&255,(SCREEN+(2*2048)+(2*256)+(2*32))&255,(SCREEN+(2*2048)+(4*256)+(2*32))&255,(SCREEN+(2*2048)+(6*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))&255,(SCREEN+(2*2048)+(2*256)+(3*32))&255,(SCREEN+(2*2048)+(4*256)+(3*32))&255,(SCREEN+(2*2048)+(6*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))&255,(SCREEN+(2*2048)+(2*256)+(4*32))&255,(SCREEN+(2*2048)+(4*256)+(4*32))&255,(SCREEN+(2*2048)+(6*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))&255,(SCREEN+(2*2048)+(2*256)+(5*32))&255,(SCREEN+(2*2048)+(4*256)+(5*32))&255,(SCREEN+(2*2048)+(6*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))&255,(SCREEN+(2*2048)+(2*256)+(6*32))&255,(SCREEN+(2*2048)+(4*256)+(6*32))&255,(SCREEN+(2*2048)+(6*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))&255,(SCREEN+(2*2048)+(2*256)+(7*32))&255,(SCREEN+(2*2048)+(4*256)+(7*32))&255,(SCREEN+(2*2048)+(6*256)+(7*32))&255

ALIGN $100

ScrBufOddH
 DEFB (SCREEN+(0*2048)+(1*256)+(0*32))/256,(SCREEN+(0*2048)+(3*256)+(0*32))/256,(SCREEN+(0*2048)+(5*256)+(0*32))/256,(SCREEN+(0*2048)+(7*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(1*32))/256,(SCREEN+(0*2048)+(3*256)+(1*32))/256,(SCREEN+(0*2048)+(5*256)+(1*32))/256,(SCREEN+(0*2048)+(7*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(2*32))/256,(SCREEN+(0*2048)+(3*256)+(2*32))/256,(SCREEN+(0*2048)+(5*256)+(2*32))/256,(SCREEN+(0*2048)+(7*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(3*32))/256,(SCREEN+(0*2048)+(3*256)+(3*32))/256,(SCREEN+(0*2048)+(5*256)+(3*32))/256,(SCREEN+(0*2048)+(7*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(4*32))/256,(SCREEN+(0*2048)+(3*256)+(4*32))/256,(SCREEN+(0*2048)+(5*256)+(4*32))/256,(SCREEN+(0*2048)+(7*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(5*32))/256,(SCREEN+(0*2048)+(3*256)+(5*32))/256,(SCREEN+(0*2048)+(5*256)+(5*32))/256,(SCREEN+(0*2048)+(7*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(6*32))/256,(SCREEN+(0*2048)+(3*256)+(6*32))/256,(SCREEN+(0*2048)+(5*256)+(6*32))/256,(SCREEN+(0*2048)+(7*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(1*256)+(7*32))/256,(SCREEN+(0*2048)+(3*256)+(7*32))/256,(SCREEN+(0*2048)+(5*256)+(7*32))/256,(SCREEN+(0*2048)+(7*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(0*32))/256,(SCREEN+(1*2048)+(3*256)+(0*32))/256,(SCREEN+(1*2048)+(5*256)+(0*32))/256,(SCREEN+(1*2048)+(7*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(1*32))/256,(SCREEN+(1*2048)+(3*256)+(1*32))/256,(SCREEN+(1*2048)+(5*256)+(1*32))/256,(SCREEN+(1*2048)+(7*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(2*32))/256,(SCREEN+(1*2048)+(3*256)+(2*32))/256,(SCREEN+(1*2048)+(5*256)+(2*32))/256,(SCREEN+(1*2048)+(7*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(3*32))/256,(SCREEN+(1*2048)+(3*256)+(3*32))/256,(SCREEN+(1*2048)+(5*256)+(3*32))/256,(SCREEN+(1*2048)+(7*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(4*32))/256,(SCREEN+(1*2048)+(3*256)+(4*32))/256,(SCREEN+(1*2048)+(5*256)+(4*32))/256,(SCREEN+(1*2048)+(7*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(5*32))/256,(SCREEN+(1*2048)+(3*256)+(5*32))/256,(SCREEN+(1*2048)+(5*256)+(5*32))/256,(SCREEN+(1*2048)+(7*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(6*32))/256,(SCREEN+(1*2048)+(3*256)+(6*32))/256,(SCREEN+(1*2048)+(5*256)+(6*32))/256,(SCREEN+(1*2048)+(7*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(1*256)+(7*32))/256,(SCREEN+(1*2048)+(3*256)+(7*32))/256,(SCREEN+(1*2048)+(5*256)+(7*32))/256,(SCREEN+(1*2048)+(7*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(0*32))/256,(SCREEN+(2*2048)+(3*256)+(0*32))/256,(SCREEN+(2*2048)+(5*256)+(0*32))/256,(SCREEN+(2*2048)+(7*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(1*32))/256,(SCREEN+(2*2048)+(3*256)+(1*32))/256,(SCREEN+(2*2048)+(5*256)+(1*32))/256,(SCREEN+(2*2048)+(7*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(2*32))/256,(SCREEN+(2*2048)+(3*256)+(2*32))/256,(SCREEN+(2*2048)+(5*256)+(2*32))/256,(SCREEN+(2*2048)+(7*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(3*32))/256,(SCREEN+(2*2048)+(3*256)+(3*32))/256,(SCREEN+(2*2048)+(5*256)+(3*32))/256,(SCREEN+(2*2048)+(7*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(4*32))/256,(SCREEN+(2*2048)+(3*256)+(4*32))/256,(SCREEN+(2*2048)+(5*256)+(4*32))/256,(SCREEN+(2*2048)+(7*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(5*32))/256,(SCREEN+(2*2048)+(3*256)+(5*32))/256,(SCREEN+(2*2048)+(5*256)+(5*32))/256,(SCREEN+(2*2048)+(7*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(6*32))/256,(SCREEN+(2*2048)+(3*256)+(6*32))/256,(SCREEN+(2*2048)+(5*256)+(6*32))/256,(SCREEN+(2*2048)+(7*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(1*256)+(7*32))/256,(SCREEN+(2*2048)+(3*256)+(7*32))/256,(SCREEN+(2*2048)+(5*256)+(7*32))/256,(SCREEN+(2*2048)+(7*256)+(7*32))/256

ALIGN $100

ScrBufOddL
 DEFB (SCREEN+(0*2048)+(1*256)+(0*32))&255,(SCREEN+(0*2048)+(3*256)+(0*32))&255,(SCREEN+(0*2048)+(5*256)+(0*32))&255,(SCREEN+(0*2048)+(7*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(1*32))&255,(SCREEN+(0*2048)+(3*256)+(1*32))&255,(SCREEN+(0*2048)+(5*256)+(1*32))&255,(SCREEN+(0*2048)+(7*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(2*32))&255,(SCREEN+(0*2048)+(3*256)+(2*32))&255,(SCREEN+(0*2048)+(5*256)+(2*32))&255,(SCREEN+(0*2048)+(7*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(3*32))&255,(SCREEN+(0*2048)+(3*256)+(3*32))&255,(SCREEN+(0*2048)+(5*256)+(3*32))&255,(SCREEN+(0*2048)+(7*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(4*32))&255,(SCREEN+(0*2048)+(3*256)+(4*32))&255,(SCREEN+(0*2048)+(5*256)+(4*32))&255,(SCREEN+(0*2048)+(7*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(5*32))&255,(SCREEN+(0*2048)+(3*256)+(5*32))&255,(SCREEN+(0*2048)+(5*256)+(5*32))&255,(SCREEN+(0*2048)+(7*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(6*32))&255,(SCREEN+(0*2048)+(3*256)+(6*32))&255,(SCREEN+(0*2048)+(5*256)+(6*32))&255,(SCREEN+(0*2048)+(7*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(1*256)+(7*32))&255,(SCREEN+(0*2048)+(3*256)+(7*32))&255,(SCREEN+(0*2048)+(5*256)+(7*32))&255,(SCREEN+(0*2048)+(7*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(0*32))&255,(SCREEN+(1*2048)+(3*256)+(0*32))&255,(SCREEN+(1*2048)+(5*256)+(0*32))&255,(SCREEN+(1*2048)+(7*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(1*32))&255,(SCREEN+(1*2048)+(3*256)+(1*32))&255,(SCREEN+(1*2048)+(5*256)+(1*32))&255,(SCREEN+(1*2048)+(7*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(2*32))&255,(SCREEN+(1*2048)+(3*256)+(2*32))&255,(SCREEN+(1*2048)+(5*256)+(2*32))&255,(SCREEN+(1*2048)+(7*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(3*32))&255,(SCREEN+(1*2048)+(3*256)+(3*32))&255,(SCREEN+(1*2048)+(5*256)+(3*32))&255,(SCREEN+(1*2048)+(7*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(4*32))&255,(SCREEN+(1*2048)+(3*256)+(4*32))&255,(SCREEN+(1*2048)+(5*256)+(4*32))&255,(SCREEN+(1*2048)+(7*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(5*32))&255,(SCREEN+(1*2048)+(3*256)+(5*32))&255,(SCREEN+(1*2048)+(5*256)+(5*32))&255,(SCREEN+(1*2048)+(7*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(6*32))&255,(SCREEN+(1*2048)+(3*256)+(6*32))&255,(SCREEN+(1*2048)+(5*256)+(6*32))&255,(SCREEN+(1*2048)+(7*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(1*256)+(7*32))&255,(SCREEN+(1*2048)+(3*256)+(7*32))&255,(SCREEN+(1*2048)+(5*256)+(7*32))&255,(SCREEN+(1*2048)+(7*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(0*32))&255,(SCREEN+(2*2048)+(3*256)+(0*32))&255,(SCREEN+(2*2048)+(5*256)+(0*32))&255,(SCREEN+(2*2048)+(7*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(1*32))&255,(SCREEN+(2*2048)+(3*256)+(1*32))&255,(SCREEN+(2*2048)+(5*256)+(1*32))&255,(SCREEN+(2*2048)+(7*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(2*32))&255,(SCREEN+(2*2048)+(3*256)+(2*32))&255,(SCREEN+(2*2048)+(5*256)+(2*32))&255,(SCREEN+(2*2048)+(7*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(3*32))&255,(SCREEN+(2*2048)+(3*256)+(3*32))&255,(SCREEN+(2*2048)+(5*256)+(3*32))&255,(SCREEN+(2*2048)+(7*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(4*32))&255,(SCREEN+(2*2048)+(3*256)+(4*32))&255,(SCREEN+(2*2048)+(5*256)+(4*32))&255,(SCREEN+(2*2048)+(7*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(5*32))&255,(SCREEN+(2*2048)+(3*256)+(5*32))&255,(SCREEN+(2*2048)+(5*256)+(5*32))&255,(SCREEN+(2*2048)+(7*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(6*32))&255,(SCREEN+(2*2048)+(3*256)+(6*32))&255,(SCREEN+(2*2048)+(5*256)+(6*32))&255,(SCREEN+(2*2048)+(7*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(1*256)+(7*32))&255,(SCREEN+(2*2048)+(3*256)+(7*32))&255,(SCREEN+(2*2048)+(5*256)+(7*32))&255,(SCREEN+(2*2048)+(7*256)+(7*32))&255

ALIGN $100

ScrBufY
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

ScrBufSprY
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14


 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14
 DEFB 0,2,4,6,8,10,12,14

; sprite
SPO16160101NA
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111

SPO16160101
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111
 DEFB %01111111,%00000000
 DEFB %00000000,%01111111

SPO16160102
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111
 DEFB %00111111,%10000000
 DEFB %10000000,%00111111

SPO16160103
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111
 DEFB %00011111,%11000000
 DEFB %11000000,%00011111

SPO16160104
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111
 DEFB %00001111,%11100000
 DEFB %11100000,%00001111

SPO16160105
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111
 DEFB %00000111,%11110000
 DEFB %11110000,%00000111

SPO16160106
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011
 DEFB %00000011,%11111000
 DEFB %11111000,%00000011

SPO16160107
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001
 DEFB %00000001,%11111100
 DEFB %11111100,%00000001

SPO16160108
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000
 DEFB %00000000,%11111110
 DEFB %11111110,%00000000

; sprite
SPO16160201
 DEFB %0,%0
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %0,%0

SPO16160202
 DEFB %0,%0
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %0,%0

SPO16160203
 DEFB %0,%0
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %0,%0

SPO16160204
 DEFB %0,%0
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %0,%0

SPO16160205
 DEFB %0,%0
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %0,%0

SPO16160206
 DEFB %0,%0
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %0,%0

SPO16160207
 DEFB %0,%0
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %0,%0

SPO16160208
 DEFB %0,%0
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %0,%0

; sprite
SPO16240101
 DEFB %0,%0,%0
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%11111111,%00000000
 DEFB %0,%0,%0

SPO16240102
 DEFB %0,%0,%0
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %10000000,%01111111,%11111111
 DEFB %01111111,%11111111,%10000000
 DEFB %0,%0,%0

SPO16240103
 DEFB %0,%0,%0
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %11000000,%00111111,%11111111
 DEFB %00111111,%11111111,%11000000
 DEFB %0,%0,%0

SPO16240104
 DEFB %0,%0,%0
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %11100000,%00011111,%11111111
 DEFB %00011111,%11111111,%11100000
 DEFB %0,%0,%0

SPO16240105
 DEFB %0,%0,%0
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %11110000,%00001111,%11111111
 DEFB %00001111,%11111111,%11110000
 DEFB %0,%0,%0

SPO16240106
 DEFB %0,%0,%0
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %11111000,%00000111,%11111111
 DEFB %00000111,%11111111,%11111000
 DEFB %0,%0,%0

SPO16240107
 DEFB %0,%0,%0
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %11111100,%00000011,%11111111
 DEFB %00000011,%11111111,%11111100
 DEFB %0,%0,%0

SPO16240108
 DEFB %0,%0,%0
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %11111110,%00000001,%11111111
 DEFB %00000001,%11111111,%11111110
 DEFB %0,%0,%0

SPO16240109
 DEFB %0,%0,%0
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %11111111,%00000000,%11111111
 DEFB %00000000,%11111111,%11111111
 DEFB %0,%0,%0

SPO16240110
 defb $00,$00,$00
 defb $00,$7f,$fe
 defb $40,$02,$00
 defb $00,$5f,$fa
 defb $50,$0a,$00
 defb $00,$57,$ea
 defb $54,$2a,$00
 defb $00,$55,$aa
 defb $55,$aa,$00
 defb $00,$54,$2a
 defb $57,$ea,$00
 defb $00,$50,$0a
 defb $5f,$fa,$00
 defb $00,$40,$02
 defb $7f,$fe,$00
 defb $00,$00,$00

SPO16240111
 defb $ff,$ff,$00
 defb $00,$80,$01
 defb $bf,$fd,$00
 defb $00,$a0,$05
 defb $af,$f5,$00
 defb $00,$a8,$15
 defb $ab,$d5,$00
 defb $00,$aa,$55
 defb $aa,$55,$00
 defb $00,$ab,$d5
 defb $a8,$15,$00
 defb $00,$af,$f5
 defb $a0,$05,$00
 defb $00,$bf,$fd
 defb $80,$01,$00
 defb $00,$ff,$ff

 defb %11110000,%10101010,%11001100,%11110000

SPO12240101
 defb $ff,$f0,$00
 defb $00,$80,$10
 defb $80,$10,$00
 defb $00,$80,$10
 defb $80,$10,$00
 defb $00,$80,$10
 defb $80,$10,$00
 defb $00,$80,$10
 defb $80,$10,$00
 defb $00,$80,$10
 defb $80,$10,$00
 defb $00,$ff,$f0

 defb %10111011,%11101110,%10101010

; defb 1,1,1

tst1:
 defb 255,255,0
 defb 128,1,0
 defb 185,57,0
 defb 171,9,0
 defb 169,57,0
 defb 169,33,0
 defb 187,185,0
 defb 128,1,0
 defb 186,185,0
 defb 138,161,0
 defb 187,185,0
 defb 138,9,0
 defb 186,57,0
 defb 128,1,0
 defb 128,1,0
 defb 255,255,0
tst2:
 defb 127,255,128
 defb 128,0,64
 defb 92,156,128
 defb 128,132,85
 defb 84,156,128
 defb 128,144,84
 defb 93,220,128
 defb 128,0,64
 defb 93,92,128
 defb 128,80,69
 defb 93,220,128
 defb 128,4,69
 defb 93,28,128
 defb 128,0,64
 defb 64,0,128
 defb 128,255,127
tst3:
 defb 63,255,192
 defb 64,0,32
 defb 46,78,64
 defb 64,194,42
 defb 42,78,64
 defb 64,72,42
 defb 46,238,64
 defb 64,0,32
 defb 46,174,64
 defb 64,168,34
 defb 46,238,64
 defb 64,130,34
 defb 46,142,64
 defb 64,0,32
 defb 32,0,64
 defb 192,255,63
tst4:
 defb 31,255,224
 defb 32,0,16
 defb 23,39,32
 defb 32,97,21
 defb 21,39,32
 defb 32,36,21
 defb 23,119,32
 defb 32,0,16
 defb 23,87,32
 defb 32,84,17
 defb 23,119,32
 defb 32,65,17
 defb 23,71,32
 defb 32,0,16
 defb 16,0,32
 defb 224,255,31
tst5:
 defb 15,255,240
 defb 16,0,8
 defb 11,147,144
 defb 144,176,10
 defb 10,147,144
 defb 16,146,10
 defb 11,187,144
 defb 16,0,8
 defb 11,171,144
 defb 16,170,8
 defb 11,187,144
 defb 144,160,8
 defb 11,163,144
 defb 16,0,8
 defb 8,0,16
 defb 240,255,15
tst6:
 defb 7,255,248
 defb 8,0,4
 defb 5,201,200
 defb 72,88,5
 defb 5,73,200
 defb 8,73,5
 defb 5,221,200
 defb 8,0,4
 defb 5,213,200
 defb 8,85,4
 defb 5,221,200
 defb 72,80,4
 defb 5,209,200
 defb 8,0,4
 defb 4,0,8
 defb 248,255,7
tst7:
 defb 3,255,252
 defb 4,0,2
 defb 2,228,228
 defb 36,172,2
 defb 2,164,228
 defb 132,164,2
 defb 2,238,228
 defb 4,0,2
 defb 2,234,228
 defb 132,42,2
 defb 2,238,228
 defb 36,40,2
 defb 2,232,228
 defb 4,0,2
 defb 2,0,4
 defb 252,255,3
tst8:
 defb 1,255,254
 defb 2,0,1
 defb 1,114,114
 defb 18,86,1
 defb 1,82,114
 defb 66,82,1
 defb 1,119,114
 defb 2,0,1
 defb 1,117,114
 defb 66,21,1
 defb 1,119,114
 defb 18,20,1
 defb 1,116,114
 defb 2,0,1
 defb 1,0,2
 defb 254,255,1

testspr01
 defb 255,255,0
 defb 0,128,1
 defb 147,185,0
 defb 0,176,137
 defb 147,185,0
 defb 0,146,9
 defb 187,185,0
 defb 0,128,1
 defb 171,185,0
 defb 0,170,33
 defb 187,185,0
 defb 0,136,169
 defb 139,185,0
 defb 0,128,1
 defb 128,1,0
 defb 0,255,255
testspr02
 defb 127,255,128
 defb 128,64,0
 defb 73,220,128
 defb 128,88,68
 defb 73,220,128
 defb 128,73,4
 defb 93,220,128
 defb 128,64,0
 defb 85,220,128
 defb 128,85,16
 defb 93,220,128
 defb 128,68,84
 defb 69,220,128
 defb 128,64,0
 defb 64,0,128
 defb 128,127,255
testspr03
 defb 63,255,192
 defb 64,32,0
 defb 36,238,64
 defb 64,44,34
 defb 36,238,64
 defb 64,36,130
 defb 46,238,64
 defb 64,32,0
 defb 42,238,64
 defb 64,42,136
 defb 46,238,64
 defb 64,34,42
 defb 34,238,64
 defb 64,32,0
 defb 32,0,64
 defb 192,63,255
testspr04
 defb 31,255,224
 defb 32,16,0
 defb 18,119,32
 defb 32,22,17
 defb 18,119,32
 defb 32,18,65
 defb 23,119,32
 defb 32,16,0
 defb 21,119,32
 defb 32,21,68
 defb 23,119,32
 defb 32,17,21
 defb 17,119,32
 defb 32,16,0
 defb 16,0,32
 defb 224,31,255
testspr05
 defb 15,255,240
 defb 16,8,0
 defb 9,59,144
 defb 144,11,8
 defb 9,59,144
 defb 144,9,32
 defb 11,187,144
 defb 16,8,0
 defb 10,187,144
 defb 16,10,162
 defb 11,187,144
 defb 144,8,138
 defb 8,187,144
 defb 16,8,0
 defb 8,0,16
 defb 240,15,255
testspr06
 defb 7,255,248
 defb 8,4,0
 defb 4,157,200
 defb 72,5,132
 defb 4,157,200
 defb 72,4,144
 defb 5,221,200
 defb 8,4,0
 defb 5,93,200
 defb 8,5,81
 defb 5,221,200
 defb 72,4,69
 defb 4,93,200
 defb 8,4,0
 defb 4,0,8
 defb 248,7,255
testspr07
 defb 3,255,252
 defb 4,2,0
 defb 2,78,228
 defb 36,2,194
 defb 2,78,228
 defb 36,2,72
 defb 2,238,228
 defb 4,2,0
 defb 2,174,228
 defb 132,2,168
 defb 2,238,228
 defb 164,2,34
 defb 2,46,228
 defb 4,2,0
 defb 2,0,4
 defb 252,3,255
testspr08
 defb 1,255,254
 defb 2,1,0
 defb 1,39,114
 defb 18,1,97
 defb 1,39,114
 defb 18,1,36
 defb 1,119,114
 defb 2,1,0
 defb 1,87,114
 defb 66,1,84
 defb 1,119,114
 defb 82,1,17
 defb 1,23,114
 defb 2,1,0
 defb 1,0,2
 defb 254,1,255

testsprv201
 defb 0,0,0
 defb 0,127,254
 defb 64,2,0
 defb 0,64,2
 defb 64,2,0
 defb 0,64,2
 defb 64,2,0
 defb 0,64,2
 defb 64,2,0
 defb 0,64,2
 defb 64,2,0
 defb 0,64,2
 defb 64,2,0
 defb 0,64,2
 defb 127,254,0
 defb 0,0,0
testsprv202
 defb 0,0,0
 defb 0,63,255
 defb 32,1,0
 defb 0,32,1
 defb 32,1,0
 defb 0,32,1
 defb 32,1,0
 defb 0,32,1
 defb 32,1,0
 defb 0,32,1
 defb 32,1,0
 defb 0,32,1
 defb 32,1,0
 defb 0,32,1
 defb 63,255,0
 defb 0,0,0
testsprv203
 defb 0,0,0
 defb 128,31,255
 defb 16,0,128
 defb 128,16,0
 defb 16,0,128
 defb 128,16,0
 defb 16,0,128
 defb 128,16,0
 defb 16,0,128
 defb 128,16,0
 defb 16,0,128
 defb 128,16,0
 defb 16,0,128
 defb 128,16,0
 defb 31,255,128
 defb 0,0,0
testsprv204
 defb 0,0,0
 defb 192,15,255
 defb 8,0,64
 defb 64,8,0
 defb 8,0,64
 defb 64,8,0
 defb 8,0,64
 defb 64,8,0
 defb 8,0,64
 defb 64,8,0
 defb 8,0,64
 defb 64,8,0
 defb 8,0,64
 defb 64,8,0
 defb 15,255,192
 defb 0,0,0
testsprv205
 defb 0,0,0
 defb 224,7,255
 defb 4,0,32
 defb 32,4,0
 defb 4,0,32
 defb 32,4,0
 defb 4,0,32
 defb 32,4,0
 defb 4,0,32
 defb 32,4,0
 defb 4,0,32
 defb 32,4,0
 defb 4,0,32
 defb 32,4,0
 defb 7,255,224
 defb 0,0,0
testsprv206
 defb 0,0,0
 defb 240,3,255
 defb 2,0,16
 defb 16,2,0
 defb 2,0,16
 defb 16,2,0
 defb 2,0,16
 defb 16,2,0
 defb 2,0,16
 defb 16,2,0
 defb 2,0,16
 defb 16,2,0
 defb 2,0,16
 defb 16,2,0
 defb 3,255,240
 defb 0,0,0
testsprv207
 defb 0,0,0
 defb 248,1,255
 defb 1,0,8
 defb 8,1,0
 defb 1,0,8
 defb 8,1,0
 defb 1,0,8
 defb 8,1,0
 defb 1,0,8
 defb 8,1,0
 defb 1,0,8
 defb 8,1,0
 defb 1,0,8
 defb 8,1,0
 defb 1,255,248
 defb 0,0,0
testsprv208
 defb 0,0,0
 defb 252,0,255
 defb 0,128,4
 defb 4,0,128
 defb 0,128,4
 defb 4,0,128
 defb 0,128,4
 defb 4,0,128
 defb 0,128,4
 defb 4,0,128
 defb 0,128,4
 defb 4,0,128
 defb 0,128,4
 defb 4,0,128
 defb 0,255,252
 defb 0,0,0

testspr161601
 defb %11111111,%11111111
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %11111111,%11111111

testspr081601
 defb %11111111,%11111111
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %10000000,%00000001
 defb %11111111,%11111111

STACK ; workaround as stack was overwriting buffer
 DEFS 4096,0

MEMTOP
 DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.
Zeus_PC EQU START  ; Tell the emulator where to start.
Zeus_SP EQU MEMTOP ; Tell the emulator where to put the stack.

;PLOT ; old
; LD H, HIGH ScrBufH     ; SCREEN V TABLE                       #7
; LD D,(HL)              ; L=VPOS                                       #7 14
; INC H                                 ; ScrBufL                                       #4 18
; LD E,(HL)              ; DE = SCREEN POS                      #7 25

; LD L,C                 ; C=HPOS                                       #4 29
; INC H                                 ; ScrBufY                                       #4 33

; LD L, (HL)             ; HOR BYTE POS                                 #7 40
; LD H,0                 ; CLEAR                                        #7 47
; ADD HL,DE              ; SCREEN POS + HOR BYTE POS #11 58

; LD B, HIGH ScrBufOR    ; SCREEN OR TABLE                      #7 65
; LD A, (BC)                    ;                                                       #7 72
; OR (HL)                               ;                                                       #7 79
; LD (HL), A                            ;                                                       #7 86
;RET

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

