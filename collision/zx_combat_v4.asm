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

 LD a, (sttx)
 ld l,a
 LD a, (endy)
 ld c,a
 CALL PLOT

 LD a, (pv)
 ld c,a
 LD a, (ph)
 ld e,a
 LD HL,combatspr00
 CALL SPRITE_XOR_1224
 
 ld c,40
 ld e,40
 LD HL,combatspr00
 CALL SPRITE_XOR_1224

 ld c,61
 ld e,61
 LD HL,squarespr00
 CALL SPRITE_XOR_1224

 LD A,22
 LD E,30
 LD HL,CHAR080801
 CALL SPRITE_PUT_BLOCK_0808

 LD A,18
 LD C,28
 LD HL,CHAR161601
 CALL SPRITE_PUT_BLOCK_1616

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

 BORDER (7)

MAIN_LOOP
 CALL V_BLANK

 BORDER (5)
 LD a, (sttx)
 ld l,a
 LD a, (endy)
 ld c,a
 CALL PLOT

 LD a, (pv)
 ld c,a
 LD a, (ph)
 ld e,a
 LD HL,combatspr00
 CALL SPRITE_XOR_1224

 BORDER (3)
 CALL MOVE_POINTS

 BORDER (5)
 LD a, (sttx)
 ld l,a
 LD a, (endy)
 ld c,a
 CALL PLOT

 BORDER (7)
 CALL KEYBOARD

 LD a, (pv)
 ld c,a
 LD a, (ph)
 ld e,a
 LD HL,combatspr00
 CALL SPRITE_XOR_1224_CLD

 LD A,IXH
 LD IY,$0008
 CALL HEX8

 BORDER (3)
JP MAIN_LOOP

sttx defb 12
stty defb 20
endx defb 95
endy defb 17
sdx defb 1
sdy defb 0 ;1
edx defb 0 ;1
edy defb 1

pv defb 100
ph defb 150

MEMSET  PROC
        ld (hl), a
        push hl
        pop de
; ld de, hl
        inc de
        ldir
        RET
        ENDP

; keys = port bit 0   1   2 3 4
;        fefe     sht z   x c v
;        fdfe     a   s   d f g
;        fbfe     q   w   e r t
;        f7fe     1   2   3 4 5
;        effe     0   9   8 7 6
;        dffe     p   o   1 u y
;        bffe     ent l   k j h
;        7ffe     spc sym m n b

KEYBOARD PROC
 LD BC,$FBFE     ; Load BC with the row port address
 IN A,(C)        ; Read the port into the accumulator
 AND $02         ; W
 JP NZ,W_KEY_N   ; not pressed
  LD A, (pv)
  DEC A
  LD (pv), A
W_KEY_N
 LD BC,$FDFE
 IN A,(C)
 AND $02
 JP NZ,S_KEY_N
  LD A, (pv)
  INC A
  LD (pv), A
S_KEY_N
  LD BC,$FDFE
 IN A,(C)
 AND $01
 JP NZ,A_KEY_N
  LD A, (ph)
  DEC A
  LD (ph), A
A_KEY_N
 LD BC,$FDFE
 IN A,(C)
 AND $04
 JP NZ,D_KEY_N
  LD A, (ph)
  INC A
  LD (ph), A
D_KEY_N
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

; todo - change to use IYL instead of B and use B as temp var in main code

SPR_LINE_EVEN_CLD MACRO ()
 LD H, HIGH ScrBufEvenL     ; SCREEN V TABLE LO  #7 7
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18
 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, IYL ; B          ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND


; todo - change to use IYL instead of B and use B as temp var in main code

SPR_LINE_ODD_CLD MACRO ()
 LD H, HIGH ScrBufOddL     ; SCREEN V TABLE LO   #7 7
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18
 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, IYL ; B           ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND


; perfect collision detection
; 1 get sprite line					11100000			; POP    DE      ; GRAPHIC
; 2 get screen						00000111			; LD     A, (HL) ; get screen byte
;  2.1 backup screen for 5
; 3 result1 = xor sprite and screen	11100111			; XOR    E
; 4 put result1 to new screen				11100111			; LD     (HL), A ; PUT SCREEN BYTE
; 5 result2 = and original screen and result1	00000111			;
; 6 compare original screen + result2			00000111 00000111
;  6.1 xor original screen + result2 
; 7 if different then collision? no
;  7.1 or result with total result

SPR_XOR_LONG_EVEN_24_CLD MACRO ()

 POP    DE      ; GRAPHIC			; ; 1 get sprite line	

 LD     A, (HL) ; get screen byte	; 2 get screen
 LD b, A		; backup			;  2.1 backup screen for 5
 XOR    E							; 3 result1 = xor sprite and screen
 LD     (HL), A ; PUT SCREEN BYTE	; 4 put result1 to screen
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result
 
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte	; 2 get screen
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D							; 3 result1 = xor sprite and screen
 LD     (HL), A ; PUT SCREEN BYTE	; 4 put result1 to screen
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    H       ; NEXT VER
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result
MEND

SPR_XOR_WORD_ODD_24_CLD MACRO ()
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR

 POP    DE      ; get right and below
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

MEND

SPR_XOR_LONG_ODD_RIGHT_24_CLD MACRO ()
 EX AF,AF'
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR
 EX AF,AF'

 ld d,a
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT right
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    H       ; NEXT VER

 POP    DE      ; get left and middle
 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 DEC    L       ; PREV HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 POP    DE      ; get right and below

 INC    L       ; NEXT HOR
 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

MEND

SPR_XOR_WORD_ODD_RIGHT_24_CLD MACRO ()
 EX AF,AF'
 POP    DE      ; get left and middle
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    E
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT SCREEN BYTE
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    L       ; NEXT HOR
 EX AF,AF'
 ld d,a
 LD     A, (HL) ; get screen byte
 LD B, A		; backup			;  2.1 backup screen for 5
 XOR    D
 LD     (HL), A ; PUT right
 AND	A, B						; 5 result2 = original screen and result1
 XOR	A, B							; 6.1 xor original screen + result2
 OR		A, IXH						;  7.1 or result with total result
 LD 	IXH, A						;  store total result

 INC    H       ; NEXT VER
MEND

SPRITE_XOR_1224_CLD PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                    LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                    ADD A, L                                ; add it to the original start position                                 #4
                    LD L, A                                 ; and move it back to HL                                #4

                    LD A, (HL)              ; SPRITE ADDRESS LO
                    INC L
                    LD H, (HL)              ; SPRITE ADDRESS HI
                    LD L, A

	ld IXH, 0

                    SRL C
                    JP C, SPR_ODD

SPR_EVEN:           LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS
					LD  IYL, B ; (HL)
					
                    SPR_LINE_EVEN_CLD () ; 0
                    SPR_XOR_LONG_EVEN_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 2
                    SPR_XOR_LONG_EVEN_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 4
                    SPR_XOR_LONG_EVEN_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 6
                    SPR_XOR_LONG_EVEN_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 8
                    SPR_XOR_LONG_EVEN_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 10
                    SPR_XOR_LONG_EVEN_24_CLD ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B, (HL)                     ; C=HOR BYTE POS
					LD  IYL, B ; (HL)

                    SPR_LINE_ODD_CLD () ; 0
                    SPR_XOR_WORD_ODD_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 1
                    LD A,D                                                  ; backup below
                    SPR_XOR_LONG_ODD_RIGHT_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 3
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 5
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 7
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 9
                    LD A,D
                    SPR_XOR_LONG_ODD_RIGHT_24_CLD ()
                    INC C

                    SPR_LINE_EVEN_CLD () ; 11
                    LD A,D
                    SPR_XOR_WORD_ODD_RIGHT_24_CLD ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPRITE_BLOCK_0802 MACRO ()
        POP DE
        LD (HL), E
        INC H
        LD (HL), D
ENDM

SPRITE_PUT_BLOCK_0808   PROC ; A=VER CHAR E=HOR CHAR ; HL=BLOCK ADDRESS
                                                LD (STACK_0808+1), SP
                                                LD SP, HL

 ADD A, A
 ADD A, A
 LD L, A
 LD A, E
 LD H, HIGH ScrBufEvenL
 ADD A, (HL)
 DEC H
 LD H, (HL)
 LD L, A

                                                SPRITE_BLOCK_0802 ()
                                                INC H
                                                SPRITE_BLOCK_0802 ()
                                                INC H
                                                SPRITE_BLOCK_0802 ()
                                                INC H
                                                SPRITE_BLOCK_0802 ()
STACK_0808              LD SP, $0000
                                                RET
                                                ENDP

SPRITE_BLOCK_1602 MACRO ()
        POP DE
        LD (HL), E
        INC L
        LD (HL), D
                INC H
        POP DE
        LD (HL), D
        DEC L
        LD (HL), E
ENDM

SPRITE_PUT_BLOCK_1616   PROC
                                                LD (STACK_1616+1), SP
                        LD SP, HL

 ADD A, A
 ADD A, A
 LD L, A
 EX AF,AF'

 LD A, C
 LD H, HIGH ScrBufEvenL
 ADD A, (HL)
 DEC H
 LD H, (HL)
 LD L, A
                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()

 EX AF,AF'
 ADD A, 4
 LD L, A

 LD A, C
 LD H, HIGH ScrBufEvenL
 ADD A, (HL)
 DEC H
 LD H, (HL)
 LD L, A

                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()
                                                INC H
                                                SPRITE_BLOCK_1602 ()
STACK_1616              LD SP, $0000

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

MOVE_POINTS	PROC
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
				RET
			ENDP

; BC pos dir
; HL limits
EDGE 	PROC
			LD A,   b
			ADD     a, c
			cp      h
			jp z, REVERSE
			cp l
			jp z, REVERSE
			LD b, a
			RET
		ENDP
		
REVERSE	PROC
			LD b, a
			LD a, c
			NEG
			LD c, a
			RET
		ENDP

PLOT PROC ; c=hor l=ver
		LD B, HIGH ScrBufY		;               #7
		LD A, (BC)   			; hor   #7 14

		LD H, HIGH ScrBufL	; verlo    #7 21
		ADD A, (HL)      		;               #7 28
		INC H				    ; verhi #4 32
		LD H, (HL)           	;               #7 39
		LD L, A 		        ;               #4 43

		INC B				    ; or    #4 47
		LD A, (BC)         		;               #7 54
		XOR (HL)            	;               #7 61
		LD (HL), A           	;               #7 68
		RET
	ENDP


; IX=Val
; A=Ver

HEX16	PROC
			LD I, A ; PUSH AF
			LD IYL, A
			LD IYH, 0
			LD A, IXH
			CALL HEX8

			LD A, I ; POP AF
			LD IYL, A
			LD IYH, 2
			LD A, IXL
; JP HEX8       ; ret
		ENDP

; IYL=Ver
; IYH=Hor
; A=Val

HEX8    PROC
			LD C,A
			EX AF,AF'

			LD A, IYL
			ADD A, A        ; *2
			ADD A, IYL      ; *3
			ADD A, A        ; *6
			LD IYL, A
			LD L, A

			LD A, C
			SRL A
			SRL A
			SRL A
			SRL A
			LD C, A

			CALL HEXCHAR

			INC IYH

			LD A, IYL
			LD L, A

			EX AF, AF'
			AND A, $0F
			LD C, A
; JP HEXCHAR    ; ret
		ENDP

; L=Ver
; IXL=Hor
; C=Val

HEXCHAR PROC
			LD A, C
			ADD A, A ; *2
			ADD A, A ; *4
			ADD A, C ; *5
			LD C, A

			LD H, HIGH ScrBufH     ; SCREEN V TABLE
			LD B, HIGH HexChar

			LD D, (HL)              ; L=VPOS
			INC H    ; ScrBufL
			LD A, IYH
			ADD A, (HL)              ; DE = SCREEN POS
			LD E, A
			LD A, (BC)
			LD (DE), A
			INC C

			DEC H ; ScrBufH
			INC L
			LD D, (HL)
			INC H
			LD A, IYH
			ADD A, (HL)
			LD E, A
			LD A, (BC)
			LD (DE), A
			INC C

			DEC H
			INC L
			LD D, (HL)
			INC H
			LD A, IYH
			ADD A, (HL)
			LD E, A
			LD A, (BC)
			LD (DE), A
			INC C

			DEC H
			INC L
			LD D, (HL)
			INC H
			LD A, IYH
			ADD A, (HL)
			LD E, A
			LD A, (BC)
			LD (DE), A
			INC C

			DEC H
			INC L
			LD D, (HL)
			INC H
			LD A, IYH
			ADD A, (HL)
			LD E, A
			LD A, (BC)
			LD (DE), A
			RET
		ENDP

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

Hex0    DB 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1    DB 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2    DB 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3    DB 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4    DB 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

ALIGN $100
combatspr00 defw combatspr01,combatspr02,combatspr03,combatspr04,combatspr05,combatspr06,combatspr07,combatspr08
squarespr00 defw squarespr01,squarespr02,squarespr03,squarespr04,squarespr05,squarespr06,squarespr07,squarespr08

ALIGN $100
ScrBufL 
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255
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
ScrBufH 
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256
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
ScrBufOR
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1
 DEFB 128,64,32,16,8,4,2,1

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

CHAR080801
 DEFB %11111111
 DEFB %10000001
 DEFB %10111101
 DEFB %10100101
 DEFB %10100101
 DEFB %10111101
 DEFB %10000001
 DEFB %11111111

CHAR161601
 DEFB %11111111,%11111111
 DEFB %10000000,%00000001
 DEFB %10111111,%11111101
 DEFB %10100000,%00000101
 DEFB %10101111,%11110101
 DEFB %10101000,%00010101
 DEFB %10101011,%11010101
 DEFB %10101010,%01010101
 DEFB %10101010,%01010101
 DEFB %10101011,%11010101
 DEFB %10101000,%00010101
 DEFB %10101111,%11110101
 DEFB %10100000,%00000101
 DEFB %10111111,%11111101
 DEFB %10000000,%00000001
 DEFB %11111111,%11111111

combatspr
combatspr01
 defb 15,0,0
 defb 0,48,192
 defb 64,32,0
 defb 0,64,32
 defb 128,16,0
 defb 0,134,16
 defb 134,16,0
 defb 0,128,16
 defb 64,32,0
 defb 0,64,32
 defb 48,192,0
 defb 0,15,0
combatspr02
 defb 7,128,0
 defb 0,24,96
 defb 32,16,0
 defb 0,32,16
 defb 64,8,0
 defb 0,67,8
 defb 67,8,0
 defb 0,64,8
 defb 32,16,0
 defb 0,32,16
 defb 24,96,0
 defb 0,7,128
combatspr03
 defb 3,192,0
 defb 0,12,48
 defb 16,8,0
 defb 0,16,8
 defb 32,4,0
 defb 0,33,132
 defb 33,132,0
 defb 0,32,4
 defb 16,8,0
 defb 0,16,8
 defb 12,48,0
 defb 0,3,192
combatspr04
 defb 1,224,0
 defb 0,6,24
 defb 8,4,0
 defb 0,8,4
 defb 16,2,0
 defb 0,16,194
 defb 16,194,0
 defb 0,16,2
 defb 8,4,0
 defb 0,8,4
 defb 6,24,0
 defb 0,1,224
combatspr05
 defb 0,240,0
 defb 0,3,12
 defb 4,2,0
 defb 0,4,2
 defb 8,1,0
 defb 0,8,97
 defb 8,97,0
 defb 0,8,1
 defb 4,2,0
 defb 0,4,2
 defb 3,12,0
 defb 0,0,240
combatspr06
 defb 0,120,0
 defb 0,1,134
 defb 2,1,0
 defb 0,2,1
 defb 4,0,128
 defb 128,4,48
 defb 4,48,128
 defb 128,4,0
 defb 2,1,0
 defb 0,2,1
 defb 1,134,0
 defb 0,0,120
combatspr07
 defb 0,60,0
 defb 0,0,195
 defb 1,0,128
 defb 128,1,0
 defb 2,0,64
 defb 64,2,24
 defb 2,24,64
 defb 64,2,0
 defb 1,0,128
 defb 128,1,0
 defb 0,195,0
 defb 0,0,60
combatspr08
 defb 0,30,0
 defb 128,0,97
 defb 0,128,64
 defb 64,0,128
 defb 1,0,32
 defb 32,1,12
 defb 1,12,32
 defb 32,1,0
 defb 0,128,64
 defb 64,0,128
 defb 0,97,128
 defb 0,0,30

squarespr01
 defb 255,240,0
 defb 0,128,16
 defb 191,208,0
 defb 0,160,80
 defb 175,80,0
 defb 0,169,80
 defb 169,80,0
 defb 0,175,80
 defb 160,80,0
 defb 0,191,208
 defb 128,16,0
 defb 0,255,240
squarespr02
 defb 127,248,0
 defb 0,64,8
 defb 95,232,0
 defb 0,80,40
 defb 87,168,0
 defb 0,84,168
 defb 84,168,0
 defb 0,87,168
 defb 80,40,0
 defb 0,95,232
 defb 64,8,0
 defb 0,127,248
squarespr03
 defb 63,252,0
 defb 0,32,4
 defb 47,244,0
 defb 0,40,20
 defb 43,212,0
 defb 0,42,84
 defb 42,84,0
 defb 0,43,212
 defb 40,20,0
 defb 0,47,244
 defb 32,4,0
 defb 0,63,252
squarespr04
 defb 31,254,0
 defb 0,16,2
 defb 23,250,0
 defb 0,20,10
 defb 21,234,0
 defb 0,21,42
 defb 21,42,0
 defb 0,21,234
 defb 20,10,0
 defb 0,23,250
 defb 16,2,0
 defb 0,31,254
squarespr05
 defb 15,255,0
 defb 0,8,1
 defb 11,253,0
 defb 0,10,5
 defb 10,245,0
 defb 0,10,149
 defb 10,149,0
 defb 0,10,245
 defb 10,5,0
 defb 0,11,253
 defb 8,1,0
 defb 0,15,255
squarespr06
 defb 7,255,128
 defb 128,4,0
 defb 5,254,128
 defb 128,5,2
 defb 5,122,128
 defb 128,5,74
 defb 5,74,128
 defb 128,5,122
 defb 5,2,128
 defb 128,5,254
 defb 4,0,128
 defb 128,7,255
squarespr07
 defb 3,255,192
 defb 64,2,0
 defb 2,255,64
 defb 64,2,129
 defb 2,189,64
 defb 64,2,165
 defb 2,165,64
 defb 64,2,189
 defb 2,129,64
 defb 64,2,255
 defb 2,0,64
 defb 192,3,255
squarespr08
 defb 1,255,224
 defb 32,1,0
 defb 1,127,160
 defb 160,1,64
 defb 1,94,160
 defb 160,1,82
 defb 1,82,160
 defb 160,1,94
 defb 1,64,160
 defb 160,1,127
 defb 1,0,32
 defb 224,1,255

STACK ; workaround as stack was overwriting buffer
 DEFS 4096,0

MEMTOP
 DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.
Zeus_PC EQU START  ; Tell the emulator where to start.
Zeus_SP EQU MEMTOP ; Tell the emulator where to put the stack.

; perfect collision detection
; get sprite line					11100000
; get screen						00000111
; result1 = xor sprite and screen	11100111
; result2 = and screen + result1	00000111
; compare screen + result2			00000111 00000111
; if different then collision? no

; get sprite line					11111100
; get screen						00000111
; result1 = xor sprite and screen	11111011
; result2 = and screen + result1	00000011
; compare screen + result2			00000011 00000111
; if different then collision? yes


