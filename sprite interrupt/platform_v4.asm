; Comment the following if you want the output to be a code block that can
; be LOADed across the serial port
snapshot equ true

;-------------
;
; Program code starts at $8000

                org $8000

if def snapshot
        ; output an SZX snapshot file
        output_szx "interrupt.szx", $8200,$8200
        output_bin "interrupt.bin", $8200, ends-$8200
else
        org .-9
        ; output an Interface 1 stream header
        output_bin "interupt.code", ., ends-.
        db 3            ; Type = CODE
        dw ends-.-8     ; Block length
        dw .+6          ; Block origin
        dw $ffff, $ffff ; Variable store and autorun for BASIC programs
endif

export_sym "interrupt.sym",0

SCREEN EQU 16384
ATTRIB EQU $5800

BORDER MACRO ()
 OUT (254),A
MEND

                org $8200

                di
                push ix
                push iy
                ld (basic_sp+1),sp

                ld hl,$8000 ; create the interrupt vector table
                ld de,$8001
                ld bc,$100
                ld (hl),$82
                ldir

                ld a,$80 ; set interrupt mode 2 with vector register pointing to new vector table
                ld i,a
                im 2

                in a,(254)
                cpl
                and $1f
                jr nz, main

 ld bc, (32*24*8)-1
 ld hl, SCREEN
 ld a, 0
 call MEMSET

 ld bc, (32*24)-1
 ld hl, ATTRIB
 ld a, 7
 call MEMSET

                ei

main:           halt
                jp main

                org $8282
inthandler:

; LD A, 2
; OUT (254),A

; LD B, 200
; CALL PAUSE

 LD A, 2
 OUT (254),A

 LD A, 1
 OUT (254),A
; LD A, (stty)
; LD E,A
; LD A, (sttx)
; LD C,A
; CALL CLEAR_1616

; LD A, (endy)
; LD E,A
; LD A, (endx)
; LD C,A
; CALL CLEAR_1616

 LD A, 3
 OUT (254),A

 LD A, 4
 OUT (254),A
 CALL MOVE_POINTS

 LD A, 6
 OUT (254),A

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SPO16160200
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616
 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SPO16160100
; CALL SPRITE_OR_1616
; CALL SPRITE_PUT_1616

 LD A, 48
 LD E,A
 LD A, 72
 LD C,A
 LD HL,SPO16240100
; CALL SPRITE_PUT_1624

 LD A, 74
 LD E,A
 LD A, 72
 LD C,A
 LD HL,SPO16240100
; CALL SPRITE_PUT_1624

 LD A, 120
 LD E,A
 LD A, (endy)
 LD C,A
 LD HL,SPO16240100
 CALL SPRITE_PUT_1624

 LD A, (endy)
 LD E,A
 LD A, 151
 LD C,A
 LD HL,SPO16240100
 CALL SPRITE_PUT_1624

 LD A, 64
 LD E,A
 LD A, 60
 LD C,A
 LD HL,SPO16240100
 CALL SPRITE_PUT_1624

 LD A, 96
 LD E,A
 LD A, 61
 LD C,A
 LD HL,SPO16240100
 CALL SPRITE_PUT_1624

 LD A, 5
 OUT (254),A

 ei             ; enable interrupts and return from ISR
 reti

quit:           di                      ; Restore interrupt mode, stack pointer and index registers before return to BASIC
                ld a,$3f
                ld i,a
                im 1
basic_sp:       ld sp,0
                pop iy
                pop ix
                ei
                ret

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

CLR_LINE_EVEN_ODD MACRO ()
 LD A, (BC)         ; HI BYTE POS                #7 7
 LD H, A            ;                            #4 11
 INC B              ;                            #4 15
 LD A, (BC)         ; SCREEN V TABLE LO          #7 22
 ADD A, E           ; LO BYTE POS + HOR BYTE POS #4 26
 LD L, A            ;                            #4 30
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
                JP  C, ODD

EVEN:           LD B, HIGH ScrBufEvenH
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

ODD:            LD B, HIGH ScrBufOddH
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
                    JP C, ODD

EVEN:               LD  (STACK_EVEN+1), SP     ; store sp
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

ODD:                LD (STACK_ODD+1), SP ; store sp
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
 LD     (HL), E ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE
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

SPR_PUT_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE
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
                    JP C, ODD

EVEN:               LD  (STACK_EVEN+1), SP     ; store sp
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

ODD:                LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH ScrBufY            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_PUT_WORD_ODD_START ()
                    INC C

                    SPR_LINE_EVEN () ; 1
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 3
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 5
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 7
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 9
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 11
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 13
                    INC L
                    SPR_PUT_LONG_ODD ()
                    INC C

                    SPR_LINE_EVEN () ; 15
                    INC L
                    SPR_PUT_WORD_ODD_END ()

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

SPR_PUT_WORD_ODD_24 MACRO ()
 POP    DE      ; get left and middle
 LD     (HL), e ; PUT left
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT middle
 INC    L       ; NEXT HOR

 POP    DE      ; get right and below
 LD     (HL), e ; put right
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

SPR_PUT_WORD_ODD_RIGHT_24 MACRO ()
 POP    DE      ; get left and middle
 LD     (HL), e ; PUT left
 INC    L       ; NEXT HOR
 LD     (HL), d ; PUT middle
 INC    L       ; NEXT HOR
 LD     (HL), A ; put right
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
                                        ld a,d                                                  ; backup below
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 3
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 5
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 7
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 9
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 11
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 13
                                        ld a,d
                    SPR_PUT_LONG_ODD_RIGHT_24 ()
                                        inc c

                    SPR_LINE_EVEN () ; 15
                                        ld a,d
; SPR_PUT_WORD_ODD_RIGHT_24
                    SPR_PUT_WORD_ODD_24 ()

STACK_ODD           LD SP, $0000
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

SPO16160100     DEFW SPO16160101,SPO16160102,SPO16160103,SPO16160104,SPO16160105,SPO16160106,SPO16160107,SPO16160108
SPO16160200     DEFW SPO16160201,SPO16160202,SPO16160203,SPO16160204,SPO16160205,SPO16160206,SPO16160207,SPO16160208

;SPO16240100
                DEFW SPO16240101
SPO16240100
 defw SPO16240111
                DEFW SPO16240102,SPO16240103,SPO16240104,SPO16240105,SPO16240106,SPO16240107,SPO16240108
                DEFW SPO16240109

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

ends:           dw 0

;Added support for "@label" half-arsed local labels. Zeus has procedures to support proper local labels, with nested
;    namespaces, but to help support legacy source-code it will also treat symbols declared with "@" as "local" to the
;    last declared symbol... please use proc/pend instead of this ghastly neolithic bodge in new sources.
;
;      Here is an example of this, but please don't use it in new code.
;
;      Print     push hl
;      @Lp       nop             ; Declares the symbol "Print.Lp"
;                call @Lp        ; Calls "Print.Lp"
;                pop hl
;                ret
;
;      PrintStr  push hl
;      @Lp       nop             ; Declares the symbol "PrintStr.Lp"
;                call @Lp        ; Calls "PrintStr.Lp"
;                pop hl
;                ret
;
;     Instead do this:
;
;     Print      proc
;     Lp         nop             ; Declares "Print.LP"
;                pend            ; or use retp to plant the "RET"
;
;     PrintStr   proc
;     Lp         nop               ; Declares "PrintStr.LP"
;                call PrintStr.Lp  ; Calls the Lp in the procedure Print
;                call Print.Lp     ; Calls our local "Lp"
;                pend

