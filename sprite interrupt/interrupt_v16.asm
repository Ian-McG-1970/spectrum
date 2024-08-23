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

BORDER MACRO ()
 OUT (254),A
MEND

                org $8200

                di
                push ix
                push iy
                ld (basic_sp+1),sp

                ; create the interrupt vector table
                ld hl,$8000
                ld de,$8001
                ld bc,$100
                ld (hl),$82
                ldir

                ; set interrupt mode 2 with vector register pointing to new vector table
                ld a,$80
                ld i,a
                im 2

                in a,(254)
                cpl
                and $1f
                jr nz, main

                ei

main:           halt
                jp main

                org $8282
inthandler:

 LD A, 1
 OUT (254),A

 LD E,10
 LD C,10
; CALL CLEAR_0816

 LD E,21
 LD C,21
; CALL CLEAR_0816

 LD E,32
 LD C,32
; CALL CLEAR_0816

 LD E,43
 LD C,43
; CALL CLEAR_0816

 LD E,54
 LD C,54
; CALL CLEAR_0816

 LD E,65
 LD C,65
; CALL CLEAR_0816

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 CALL CLEAR_0816

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 CALL CLEAR_0816

 CALL MOVE_POINTS

 LD E,10
 LD C,10
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD E,21
 LD C,21
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD E,32
 LD C,32
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD E,43
 LD C,43
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD E,54
 LD C,54
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD E,65
 LD C,65
 LD HL,SP16160300
; CALL SPRITE_AND_OR_0816

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SP16160300
 CALL SPRITE_AND_OR_0816

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SP16160300
 CALL SPRITE_AND_OR_0816

 LD A, 2
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
stty defb 17
endx defb 95
endy defb 107
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

CLEAR_0816      PROC
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

SPRITE_AND_OR_0816      PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
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

EVEN:                   LD  (STACK_EVEN+1), SP     ; store sp
                        LD  SP, HL                          ; SP = SPRITE ADDRESS

                        LD  H, HIGH ScrBufY                 ; HOR BYTES
                        LD  L, E                            ; HOR
                        LD  B, (HL)                         ; C=HOR BYTE POS

                        SPR_LINE_EVEN () ; 0
                        SPR_AND_OR_LONG_EVEN ()
                        INC C

                        SPR_LINE_EVEN () ; 2
                        SPR_AND_OR_LONG_EVEN ()
                        INC C

                        SPR_LINE_EVEN () ; 4
                        SPR_AND_OR_LONG_EVEN ()
                        INC C

                        SPR_LINE_EVEN () ; 6
                        SPR_AND_OR_LONG_EVEN ()

STACK_EVEN              LD SP, $0000
                        RET

ODD:                    LD (STACK_ODD+1), SP ; store sp
                        LD SP, HL                     ; SP = SPRITE ADDRESS

                        LD H, HIGH ScrBufY            ; HOR BYTES
                        LD L, E                        ; HOR
                        LD B,(HL)                     ; C=HOR BYTE POS

                        SPR_LINE_ODD () ; 0
                        SPR_AND_OR_WORD_ODD_START ()
                        INC C

                        SPR_LINE_EVEN () ; 1
                        INC L
                        SPR_AND_OR_LONG_ODD ()
                        INC C

                        SPR_LINE_EVEN () ; 3
                        INC L
                        SPR_AND_OR_LONG_ODD ()
                        INC C

                        SPR_LINE_EVEN () ; 5
                        INC L
                        SPR_AND_OR_LONG_ODD ()
                        INC C

                        SPR_LINE_EVEN () ; 7
                        INC L
                        SPR_AND_OR_WORD_ODD_END ()

STACK_ODD               LD SP, $0000
                        RET
                        ENDP

SPR_OR_LONG_EVEN MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L               ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L       ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 INC    L               ; NEXT HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_OR_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR

 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 OR     E       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPRITE_OR_0816      PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
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
                    SPR_OR_WORD_ODD_END ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

SPR_PUT_LONG_EVEN MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), E ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L               ; PREV HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

SPR_PUT_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L       ; NEXT HOR
 LD     (HL), E ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; PREV HOR
 LD     (HL), A ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

SPR_PUT_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), D ; PUT SCREEN BYTE
 DEC    L       ; PREV HOR
 LD     (HL), E ; PUT SCREEN BYTE
MEND

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
                    JP C, ODD

EVEN:               LD  (STACK_EVEN+1), SP     ; store sp
                    LD  SP, HL                          ; SP = SPRITE ADDRESS

                    LD  H, HIGH ScrBufY                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_PUT_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 2
                    SPR_PUT_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 4
                    SPR_PUT_LONG_EVEN ()
                    INC C

                    SPR_LINE_EVEN () ; 6
                    SPR_PUT_LONG_EVEN ()

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
                    SPR_PUT_WORD_ODD_END ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP

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

SP16160300 DEFW SP16160301,SP16160302,SP16160303,SP16160304,SP16160305,SP16160306,SP16160307,SP16160308

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

; mask , sprite
SP16160301
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111

SP16160302
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111

SP16160303
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111

SP16160304
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111

SP16160305
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111

SP16160306
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111

SP16160307
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011

SP16160308
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001

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
