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

; TODO - MAKE 2 ATTRIBS AT EITHER SIDE BLACK

 CALL BUILD_LANDSCAPE
 ld hl,0
 ld (landscape_pos),hl
 CALL DRAW_LANDSCAPE

                ei

main:           halt
                jp main

                org $8282
inthandler:

 ld hl,(landscape_pos)
 inc hl
 ld (landscape_pos),hl

; LD A, 2
; OUT (254),A

 LD B, 200
 CALL PAUSE

 LD A, 2
 OUT (254),A
 CALL CLEAR_LANDSCAPE

 LD A, 1
 OUT (254),A
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

 LD A, 3
 OUT (254),A
 ld hl,(landscape_pos)

 CALL DRAW_LANDSCAPE

 LD A, 4
 OUT (254),A
 CALL MOVE_POINTS

 LD A, 6
 OUT (254),A

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
; LD HL,SPAO08160100
; CALL SPRITE_AND_OR_0816
 LD HL,SPO08160200
 CALL SPRITE_OR_0816
; CALL SPRITE_PUT_0816

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
; LD HL,SPAO08160100         ; SPO08160200
; CALL SPRITE_AND_OR_0816
 LD HL,SPO08160200
 CALL SPRITE_OR_0816
; CALL SPRITE_PUT_0816


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
 LD     (HL), E ; PUT SCREEN BYTE
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     (HL), E ; PUT SCREEN BYTE
 DEC    L               ; PREV HOR
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

; ld sp,ix ;10
; ld sp,iy ;10

DRAW_LANDSCAPE_BYTE MACRO (ADDR_1, ADDR_2, ADDR_3, ADDR_4)

        POP     HL                      ; get screen address of point   #10 10
        ADD     HL, BC                  ; add horizontal pos            #11 21
        LD      (HL), %00000011         ; #10                               31   ; ld 1st
; ld (hl), D ; 7
        LD      (ADDR_1+1),HL        ; STORE IN ERASE LANDSCAPE

        POP     HL                      ; get screen address of point   #10 10
        ADD     HL, BC                  ; add horizontal pos            #11 21
        LD      (HL), %00001100         ; #10                               31   ; ld 2nd
; ld (hl), E ; 7
        LD      (ADDR_2+1),HL        ; STORE IN ERASE LANDSCAPE

        POP     HL                      ; get screen address of point   #10 10
        ADD     HL, BC                  ; add horizontal pos            #11 21
        LD      A, (HL)                 ; #7                                ; or 3rd
        OR      D            ; #4
; OR IXL ; 8
        LD      (HL), A                 ; #7
        LD      (ADDR_3+1),HL        ; STORE IN ERASE LANDSCAPE

        POP     HL                      ; get screen address of point   #10 10
        ADD     HL, BC                  ; add horizontal pos            #11 21
        LD      A, (HL)                 ; #7                                ; or 4th
        OR      E            ; #4
; OR IXH ; 8
        LD      (HL), A                 ; #7
        LD      (ADDR_4+1),HL        ; STORE IN ERASE LANDSCAPE
MEND

landscape_pos dw 0

DRAW_LANDSCAPE  PROC
                LD      (STACK+1), SP ; store sp

 ld sp,LANDSCAPE_HEIGHT+128
 add hl,hl
 add hl,sp
                LD      SP, HL                          ; SP = landscape address

 LD DE, %0011000011000000

                LD      BC, 29
                DRAW_LANDSCAPE_BYTE (ADDRESS_29_1, ADDRESS_29_2, ADDRESS_29_3, ADDRESS_29_4)
                DEC     C ; 28
                DRAW_LANDSCAPE_BYTE (ADDRESS_28_1, ADDRESS_28_2, ADDRESS_28_3, ADDRESS_28_4)
                DEC      C ; 27
                DRAW_LANDSCAPE_BYTE (ADDRESS_27_1, ADDRESS_27_2, ADDRESS_27_3, ADDRESS_27_4)
                DEC      C ; 26
                DRAW_LANDSCAPE_BYTE (ADDRESS_26_1, ADDRESS_26_2, ADDRESS_26_3, ADDRESS_26_4)
                DEC      C ;, 25
                DRAW_LANDSCAPE_BYTE (ADDRESS_25_1, ADDRESS_25_2, ADDRESS_25_3, ADDRESS_25_4)
                DEC      C ;, 24
                DRAW_LANDSCAPE_BYTE (ADDRESS_24_1, ADDRESS_24_2, ADDRESS_24_3, ADDRESS_24_4)
                DEC      C ;, 23
                DRAW_LANDSCAPE_BYTE (ADDRESS_23_1, ADDRESS_23_2, ADDRESS_23_3, ADDRESS_23_4)
                DEC      C ;, 22
                DRAW_LANDSCAPE_BYTE (ADDRESS_22_1, ADDRESS_22_2, ADDRESS_22_3, ADDRESS_22_4)
                DEC      C ;, 21
                DRAW_LANDSCAPE_BYTE (ADDRESS_21_1, ADDRESS_21_2, ADDRESS_21_3, ADDRESS_21_4)
                DEC      C ;, 20
                DRAW_LANDSCAPE_BYTE (ADDRESS_20_1, ADDRESS_20_2, ADDRESS_20_3, ADDRESS_20_4)
                DEC      C ;, 19
                DRAW_LANDSCAPE_BYTE (ADDRESS_19_1, ADDRESS_19_2, ADDRESS_19_3, ADDRESS_19_4)
                DEC      C ;, 18
                DRAW_LANDSCAPE_BYTE (ADDRESS_18_1, ADDRESS_18_2, ADDRESS_18_3, ADDRESS_18_4)
                DEC      C ;, 17
                DRAW_LANDSCAPE_BYTE (ADDRESS_17_1, ADDRESS_17_2, ADDRESS_17_3, ADDRESS_17_4)
                DEC      C ;, 16
                DRAW_LANDSCAPE_BYTE (ADDRESS_16_1, ADDRESS_16_2, ADDRESS_16_3, ADDRESS_16_4)
                DEC      C ;, 15
                DRAW_LANDSCAPE_BYTE (ADDRESS_15_1, ADDRESS_15_2, ADDRESS_15_3, ADDRESS_15_4)
                DEC      C ;, 14
                DRAW_LANDSCAPE_BYTE (ADDRESS_14_1, ADDRESS_14_2, ADDRESS_14_3, ADDRESS_14_4)
                DEC      C ;, 13
                DRAW_LANDSCAPE_BYTE (ADDRESS_13_1, ADDRESS_13_2, ADDRESS_13_3, ADDRESS_13_4)
                DEC      C ;, 12
                DRAW_LANDSCAPE_BYTE (ADDRESS_12_1, ADDRESS_12_2, ADDRESS_12_3, ADDRESS_12_4)
                DEC      C ;, 11
                DRAW_LANDSCAPE_BYTE (ADDRESS_11_1, ADDRESS_11_2, ADDRESS_11_3, ADDRESS_11_4)
                DEC      C ;, 10
                DRAW_LANDSCAPE_BYTE (ADDRESS_10_1, ADDRESS_10_2, ADDRESS_10_3, ADDRESS_10_4)
                DEC      C ;, 9
                DRAW_LANDSCAPE_BYTE (ADDRESS_09_1, ADDRESS_09_2, ADDRESS_09_3, ADDRESS_09_4)
                DEC      C ;, 8
                DRAW_LANDSCAPE_BYTE (ADDRESS_08_1, ADDRESS_08_2, ADDRESS_08_3, ADDRESS_08_4)
                DEC      C ;, 7
                DRAW_LANDSCAPE_BYTE (ADDRESS_07_1, ADDRESS_07_2, ADDRESS_07_3, ADDRESS_07_4)
                DEC      C ;, 6
                DRAW_LANDSCAPE_BYTE (ADDRESS_06_1, ADDRESS_06_2, ADDRESS_06_3, ADDRESS_06_4)
                DEC      C ;, 5
                DRAW_LANDSCAPE_BYTE (ADDRESS_05_1, ADDRESS_05_2, ADDRESS_05_3, ADDRESS_05_4)
                DEC      C ;, 4
                DRAW_LANDSCAPE_BYTE (ADDRESS_04_1, ADDRESS_04_2, ADDRESS_04_3, ADDRESS_04_4)
                DEC      C ;, 3
                DRAW_LANDSCAPE_BYTE (ADDRESS_03_1, ADDRESS_04_2, ADDRESS_04_3, ADDRESS_03_4)
                DEC      C ;, 2
                DRAW_LANDSCAPE_BYTE (ADDRESS_02_1, ADDRESS_02_2, ADDRESS_02_3, ADDRESS_02_4)
STACK           LD      SP, $0000
                RET
                ENDP

CLEAR_LANDSCAPE
        XOR     A

ADDRESS_29_1    LD      (SCREEN), A
ADDRESS_29_2    LD      (SCREEN), A
ADDRESS_29_3    LD      (SCREEN), A
ADDRESS_29_4    LD      (SCREEN), A

ADDRESS_28_1    LD      (SCREEN), A
ADDRESS_28_2    LD      (SCREEN), A
ADDRESS_28_3    LD      (SCREEN), A
ADDRESS_28_4    LD      (SCREEN), A

ADDRESS_27_1    LD      (SCREEN), A
ADDRESS_27_2    LD      (SCREEN), A
ADDRESS_27_3    LD      (SCREEN), A
ADDRESS_27_4    LD      (SCREEN), A

ADDRESS_26_1    LD      (SCREEN), A
ADDRESS_26_2    LD      (SCREEN), A
ADDRESS_26_3    LD      (SCREEN), A
ADDRESS_26_4    LD      (SCREEN), A

ADDRESS_25_1    LD      (SCREEN), A
ADDRESS_25_2    LD      (SCREEN), A
ADDRESS_25_3    LD      (SCREEN), A
ADDRESS_25_4    LD      (SCREEN), A

ADDRESS_24_1    LD      (SCREEN), A
ADDRESS_24_2    LD      (SCREEN), A
ADDRESS_24_3    LD      (SCREEN), A
ADDRESS_24_4    LD      (SCREEN), A

ADDRESS_23_1    LD      (SCREEN), A
ADDRESS_23_2    LD      (SCREEN), A
ADDRESS_23_3    LD      (SCREEN), A
ADDRESS_23_4    LD      (SCREEN), A

ADDRESS_22_1    LD      (SCREEN), A
ADDRESS_22_2    LD      (SCREEN), A
ADDRESS_22_3    LD      (SCREEN), A
ADDRESS_22_4    LD      (SCREEN), A

ADDRESS_21_1    LD      (SCREEN), A
ADDRESS_21_2    LD      (SCREEN), A
ADDRESS_21_3    LD      (SCREEN), A
ADDRESS_21_4    LD      (SCREEN), A

ADDRESS_20_1    LD      (SCREEN), A
ADDRESS_20_2    LD      (SCREEN), A
ADDRESS_20_3    LD      (SCREEN), A
ADDRESS_20_4    LD      (SCREEN), A

ADDRESS_19_1    LD      (SCREEN), A
ADDRESS_19_2    LD      (SCREEN), A
ADDRESS_19_3    LD      (SCREEN), A
ADDRESS_19_4    LD      (SCREEN), A

ADDRESS_18_1    LD      (SCREEN), A
ADDRESS_18_2    LD      (SCREEN), A
ADDRESS_18_3    LD      (SCREEN), A
ADDRESS_18_4    LD      (SCREEN), A

ADDRESS_17_1    LD      (SCREEN), A
ADDRESS_17_2    LD      (SCREEN), A
ADDRESS_17_3    LD      (SCREEN), A
ADDRESS_17_4    LD      (SCREEN), A

ADDRESS_16_1    LD      (SCREEN), A
ADDRESS_16_2    LD      (SCREEN), A
ADDRESS_16_3    LD      (SCREEN), A
ADDRESS_16_4    LD      (SCREEN), A

ADDRESS_15_1    LD      (SCREEN), A
ADDRESS_15_2    LD      (SCREEN), A
ADDRESS_15_3    LD      (SCREEN), A
ADDRESS_15_4    LD      (SCREEN), A

ADDRESS_14_1    LD      (SCREEN), A
ADDRESS_14_2    LD      (SCREEN), A
ADDRESS_14_3    LD      (SCREEN), A
ADDRESS_14_4    LD      (SCREEN), A

ADDRESS_13_1    LD      (SCREEN), A
ADDRESS_13_2    LD      (SCREEN), A
ADDRESS_13_3    LD      (SCREEN), A
ADDRESS_13_4    LD      (SCREEN), A

ADDRESS_12_1    LD      (SCREEN), A
ADDRESS_12_2    LD      (SCREEN), A
ADDRESS_12_3    LD      (SCREEN), A
ADDRESS_12_4    LD      (SCREEN), A

ADDRESS_11_1    LD      (SCREEN), A
ADDRESS_11_2    LD      (SCREEN), A
ADDRESS_11_3    LD      (SCREEN), A
ADDRESS_11_4    LD      (SCREEN), A

ADDRESS_10_1    LD      (SCREEN), A
ADDRESS_10_2    LD      (SCREEN), A
ADDRESS_10_3    LD      (SCREEN), A
ADDRESS_10_4    LD      (SCREEN), A

ADDRESS_09_1    LD      (SCREEN), A
ADDRESS_09_2    LD      (SCREEN), A
ADDRESS_09_3    LD      (SCREEN), A
ADDRESS_09_4    LD      (SCREEN), A

ADDRESS_08_1    LD      (SCREEN), A
ADDRESS_08_2    LD      (SCREEN), A
ADDRESS_08_3    LD      (SCREEN), A
ADDRESS_08_4    LD      (SCREEN), A

ADDRESS_07_1    LD      (SCREEN), A
ADDRESS_07_2    LD      (SCREEN), A
ADDRESS_07_3    LD      (SCREEN), A
ADDRESS_07_4    LD      (SCREEN), A

ADDRESS_06_1    LD      (SCREEN), A
ADDRESS_06_2    LD      (SCREEN), A
ADDRESS_06_3    LD      (SCREEN), A
ADDRESS_06_4    LD      (SCREEN), A

ADDRESS_05_1    LD      (SCREEN), A
ADDRESS_05_2    LD      (SCREEN), A
ADDRESS_05_3    LD      (SCREEN), A
ADDRESS_05_4    LD      (SCREEN), A

ADDRESS_04_1    LD      (SCREEN), A
ADDRESS_04_2    LD      (SCREEN), A
ADDRESS_04_3    LD      (SCREEN), A
ADDRESS_04_4    LD      (SCREEN), A

ADDRESS_03_1    LD      (SCREEN), A
ADDRESS_03_2    LD      (SCREEN), A
ADDRESS_03_3    LD      (SCREEN), A
ADDRESS_03_4    LD      (SCREEN), A

ADDRESS_02_1    LD      (SCREEN), A
ADDRESS_02_2    LD      (SCREEN), A
ADDRESS_02_3    LD      (SCREEN), A
ADDRESS_02_4    LD      (SCREEN), A

 RET

;landscape is a bitmap with 0 = up / 1 = down - 1024 /8 = 128 bits needed = reuse as buffer for current screen locations to be cleared

ALIGN $100

SPAO08160100    DEFW SPAO08160101,SPAO08160102,SPAO08160103,SPAO08160104,SPAO08160105,SPAO08160106,SPAO08160107,SPAO08160108
SPO08160200     DEFW SPA08160201,SPA08160202,SPA08160203,SPA08160204,SPA08160205,SPA08160206,SPA08160207,SPA08160208

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
SPAO08160101
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111
 DEFB %00000000,%11111111, %11111111,%00000000
 DEFB %11111111,%00000000, %00000000,%11111111

SPAO08160102
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111
 DEFB %10000000,%01111111, %01111111,%10000000
 DEFB %01111111,%10000000, %10000000,%01111111

SPAO08160103
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111
 DEFB %11000000,%00111111, %00111111,%11000000
 DEFB %00111111,%11000000, %11000000,%00111111

SPAO08160104
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111
 DEFB %11100000,%00011111, %00011111,%11100000
 DEFB %00011111,%11100000, %11100000,%00011111

SPAO08160105
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111
 DEFB %11110000,%00001111, %00001111,%11110000
 DEFB %00001111,%11110000, %11110000,%00001111

SPAO08160106
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111
 DEFB %11111000,%00000111, %00000111,%11111000
 DEFB %00000111,%11111000, %11111000,%00000111

SPAO08160107
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011
 DEFB %11111100,%00000011, %00000011,%11111100
 DEFB %00000011,%11111100, %11111100,%00000011

SPAO08160108
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001
 DEFB %11111110,%00000001, %00000001,%11111110
 DEFB %00000001,%11111110, %11111110,%00000001

; sprite
; mask , sprite
SPA08160201
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111
 DEFB %11111111,%00000000
 DEFB %00000000,%11111111

SPA08160202
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111
 DEFB %01111111,%10000000
 DEFB %10000000,%01111111

SPA08160203
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111
 DEFB %00111111,%11000000
 DEFB %11000000,%00111111

SPA08160204
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111
 DEFB %00011111,%11100000
 DEFB %11100000,%00011111

SPA08160205
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111
 DEFB %00001111,%11110000
 DEFB %11110000,%00001111

SPA08160206
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111
 DEFB %00000111,%11111000
 DEFB %11111000,%00000111

SPA08160207
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011
 DEFB %00000011,%11111100
 DEFB %11111100,%00000011

SPA08160208
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001
 DEFB %00000001,%11111110
 DEFB %11111110,%00000001


BUILD_LANDSCAPE PROC

; for x eq 1 to 256
; get byte
;  for y eq 1 to 8
;   shift byte left
;   if carry set
;    dec vert
;   else
;    inc vert
;   move vert to temp vert
;   shift termp vert right
;   if carry set
;    ld odd high byte
;   else
;    ld even high byte
;   lookup high vert pos
;   store hl
;   lookup low vert pos
;   store hl

        LD      HL, LANDSCAPE_BITMAP    ; source in bits
        LD      DE, LANDSCAPE_HEIGHT    ; destination in words
        LD      B, 0 ; 256 bytes to loop through
        LD      A, 96 ; start at middle vert
        EX      AF, AF' ; backup current vert

O_LOOP          PUSH    BC
                PUSH    HL
                LD      B, (HL)         ; get next byte
                LD      C,8             ; 8 bits of byte to loop through

I_LOOP                  EX      AF, AF' ; restore current vert
                        SRL     B       ; shift next bit into carry
                        JR C, V_INC     ; check next bit
V_DEC                           DEC     A       ; dec current vert
                                JR      I_CONT  ; continue
V_INC                           INC     A       ; inc current ver
I_CONT                  LD      L, A    ; current vert
                        EX      AF, AF' ; backup current vert
                        SRL     L       ; odd or even vert pos
                        JR C, ODD
EVEN                            LD H, HIGH ScrBufEvenL
                                JR V_CONT               ; continue
ODD                             LD H, HIGH ScrBufOddL
V_CONT                  LD A, (HL)         ; SCREEN LO
                        LD     (DE), A     ; store in landscape
                        INC    DE
                        DEC H              ;                            #4 22
                        LD A, (HL)         ; SCREEN HI
                        LD     (DE), A     ; store in landscape
                        INC    DE

                        DEC     C       ; next bit
                        JR NZ, I_LOOP

                POP     HL
                POP     BC              ; counter
                INC     HL              ; next byte pos
                DEC     B               ; next byte
                JP NZ, O_LOOP

;                ld      d, h    ; Make a copy of the first 256 diffs to wrap around
;                ld      e, l
;                LD      HL, LANDSCAPE_HEIGHT
;                ld      bc, 512
;                ldir

        RET
                ENDP

;SPR_LINE_EVEN MACRO ()
; LD H, HIGH ScrBufEvenL     ; SCREEN V TABLE LO  #7 7
; LD L, C            ; VPOS                       #4 11
; LD A, (HL)         ; LO BYTE POS                #7 18
; DEC H              ;                            #4 22
; LD H, (HL)         ; SCREEN V TABLE HI          #7 29
; ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
; LD L, A            ;                            #4 37
;MEND

;SPR_LINE_ODD MACRO ()
; LD H, HIGH ScrBufOddL     ; SCREEN V TABLE LO   #7 7
; LD L, C            ; VPOS                       #4 11
; LD A, (HL)         ; LO BYTE POS                #7 18
; DEC H              ;                            #4 22
; LD H, (HL)         ; SCREEN V TABLE HI          #7 29
; ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
; LD L, A            ;                            #4 37
;MEND

ALIGN $100

LANDSCAPE_BITMAP
        DB %00000000,%11111111,%00000000,%11111111,%11111111,%11111111,%00000000,%11111111
        DB %00000000,%00000000,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%00000000,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%11111111,%11111111
        DB %00000000,%11111111,%00000000,%00000000,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%11111111,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%11111111,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%00000000,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%00000000
        DB %00000000,%11111111,%00000000,%11111111,%11111111,%11111111,%00000000,%11111111
        DB %00000000,%00000000,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %11111111,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%00000000,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111
        DB %00000000,%11111111,%00000000,%11111111,%00000000,%11111111,%00000000,%11111111

LANDSCAPE_HEIGHT equ (59*1024)+512 ; landscape = 8 screens + 1 screen = 8 screens = (8*256)*2 = 4096 + 1 screen = (1*256) =512

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
