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

 CALL DELAY

 LD A, 1
 OUT (254),A

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 CALL CLEAR_2124

 LD A, 2
 OUT (254),A

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 CALL CLEAR_2124

 LD A, 3
 OUT (254),A

 CALL MOVE_POINTS

 LD A, 6
 OUT (254),A

 LD A, (stty)
 LD E,A
 LD A, (sttx)
 LD C,A
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 4
 OUT (254),A

 LD A, (endy)
 LD E,A
 LD A, (endx)
 LD C,A
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 3
 OUT (254),A

 LD E,48
 LD C,0
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 7
 OUT (254),A

 LD E,80
 LD C,1
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 0
 OUT (254),A

 LD E,104
 LD C,2
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 2
 OUT (254),A

 LD E,128
 LD C,4
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 3
 OUT (254),A

 LD E,140
 LD C,6
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 3
 OUT (254),A

 LD E,160
 LD C,3
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 4
 OUT (254),A

 LD E,180
 LD C,5
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 0
 OUT (254),A

 LD E,200
 LD C,7
 LD HL,SP21240300
 CALL SPRITE_AND_OR_2124

 LD A, 1
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

DELAY   PROC
        LD B,255
_LOOP:
                LD A, 1
                OUT (254), A
                INC A
                OUT (254), A
                DJNZ _LOOP
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
 LD     (HL), D ; PUT SCREEN BYTE 1ST HOR
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE 2ND HOR
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE 3RD HOR
 INC    H       ; NEXT VER
 LD     (HL), D ; PUT SCREEN BYTE 3RD HOR
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE 2ND HOR
 DEC    L       ; PREV HOR
 LD     (HL), D ; PUT SCREEN BYTE 1ST HOR
MEND

CLR_WORD_EVEN_ODD MACRO ()
 LD     (HL), D ; PUT SCREEN BYTE 1ST HOR
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE 2ND HOR
 INC    L       ; NEXT HOR
 LD     (HL), D ; PUT SCREEN BYTE 3RD HOR
MEND

CLEAR_2124      PROC
                LD  D, HIGH ScrBufY                 ; HOR BYTES
                LD  A, (DE)                         ; C=HOR BYTE POS
                LD  E,A

                LD  D,0

                SRL C
                JP  C, ODD_LINE

EVEN_LINE:      LD B, HIGH ScrBufEvenH
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
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 16
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 18
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 20
                CLR_WORD_EVEN_ODD ()

                RET

ODD_LINE:       LD B, HIGH ScrBufOddH
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
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 17
                DEC B
                CLR_LONG_EVEN_ODD ()
                INC C

                CLR_LINE_EVEN_ODD () ; 19
                CLR_LONG_EVEN_ODD ()
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
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
MEND

SPR_AND_OR_LONG_ODD MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
 DEC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 DEC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
 INC    H       ; NEXT VER

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
 INC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 INC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (1/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
MEND

; l=x pos
; e=y pos
; HL = sprite address (needs to be put into SP)

SPR_AND_OR_WORD_ODD_START MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 INC    L       ; NEXT HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
MEND

; l=x pos
; e=y pos
; HL = sprite address (needs to be put into SP)

SPR_AND_OR_WORD_ODD_END MACRO ()
 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 3RD HOR
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/0)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 2ND HOR
 DEC    L       ; PREV HOR

 POP    DE      ; GRAPHIC (AND/OR)
 LD     A, (HL) ; GET SCREEN BYTE (0/1)
 AND    E       ; AND MASK
 OR     D       ; OR SPRITE
 LD     (HL), A ; PUT SCREEN BYTE 1ST HOR
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

SPR_LINE MACRO ()
 LD H, HIGH ScrBufL     ; SCREEN V TABLE LO  #7 7
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

SPRITE_AND_OR_2124   PROC    ; C=ver - E=hor - HL = sprite table ADDRESS

                        LD D, HIGH ScrBufSprY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
                        LD A, (DE)              ; GET TABLE ADDRESS OFFSET                                              #7

                        ADD A, L                                ; add it to the original start position                                 #4
                        LD L, A                                 ; and move it back to HL                                #4

                                                ld b,e ; backup hor

                        LD E, (HL)              ; SPRITE ADDRESS LO
                        INC L
                        LD D, (HL)              ; SPRITE ADDRESS HI

                                                ld l,c         ; v
                                                ld h, high SPR_LINE_JMP_TAB_LO ; point to table start
                                                ld a,(hl)
                                                inc h
                                                ld h,(hl)
                                                ld l,a
                                                jp (hl) ; jump to odd or even address

        ENDP

EVEN_LINE_0     PROC
                                LD  (STACK_EVEN+1), SP  ; store sp
                                                                ex de,hl                               ; restore sprite address
                                LD  SP, HL              ; SP = SPRITE ADDRESS

                                LD  H, HIGH ScrBufY                 ; HOR BYTES
                                LD  L, B                            ; HOR
                                LD  B, (HL)                         ; C=HOR BYTE POS

                SPR_LINE () ; 0
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 8
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_WORD_ODD_START ()

STACK_EVEN      LD SP, $0000
                                RET
            ENDP

EVEN_LINE_2     PROC
                                LD  (STACK_EVEN+1), SP  ; store sp
                                                                ex de,hl                               ; restore sprite address
                                LD  SP, HL              ; SP = SPRITE ADDRESS

                                LD  H, HIGH ScrBufY                 ; HOR BYTES
                                LD  L, B                            ; HOR
                                LD  B, (HL)                         ; C=HOR BYTE POS

                SPR_LINE () ; 0
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()

                                LD A,6  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 8
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_WORD_ODD_START ()

STACK_EVEN      LD SP, $0000
                                RET
            ENDP

EVEN_LINE_4     PROC
                                LD  (STACK_EVEN+1), SP  ; store sp
                                                                ex de,hl                               ; restore sprite address
                                LD  SP, HL              ; SP = SPRITE ADDRESS

                                LD  H, HIGH ScrBufY                 ; HOR BYTES
                                LD  L, B                            ; HOR
                                LD  B, (HL)                         ; C=HOR BYTE POS

                SPR_LINE () ; 0
                SPR_AND_OR_LONG_EVEN ()
                INC H
                SPR_AND_OR_LONG_EVEN ()

                                LD A,4  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 8
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16

                                SPR_AND_OR_WORD_ODD_START ()

STACK_EVEN      LD SP, $0000
                                RET
            ENDP

EVEN_LINE_6     PROC
                                LD  (STACK_EVEN+1), SP  ; store sp
                                                                ex de,hl                               ; restore sprite address
                                LD  SP, HL              ; SP = SPRITE ADDRESS

                                LD  H, HIGH ScrBufY                 ; HOR BYTES
                                LD  L, B                            ; HOR
                                LD  B, (HL)                         ; C=HOR BYTE POS

                SPR_LINE () ; 0
                SPR_AND_OR_LONG_EVEN ()

 inc c
 inc c
;                                LD A,2  ; 7
;                                ADD A,C ; 4
;                                LD C,A  ; 4

                                SPR_LINE () ; 8
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_LONG_EVEN ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 16
                                SPR_AND_OR_LONG_EVEN ()
                                INC H
                                SPR_AND_OR_WORD_ODD_START ()

STACK_EVEN      LD SP, $0000
                                RET
            ENDP

ODD_LINE_1      PROC
                                LD (STACK_ODD+1), SP ; store sp
                                                                ex de,hl ; restore sprite address
                                LD SP, HL                     ; SP = SPRITE ADDRESS

                                LD H, HIGH ScrBufY            ; HOR BYTES
                                LD L, B                        ; HOR
                                LD B,(HL)                     ; C=HOR BYTE POS

                                SPR_LINE () ; 0
                                SPR_AND_OR_WORD_ODD_START ()
                                                                INC H

                                INC B
                                INC B

                                SPR_AND_OR_LONG_ODD ()
                                INC H
                                SPR_AND_OR_LONG_ODD ()
                                INC H
                                SPR_AND_OR_LONG_ODD ()

                                LD A,7  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()

STACK_ODD       LD SP, $0000
                RET
                        ENDP

ODD_LINE_3      PROC
                                LD (STACK_ODD+1), SP ; store sp
                                                                ex de,hl ; restore sprite address
                                LD SP, HL                     ; SP = SPRITE ADDRESS

                                LD H, HIGH ScrBufY            ; HOR BYTES
                                LD L, B                        ; HOR
                                LD B,(HL)                     ; C=HOR BYTE POS

                                SPR_LINE () ; 0
                                SPR_AND_OR_WORD_ODD_START ()
                                INC H

                                INC B
                                INC B

                                SPR_AND_OR_LONG_ODD ()
                                INC H
                                SPR_AND_OR_LONG_ODD ()

                                LD A,5  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()

STACK_ODD       LD SP, $0000
                RET
                ENDP

ODD_LINE_5      PROC
                                LD (STACK_ODD+1), SP ; store sp
                                                                ex de,hl ; restore sprite address
                                LD SP, HL                     ; SP = SPRITE ADDRESS

                                LD H, HIGH ScrBufY            ; HOR BYTES
                                LD L, B                        ; HOR
                                LD B,(HL)                     ; C=HOR BYTE POS

                                SPR_LINE () ; 0
                                SPR_AND_OR_WORD_ODD_START ()
                                INC H

                                INC B
                                INC B

                                SPR_AND_OR_LONG_ODD ()

 inc c
 inc c
 inc c

                                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()

STACK_ODD       LD SP, $0000
                RET
                ENDP

ODD_LINE_7      PROC
; ret
                                LD (STACK_ODD+1), SP ; store sp
                                                                ex de,hl ; restore sprite address
                                LD SP, HL                     ; SP = SPRITE ADDRESS

                                LD H, HIGH ScrBufY            ; HOR BYTES
                                LD L, B                        ; HOR
                                LD B,(HL)                     ; C=HOR BYTE POS

                                SPR_LINE () ; 0
                                SPR_AND_OR_WORD_ODD_START ()
;                                INC H

                                INC B
                                INC B

;                                SPR_AND_OR_LONG_ODD ()

 inc c

                                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()

                                LD A,8  ; 7
                                ADD A,C ; 4
                                LD C,A  ; 4

                SPR_LINE () ; 9
                SPR_AND_OR_LONG_ODD ()
                INC H
                SPR_AND_OR_LONG_ODD ()

STACK_ODD       LD SP, $0000
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

SP21240300 DEFW SP21240301,SP21240302,SP21240303,SP21240304,SP21240305,SP21240306,SP21240307,SP21240308

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
ScrBufH
 DB (SCREEN+(0*2048)+(0*256)+(0*32))/256,(SCREEN+(0*2048)+(1*256)+(0*32))/256,(SCREEN+(0*2048)+(2*256)+(0*32))/256,(SCREEN+(0*2048)+(3*256)+(0*32))/256,(SCREEN+(0*2048)+(4*256)+(0*32))/256,(SCREEN+(0*2048)+(5*256)+(0*32))/256,(SCREEN+(0*2048)+(6*256)+(0*32))/256,(SCREEN+(0*2048)+(7*256)+(0*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(1*32))/256,(SCREEN+(0*2048)+(1*256)+(1*32))/256,(SCREEN+(0*2048)+(2*256)+(1*32))/256,(SCREEN+(0*2048)+(3*256)+(1*32))/256,(SCREEN+(0*2048)+(4*256)+(1*32))/256,(SCREEN+(0*2048)+(5*256)+(1*32))/256,(SCREEN+(0*2048)+(6*256)+(1*32))/256,(SCREEN+(0*2048)+(7*256)+(1*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(2*32))/256,(SCREEN+(0*2048)+(1*256)+(2*32))/256,(SCREEN+(0*2048)+(2*256)+(2*32))/256,(SCREEN+(0*2048)+(3*256)+(2*32))/256,(SCREEN+(0*2048)+(4*256)+(2*32))/256,(SCREEN+(0*2048)+(5*256)+(2*32))/256,(SCREEN+(0*2048)+(6*256)+(2*32))/256,(SCREEN+(0*2048)+(7*256)+(2*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(3*32))/256,(SCREEN+(0*2048)+(1*256)+(3*32))/256,(SCREEN+(0*2048)+(2*256)+(3*32))/256,(SCREEN+(0*2048)+(3*256)+(3*32))/256,(SCREEN+(0*2048)+(4*256)+(3*32))/256,(SCREEN+(0*2048)+(5*256)+(3*32))/256,(SCREEN+(0*2048)+(6*256)+(3*32))/256,(SCREEN+(0*2048)+(7*256)+(3*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(4*32))/256,(SCREEN+(0*2048)+(1*256)+(4*32))/256,(SCREEN+(0*2048)+(2*256)+(4*32))/256,(SCREEN+(0*2048)+(3*256)+(4*32))/256,(SCREEN+(0*2048)+(4*256)+(4*32))/256,(SCREEN+(0*2048)+(5*256)+(4*32))/256,(SCREEN+(0*2048)+(6*256)+(4*32))/256,(SCREEN+(0*2048)+(7*256)+(4*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(5*32))/256,(SCREEN+(0*2048)+(1*256)+(5*32))/256,(SCREEN+(0*2048)+(2*256)+(5*32))/256,(SCREEN+(0*2048)+(3*256)+(5*32))/256,(SCREEN+(0*2048)+(4*256)+(5*32))/256,(SCREEN+(0*2048)+(5*256)+(5*32))/256,(SCREEN+(0*2048)+(6*256)+(5*32))/256,(SCREEN+(0*2048)+(7*256)+(5*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(6*32))/256,(SCREEN+(0*2048)+(1*256)+(6*32))/256,(SCREEN+(0*2048)+(2*256)+(6*32))/256,(SCREEN+(0*2048)+(3*256)+(6*32))/256,(SCREEN+(0*2048)+(4*256)+(6*32))/256,(SCREEN+(0*2048)+(5*256)+(6*32))/256,(SCREEN+(0*2048)+(6*256)+(6*32))/256,(SCREEN+(0*2048)+(7*256)+(6*32))/256
 DB (SCREEN+(0*2048)+(0*256)+(7*32))/256,(SCREEN+(0*2048)+(1*256)+(7*32))/256,(SCREEN+(0*2048)+(2*256)+(7*32))/256,(SCREEN+(0*2048)+(3*256)+(7*32))/256,(SCREEN+(0*2048)+(4*256)+(7*32))/256,(SCREEN+(0*2048)+(5*256)+(7*32))/256,(SCREEN+(0*2048)+(6*256)+(7*32))/256,(SCREEN+(0*2048)+(7*256)+(7*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(0*32))/256,(SCREEN+(1*2048)+(1*256)+(0*32))/256,(SCREEN+(1*2048)+(2*256)+(0*32))/256,(SCREEN+(1*2048)+(3*256)+(0*32))/256,(SCREEN+(1*2048)+(4*256)+(0*32))/256,(SCREEN+(1*2048)+(5*256)+(0*32))/256,(SCREEN+(1*2048)+(6*256)+(0*32))/256,(SCREEN+(1*2048)+(7*256)+(0*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(1*32))/256,(SCREEN+(1*2048)+(1*256)+(1*32))/256,(SCREEN+(1*2048)+(2*256)+(1*32))/256,(SCREEN+(1*2048)+(3*256)+(1*32))/256,(SCREEN+(1*2048)+(4*256)+(1*32))/256,(SCREEN+(1*2048)+(5*256)+(1*32))/256,(SCREEN+(1*2048)+(6*256)+(1*32))/256,(SCREEN+(1*2048)+(7*256)+(1*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(2*32))/256,(SCREEN+(1*2048)+(1*256)+(2*32))/256,(SCREEN+(1*2048)+(2*256)+(2*32))/256,(SCREEN+(1*2048)+(3*256)+(2*32))/256,(SCREEN+(1*2048)+(4*256)+(2*32))/256,(SCREEN+(1*2048)+(5*256)+(2*32))/256,(SCREEN+(1*2048)+(6*256)+(2*32))/256,(SCREEN+(1*2048)+(7*256)+(2*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(3*32))/256,(SCREEN+(1*2048)+(1*256)+(3*32))/256,(SCREEN+(1*2048)+(2*256)+(3*32))/256,(SCREEN+(1*2048)+(3*256)+(3*32))/256,(SCREEN+(1*2048)+(4*256)+(3*32))/256,(SCREEN+(1*2048)+(5*256)+(3*32))/256,(SCREEN+(1*2048)+(6*256)+(3*32))/256,(SCREEN+(1*2048)+(7*256)+(3*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(4*32))/256,(SCREEN+(1*2048)+(1*256)+(4*32))/256,(SCREEN+(1*2048)+(2*256)+(4*32))/256,(SCREEN+(1*2048)+(3*256)+(4*32))/256,(SCREEN+(1*2048)+(4*256)+(4*32))/256,(SCREEN+(1*2048)+(5*256)+(4*32))/256,(SCREEN+(1*2048)+(6*256)+(4*32))/256,(SCREEN+(1*2048)+(7*256)+(4*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(5*32))/256,(SCREEN+(1*2048)+(1*256)+(5*32))/256,(SCREEN+(1*2048)+(2*256)+(5*32))/256,(SCREEN+(1*2048)+(3*256)+(5*32))/256,(SCREEN+(1*2048)+(4*256)+(5*32))/256,(SCREEN+(1*2048)+(5*256)+(5*32))/256,(SCREEN+(1*2048)+(6*256)+(5*32))/256,(SCREEN+(1*2048)+(7*256)+(5*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(6*32))/256,(SCREEN+(1*2048)+(1*256)+(6*32))/256,(SCREEN+(1*2048)+(2*256)+(6*32))/256,(SCREEN+(1*2048)+(3*256)+(6*32))/256,(SCREEN+(1*2048)+(4*256)+(6*32))/256,(SCREEN+(1*2048)+(5*256)+(6*32))/256,(SCREEN+(1*2048)+(6*256)+(6*32))/256,(SCREEN+(1*2048)+(7*256)+(6*32))/256
 DB (SCREEN+(1*2048)+(0*256)+(7*32))/256,(SCREEN+(1*2048)+(1*256)+(7*32))/256,(SCREEN+(1*2048)+(2*256)+(7*32))/256,(SCREEN+(1*2048)+(3*256)+(7*32))/256,(SCREEN+(1*2048)+(4*256)+(7*32))/256,(SCREEN+(1*2048)+(5*256)+(7*32))/256,(SCREEN+(1*2048)+(6*256)+(7*32))/256,(SCREEN+(1*2048)+(7*256)+(7*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(0*32))/256,(SCREEN+(2*2048)+(1*256)+(0*32))/256,(SCREEN+(2*2048)+(2*256)+(0*32))/256,(SCREEN+(2*2048)+(3*256)+(0*32))/256,(SCREEN+(2*2048)+(4*256)+(0*32))/256,(SCREEN+(2*2048)+(5*256)+(0*32))/256,(SCREEN+(2*2048)+(6*256)+(0*32))/256,(SCREEN+(2*2048)+(7*256)+(0*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(1*32))/256,(SCREEN+(2*2048)+(1*256)+(1*32))/256,(SCREEN+(2*2048)+(2*256)+(1*32))/256,(SCREEN+(2*2048)+(3*256)+(1*32))/256,(SCREEN+(2*2048)+(4*256)+(1*32))/256,(SCREEN+(2*2048)+(5*256)+(1*32))/256,(SCREEN+(2*2048)+(6*256)+(1*32))/256,(SCREEN+(2*2048)+(7*256)+(1*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(2*32))/256,(SCREEN+(2*2048)+(1*256)+(2*32))/256,(SCREEN+(2*2048)+(2*256)+(2*32))/256,(SCREEN+(2*2048)+(3*256)+(2*32))/256,(SCREEN+(2*2048)+(4*256)+(2*32))/256,(SCREEN+(2*2048)+(5*256)+(2*32))/256,(SCREEN+(2*2048)+(6*256)+(2*32))/256,(SCREEN+(2*2048)+(7*256)+(2*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(3*32))/256,(SCREEN+(2*2048)+(1*256)+(3*32))/256,(SCREEN+(2*2048)+(2*256)+(3*32))/256,(SCREEN+(2*2048)+(3*256)+(3*32))/256,(SCREEN+(2*2048)+(4*256)+(3*32))/256,(SCREEN+(2*2048)+(5*256)+(3*32))/256,(SCREEN+(2*2048)+(6*256)+(3*32))/256,(SCREEN+(2*2048)+(7*256)+(3*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(4*32))/256,(SCREEN+(2*2048)+(1*256)+(4*32))/256,(SCREEN+(2*2048)+(2*256)+(4*32))/256,(SCREEN+(2*2048)+(3*256)+(4*32))/256,(SCREEN+(2*2048)+(4*256)+(4*32))/256,(SCREEN+(2*2048)+(5*256)+(4*32))/256,(SCREEN+(2*2048)+(6*256)+(4*32))/256,(SCREEN+(2*2048)+(7*256)+(4*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(5*32))/256,(SCREEN+(2*2048)+(1*256)+(5*32))/256,(SCREEN+(2*2048)+(2*256)+(5*32))/256,(SCREEN+(2*2048)+(3*256)+(5*32))/256,(SCREEN+(2*2048)+(4*256)+(5*32))/256,(SCREEN+(2*2048)+(5*256)+(5*32))/256,(SCREEN+(2*2048)+(6*256)+(5*32))/256,(SCREEN+(2*2048)+(7*256)+(5*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(6*32))/256,(SCREEN+(2*2048)+(1*256)+(6*32))/256,(SCREEN+(2*2048)+(2*256)+(6*32))/256,(SCREEN+(2*2048)+(3*256)+(6*32))/256,(SCREEN+(2*2048)+(4*256)+(6*32))/256,(SCREEN+(2*2048)+(5*256)+(6*32))/256,(SCREEN+(2*2048)+(6*256)+(6*32))/256,(SCREEN+(2*2048)+(7*256)+(6*32))/256
 DB (SCREEN+(2*2048)+(0*256)+(7*32))/256,(SCREEN+(2*2048)+(1*256)+(7*32))/256,(SCREEN+(2*2048)+(2*256)+(7*32))/256,(SCREEN+(2*2048)+(3*256)+(7*32))/256,(SCREEN+(2*2048)+(4*256)+(7*32))/256,(SCREEN+(2*2048)+(5*256)+(7*32))/256,(SCREEN+(2*2048)+(6*256)+(7*32))/256,(SCREEN+(2*2048)+(7*256)+(7*32))/256

ALIGN $100
ScrBufL
 DB (SCREEN+(0*2048)+(0*256)+(0*32))&255,(SCREEN+(0*2048)+(1*256)+(0*32))&255,(SCREEN+(0*2048)+(2*256)+(0*32))&255,(SCREEN+(0*2048)+(3*256)+(0*32))&255,(SCREEN+(0*2048)+(4*256)+(0*32))&255,(SCREEN+(0*2048)+(5*256)+(0*32))&255,(SCREEN+(0*2048)+(6*256)+(0*32))&255,(SCREEN+(0*2048)+(7*256)+(0*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(1*32))&255,(SCREEN+(0*2048)+(1*256)+(1*32))&255,(SCREEN+(0*2048)+(2*256)+(1*32))&255,(SCREEN+(0*2048)+(3*256)+(1*32))&255,(SCREEN+(0*2048)+(4*256)+(1*32))&255,(SCREEN+(0*2048)+(5*256)+(1*32))&255,(SCREEN+(0*2048)+(6*256)+(1*32))&255,(SCREEN+(0*2048)+(7*256)+(1*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(2*32))&255,(SCREEN+(0*2048)+(1*256)+(2*32))&255,(SCREEN+(0*2048)+(2*256)+(2*32))&255,(SCREEN+(0*2048)+(3*256)+(2*32))&255,(SCREEN+(0*2048)+(4*256)+(2*32))&255,(SCREEN+(0*2048)+(5*256)+(2*32))&255,(SCREEN+(0*2048)+(6*256)+(2*32))&255,(SCREEN+(0*2048)+(7*256)+(2*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(3*32))&255,(SCREEN+(0*2048)+(1*256)+(3*32))&255,(SCREEN+(0*2048)+(2*256)+(3*32))&255,(SCREEN+(0*2048)+(3*256)+(3*32))&255,(SCREEN+(0*2048)+(4*256)+(3*32))&255,(SCREEN+(0*2048)+(5*256)+(3*32))&255,(SCREEN+(0*2048)+(6*256)+(3*32))&255,(SCREEN+(0*2048)+(7*256)+(3*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(4*32))&255,(SCREEN+(0*2048)+(1*256)+(4*32))&255,(SCREEN+(0*2048)+(2*256)+(4*32))&255,(SCREEN+(0*2048)+(3*256)+(4*32))&255,(SCREEN+(0*2048)+(4*256)+(4*32))&255,(SCREEN+(0*2048)+(5*256)+(4*32))&255,(SCREEN+(0*2048)+(6*256)+(4*32))&255,(SCREEN+(0*2048)+(7*256)+(4*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(5*32))&255,(SCREEN+(0*2048)+(1*256)+(5*32))&255,(SCREEN+(0*2048)+(2*256)+(5*32))&255,(SCREEN+(0*2048)+(3*256)+(5*32))&255,(SCREEN+(0*2048)+(4*256)+(5*32))&255,(SCREEN+(0*2048)+(5*256)+(5*32))&255,(SCREEN+(0*2048)+(6*256)+(5*32))&255,(SCREEN+(0*2048)+(7*256)+(5*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(6*32))&255,(SCREEN+(0*2048)+(1*256)+(6*32))&255,(SCREEN+(0*2048)+(2*256)+(6*32))&255,(SCREEN+(0*2048)+(3*256)+(6*32))&255,(SCREEN+(0*2048)+(4*256)+(6*32))&255,(SCREEN+(0*2048)+(5*256)+(6*32))&255,(SCREEN+(0*2048)+(6*256)+(6*32))&255,(SCREEN+(0*2048)+(7*256)+(6*32))&255
 DB (SCREEN+(0*2048)+(0*256)+(7*32))&255,(SCREEN+(0*2048)+(1*256)+(7*32))&255,(SCREEN+(0*2048)+(2*256)+(7*32))&255,(SCREEN+(0*2048)+(3*256)+(7*32))&255,(SCREEN+(0*2048)+(4*256)+(7*32))&255,(SCREEN+(0*2048)+(5*256)+(7*32))&255,(SCREEN+(0*2048)+(6*256)+(7*32))&255,(SCREEN+(0*2048)+(7*256)+(7*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(0*32))&255,(SCREEN+(1*2048)+(1*256)+(0*32))&255,(SCREEN+(1*2048)+(2*256)+(0*32))&255,(SCREEN+(1*2048)+(3*256)+(0*32))&255,(SCREEN+(1*2048)+(4*256)+(0*32))&255,(SCREEN+(1*2048)+(5*256)+(0*32))&255,(SCREEN+(1*2048)+(6*256)+(0*32))&255,(SCREEN+(1*2048)+(7*256)+(0*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(1*32))&255,(SCREEN+(1*2048)+(1*256)+(1*32))&255,(SCREEN+(1*2048)+(2*256)+(1*32))&255,(SCREEN+(1*2048)+(3*256)+(1*32))&255,(SCREEN+(1*2048)+(4*256)+(1*32))&255,(SCREEN+(1*2048)+(5*256)+(1*32))&255,(SCREEN+(1*2048)+(6*256)+(1*32))&255,(SCREEN+(1*2048)+(7*256)+(1*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(2*32))&255,(SCREEN+(1*2048)+(1*256)+(2*32))&255,(SCREEN+(1*2048)+(2*256)+(2*32))&255,(SCREEN+(1*2048)+(3*256)+(2*32))&255,(SCREEN+(1*2048)+(4*256)+(2*32))&255,(SCREEN+(1*2048)+(5*256)+(2*32))&255,(SCREEN+(1*2048)+(6*256)+(2*32))&255,(SCREEN+(1*2048)+(7*256)+(2*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(3*32))&255,(SCREEN+(1*2048)+(1*256)+(3*32))&255,(SCREEN+(1*2048)+(2*256)+(3*32))&255,(SCREEN+(1*2048)+(3*256)+(3*32))&255,(SCREEN+(1*2048)+(4*256)+(3*32))&255,(SCREEN+(1*2048)+(5*256)+(3*32))&255,(SCREEN+(1*2048)+(6*256)+(3*32))&255,(SCREEN+(1*2048)+(7*256)+(3*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(4*32))&255,(SCREEN+(1*2048)+(1*256)+(4*32))&255,(SCREEN+(1*2048)+(2*256)+(4*32))&255,(SCREEN+(1*2048)+(3*256)+(4*32))&255,(SCREEN+(1*2048)+(4*256)+(4*32))&255,(SCREEN+(1*2048)+(5*256)+(4*32))&255,(SCREEN+(1*2048)+(6*256)+(4*32))&255,(SCREEN+(1*2048)+(7*256)+(4*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(5*32))&255,(SCREEN+(1*2048)+(1*256)+(5*32))&255,(SCREEN+(1*2048)+(2*256)+(5*32))&255,(SCREEN+(1*2048)+(3*256)+(5*32))&255,(SCREEN+(1*2048)+(4*256)+(5*32))&255,(SCREEN+(1*2048)+(5*256)+(5*32))&255,(SCREEN+(1*2048)+(6*256)+(5*32))&255,(SCREEN+(1*2048)+(7*256)+(5*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(6*32))&255,(SCREEN+(1*2048)+(1*256)+(6*32))&255,(SCREEN+(1*2048)+(2*256)+(6*32))&255,(SCREEN+(1*2048)+(3*256)+(6*32))&255,(SCREEN+(1*2048)+(4*256)+(6*32))&255,(SCREEN+(1*2048)+(5*256)+(6*32))&255,(SCREEN+(1*2048)+(6*256)+(6*32))&255,(SCREEN+(1*2048)+(7*256)+(6*32))&255
 DB (SCREEN+(1*2048)+(0*256)+(7*32))&255,(SCREEN+(1*2048)+(1*256)+(7*32))&255,(SCREEN+(1*2048)+(2*256)+(7*32))&255,(SCREEN+(1*2048)+(3*256)+(7*32))&255,(SCREEN+(1*2048)+(4*256)+(7*32))&255,(SCREEN+(1*2048)+(5*256)+(7*32))&255,(SCREEN+(1*2048)+(6*256)+(7*32))&255,(SCREEN+(1*2048)+(7*256)+(7*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(0*32))&255,(SCREEN+(2*2048)+(1*256)+(0*32))&255,(SCREEN+(2*2048)+(2*256)+(0*32))&255,(SCREEN+(2*2048)+(3*256)+(0*32))&255,(SCREEN+(2*2048)+(4*256)+(0*32))&255,(SCREEN+(2*2048)+(5*256)+(0*32))&255,(SCREEN+(2*2048)+(6*256)+(0*32))&255,(SCREEN+(2*2048)+(7*256)+(0*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(1*32))&255,(SCREEN+(2*2048)+(1*256)+(1*32))&255,(SCREEN+(2*2048)+(2*256)+(1*32))&255,(SCREEN+(2*2048)+(3*256)+(1*32))&255,(SCREEN+(2*2048)+(4*256)+(1*32))&255,(SCREEN+(2*2048)+(5*256)+(1*32))&255,(SCREEN+(2*2048)+(6*256)+(1*32))&255,(SCREEN+(2*2048)+(7*256)+(1*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(2*32))&255,(SCREEN+(2*2048)+(1*256)+(2*32))&255,(SCREEN+(2*2048)+(2*256)+(2*32))&255,(SCREEN+(2*2048)+(3*256)+(2*32))&255,(SCREEN+(2*2048)+(4*256)+(2*32))&255,(SCREEN+(2*2048)+(5*256)+(2*32))&255,(SCREEN+(2*2048)+(6*256)+(2*32))&255,(SCREEN+(2*2048)+(7*256)+(2*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(3*32))&255,(SCREEN+(2*2048)+(1*256)+(3*32))&255,(SCREEN+(2*2048)+(2*256)+(3*32))&255,(SCREEN+(2*2048)+(3*256)+(3*32))&255,(SCREEN+(2*2048)+(4*256)+(3*32))&255,(SCREEN+(2*2048)+(5*256)+(3*32))&255,(SCREEN+(2*2048)+(6*256)+(3*32))&255,(SCREEN+(2*2048)+(7*256)+(3*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(4*32))&255,(SCREEN+(2*2048)+(1*256)+(4*32))&255,(SCREEN+(2*2048)+(2*256)+(4*32))&255,(SCREEN+(2*2048)+(3*256)+(4*32))&255,(SCREEN+(2*2048)+(4*256)+(4*32))&255,(SCREEN+(2*2048)+(5*256)+(4*32))&255,(SCREEN+(2*2048)+(6*256)+(4*32))&255,(SCREEN+(2*2048)+(7*256)+(4*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(5*32))&255,(SCREEN+(2*2048)+(1*256)+(5*32))&255,(SCREEN+(2*2048)+(2*256)+(5*32))&255,(SCREEN+(2*2048)+(3*256)+(5*32))&255,(SCREEN+(2*2048)+(4*256)+(5*32))&255,(SCREEN+(2*2048)+(5*256)+(5*32))&255,(SCREEN+(2*2048)+(6*256)+(5*32))&255,(SCREEN+(2*2048)+(7*256)+(5*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(6*32))&255,(SCREEN+(2*2048)+(1*256)+(6*32))&255,(SCREEN+(2*2048)+(2*256)+(6*32))&255,(SCREEN+(2*2048)+(3*256)+(6*32))&255,(SCREEN+(2*2048)+(4*256)+(6*32))&255,(SCREEN+(2*2048)+(5*256)+(6*32))&255,(SCREEN+(2*2048)+(6*256)+(6*32))&255,(SCREEN+(2*2048)+(7*256)+(6*32))&255
 DB (SCREEN+(2*2048)+(0*256)+(7*32))&255,(SCREEN+(2*2048)+(1*256)+(7*32))&255,(SCREEN+(2*2048)+(2*256)+(7*32))&255,(SCREEN+(2*2048)+(3*256)+(7*32))&255,(SCREEN+(2*2048)+(4*256)+(7*32))&255,(SCREEN+(2*2048)+(5*256)+(7*32))&255,(SCREEN+(2*2048)+(6*256)+(7*32))&255,(SCREEN+(2*2048)+(7*256)+(7*32))&255








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

ALIGN $100
SPR_LINE_JMP_TAB_LO
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
 db EVEN_LINE_0 &255, ODD_LINE_1 &255, EVEN_LINE_2 &255, ODD_LINE_3 &255, EVEN_LINE_4 &255, ODD_LINE_5 &255, EVEN_LINE_6 &255, ODD_LINE_7 &255
ALIGN $100
SPR_LINE_JMP_TAB_HI
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256
 db EVEN_LINE_0 /256, ODD_LINE_1 /256, EVEN_LINE_2 /256, ODD_LINE_3 /256, EVEN_LINE_4 /256, ODD_LINE_5 /256, EVEN_LINE_6 /256, ODD_LINE_7 /256

; mask , sprite
SP21240301
 db 252,3,63,192,255,0
 db 255,0,15,48,240,12
 db 224,16,7,24,255,0
 db 255,0,7,8,224,16
 db 192,32,3,4,255,0
 db 255,0,3,4,192,32
 db 192,32,3,4,255,0
 db 255,0,3,4,192,32
 db 192,48,3,12,255,0
 db 255,0,1,10,128,80
 db 128,76,1,50,255,0
 db 255,0,0,193,0,131
 db 0,128,0,1,255,0
 db 255,0,0,1,0,128
 db 0,128,0,1,255,0
 db 255,0,1,2,128,64
 db 128,64,1,2,255,0
 db 255,0,3,4,192,32
 db 224,16,7,8,255,0
 db 255,0,15,16,240,12
 db 252,3,31,224,255,0
SP21240302
 db 254,1,31,224,255,0
 db 255,0,7,24,248,6
 db 240,8,3,12,255,0
 db 255,0,3,4,240,8
 db 224,16,1,2,255,0
 db 255,0,1,2,224,16
 db 224,16,1,2,255,0
 db 255,0,1,2,224,16
 db 224,24,1,6,255,0
 db 255,0,0,5,192,40
 db 192,38,0,25,255,0
 db 127,128,0,224,128,65
 db 128,64,0,0,127,128
 db 127,128,0,0,128,64
 db 128,64,0,0,127,128
 db 255,0,0,1,192,32
 db 192,32,0,1,255,0
 db 255,0,1,2,224,16
 db 240,8,3,4,255,0
 db 255,0,7,8,248,6
 db 254,1,15,240,255,0
SP21240303
 db 255,0,15,240,255,0
 db 255,0,3,12,252,3
 db 248,4,1,6,255,0
 db 255,0,1,2,248,4
 db 240,8,0,1,255,0
 db 255,0,0,1,240,8
 db 240,8,0,1,255,0
 db 255,0,0,1,240,8
 db 240,12,0,3,255,0
 db 127,128,0,2,224,20
 db 224,19,0,12,127,128
 db 63,64,0,240,192,32
 db 192,32,0,0,63,64
 db 63,64,0,0,192,32
 db 192,32,0,0,63,64
 db 127,128,0,0,224,16
 db 224,16,0,0,127,128
 db 255,0,0,1,240,8
 db 248,4,1,2,255,0
 db 255,0,3,4,252,3
 db 255,0,7,248,255,0
SP21240304
 db 255,0,135,120,255,0
 db 255,0,1,134,254,1
 db 252,2,0,3,255,0
 db 255,0,0,1,252,2
 db 248,4,0,0,127,128
 db 127,128,0,0,248,4
 db 248,4,0,0,127,128
 db 127,128,0,0,248,4
 db 248,6,0,1,127,128
 db 63,64,0,1,240,10
 db 240,9,0,134,63,64
 db 31,32,0,120,224,16
 db 224,16,0,0,31,32
 db 31,32,0,0,224,16
 db 224,16,0,0,31,32
 db 63,64,0,0,240,8
 db 240,8,0,0,63,64
 db 127,128,0,0,248,4
 db 252,2,0,1,255,0
 db 255,0,1,130,254,1
 db 255,0,131,124,255,0
SP21240305
 db 255,0,195,60,255,0
 db 255,0,0,195,255,0
 db 254,1,0,1,127,128
 db 127,128,0,0,254,1
 db 252,2,0,0,63,64
 db 63,64,0,0,252,2
 db 252,2,0,0,63,64
 db 63,64,0,0,252,2
 db 252,3,0,0,63,192
 db 31,160,0,0,248,5
 db 248,4,0,195,31,32
 db 15,16,0,60,240,8
 db 240,8,0,0,15,16
 db 15,16,0,0,240,8
 db 240,8,0,0,15,16
 db 31,32,0,0,248,4
 db 248,4,0,0,31,32
 db 63,64,0,0,252,2
 db 254,1,0,0,127,128
 db 255,0,0,193,255,0
 db 255,0,193,62,255,0
SP21240306
 db 255,0,225,30,255,0
 db 127,128,128,97,255,0
 db 255,0,0,128,63,192
 db 63,64,0,128,255,0
 db 254,1,0,0,31,32
 db 31,32,0,0,254,1
 db 254,1,0,0,31,32
 db 31,32,0,0,254,1
 db 254,1,0,128,31,96
 db 15,80,0,128,252,2
 db 252,2,0,97,15,144
 db 7,8,0,30,248,4
 db 248,4,0,0,7,8
 db 7,8,0,0,248,4
 db 248,4,0,0,7,8
 db 15,16,0,0,252,2
 db 252,2,0,0,15,16
 db 31,32,0,0,254,1
 db 255,0,0,128,63,64
 db 127,128,128,96,255,0
 db 255,0,224,31,255,0
SP21240307
 db 255,0,240,15,255,0
 db 63,192,192,48,255,0
 db 255,0,128,64,31,96
 db 31,32,128,64,255,0
 db 255,0,0,128,15,16
 db 15,16,0,128,255,0
 db 255,0,0,128,15,16
 db 15,16,0,128,255,0
 db 255,0,0,192,15,48
 db 7,40,0,64,254,1
 db 254,1,0,48,7,200
 db 3,4,0,15,252,2
 db 252,2,0,0,3,4
 db 3,4,0,0,252,2
 db 252,2,0,0,3,4
 db 7,8,0,0,254,1
 db 254,1,0,0,7,8
 db 15,16,0,128,255,0
 db 255,0,128,64,31,32
 db 63,64,192,48,255,0
 db 255,0,240,15,127,128
SP21240308
 db 255,0,248,7,127,128
 db 31,96,224,24,255,0
 db 255,0,192,32,15,48
 db 15,16,192,32,255,0
 db 255,0,128,64,7,8
 db 7,8,128,64,255,0
 db 255,0,128,64,7,8
 db 7,8,128,64,255,0
 db 255,0,128,96,7,24
 db 3,20,0,160,255,0
 db 255,0,0,152,3,100
 db 1,130,0,7,254,1
 db 254,1,0,0,1,2
 db 1,2,0,0,254,1
 db 254,1,0,0,1,2
 db 3,4,0,128,255,0
 db 255,0,0,128,3,4
 db 7,8,128,64,255,0
 db 255,0,192,32,15,16
 db 31,32,224,24,255,0
 db 255,0,248,7,63,192

; ld l,c                         ; 4
; add hl,hl                      ; 15
; ld h, high addr        ; 7

; ld a,c         ; 4
; add a,a        ; 4
; ld l,a         ; 4
; ld h, high addr
; jp (hl)



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
