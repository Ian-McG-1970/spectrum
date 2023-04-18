; This command tells Zeus where to put the code it generates. As a szx file...
; Alter the path to suit your system

; zeusemulate "next"
zeusemulate "48k"
output_szx "spi.szx",32768,Start     ; The szx file

; If for some reason you want binary, uncomment this line
; output_bin "c:\spi.bin",$0000,$10000    ; The binary file
 ORG 32768

SCREEN          EQU 16384

SCREEN_ROW      EQU 32
SCREEN_LINE     EQU 192
SCREEN_SIZE     EQU SCREEN_ROW*SCREEN_LINE

ATTRIB          EQU 22528
ATTRIB_ROW      EQU 32
ATTRIB_LINE     EQU 24
ATTRIB_SIZE     EQU (ATTRIB_ROW*ATTRIB_LINE)

BORDER MACRO (COLOUR)
 LD A,COLOUR
 OUT (254),A
MEND

Start   DI                      ; interrupts off
        LD SP,MemTop            ; set stack to end

        LD HL,ATTRIB            ; attr start                             ; Clear the attributes
        LD DE,ATTRIB+1          ; sttr start +1
        LD BC,ATTRIB_SIZE-1     ; attr size -1
        LD (HL),7               ; clear first attr to white
        LDIR                    ; copy

 BORDER (0)

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

MAIN_LOOP
 CALL V_BLANK
 CALL COPY_SCREEN
 CALL CLEAR_SCREEN

 LD A,(XVAL+2)
 LD IXL,A
 LD A,(XVAL+1)
 LD IXH,A
 LD A,(XVAL+0)
 LD B,A
 LD A,0
 CALL HEX24

 LD A,(XINC)
 LD IY,$0001
 CALL HEX8

 LD A,(XVAL+2)
 LD L,A
 LD A,(XVAL+1)
 LD H,A
 LD A,(XVAL+0)
 CALL BIT24TOFP
 LD (XFP),HL

 LD IX,(XFP)
 LD A,2
 CALL HEX16

 LD HL,(XFP)
 CALL FPTO16BIT
 LD (XFP16B),HL

 LD IX,(XFP16B)
 LD A,3
 CALL HEX16

 LD HL,(XFP)
 CALL FPTO8BIT
 LD (XFP8B),A

 LD A,(XFP8B)
 LD IY,$0004
 CALL HEX8

 LD A,(ZVAL+2)
 LD IXL,A
 LD A,(ZVAL+1)
 LD IXH,A
 LD A,(ZVAL+0)
 LD B,A
 LD A,5
 CALL HEX24

 LD A,(ZINC)
 LD IY,$0006
 CALL HEX8

 LD A,(ZVAL+2)
 LD L,A
 LD A,(ZVAL+1)
 LD H,A
 LD A,(ZVAL+0)
 CALL BIT24TOFP
 LD A,L
 LD (ZFP),A
 LD A,H
 LD (ZFP+1),A

 LD IX,(ZFP)
 LD A,7
 CALL HEX16

 LD HL,(ZFP)
 CALL FPTO16BIT
 LD (ZFP16B),HL

 LD IX,(ZFP16B)
 LD A,8
 CALL HEX16

 LD HL,(ZFP)
 CALL FPTO8BIT
 LD (ZFP8B),A

 LD A,(ZFP8B)
 LD IY,$0009
 CALL HEX8

 LD BC,(XFP)
 LD HL,(ZFP)
 CALL FPADD
 LD (FP16ADD),HL

 LD IX,(FP16ADD)
 LD A,10
 CALL HEX16

; LD IY,$0001
; LD A,(XINC)
; CALL HEX8

 LD IX,$feef
 LD A,$0e
 CALL HEX16

 LD B,$FF
 LD IX,$EEDD
 LD A,$0f
 CALL HEX24

 CALL KEYBOARD
 JP MAIN_LOOP

; keys = port bit 0   1   2 3 4
;        fefe     sht z   x c v
;        fdfe     a   s   d f g
;        fbfe     q   w   e r t
;        f7fe     1   2   3 4 5
;        effe     0   9   8 7 6
;        dffe     p   o   1 u y
;        bffe     ent l   k j h
;        7ffe     spc sym m n b

KEYBOARD LD BC,$FBFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; q
         JP NZ,Q_KEY_N   ; not pressed
                LD      A,(XINC)
                LD      B,A
                LD      A,(XVAL+2)
                ADD     A,B
                LD      (XVAL+2),A
                LD      A,(XVAL+1)
                ADC     A,0
                LD      (XVAL+1),A
                LD      A,(XVAL+0)
                ADC     A,0
                LD      (XVAL+0),A

Q_KEY_N  LD BC,$FBFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $04         ; e
         JP NZ,Z_KEY_N   ; not pressed
                LD      A,(XINC)
                INC     A
                LD      (XINC),A

Z_KEY_N  LD BC,$FDFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; a
         JP NZ,A_KEY_N   ; not pressed
                LD      A,(XINC)
                LD      B,A
                LD      A,(XVAL+2)
                SUB     A,B
                LD      (XVAL+2),A
                LD      A,(XVAL+1)
                SBC     A,0
                LD      (XVAL+1),A
                LD      A,(XVAL+0)
                SBC     A,0
                LD      (XVAL+0),A

A_KEY_N  LD BC,$FDFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $04         ; d
         JP NZ,D_KEY_N   ; not pressed
                LD      A,(XINC)
                DEC     A
                LD      (XINC),A

D_KEY_N  LD BC,$FBFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $02         ; w
         JP NZ,W_KEY_N   ; not pressed
                LD      A,(ZINC)
                LD      B,A
                LD      A,(ZVAL+2)
                ADD     A,B
                LD      (ZVAL+2),A
                LD      A,(ZVAL+1)
                ADC     A,0
                LD      (ZVAL+1),A
                LD      A,(ZVAL+0)
                ADC     A,0
                LD      (ZVAL+0),A

W_KEY_N  LD BC,$FDFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $02         ; s
         JP NZ,S_KEY_N   ; not pressed
                LD      A,(ZINC)
                LD      B,A
                LD      A,(ZVAL+2)
                SUB     A,B
                LD      (ZVAL+2),A
                LD      A,(ZVAL+1)
                SBC     A,0
                LD      (ZVAL+1),A
                LD      A,(ZVAL+0)
                SBC     A,0
                LD      (ZVAL+0),A

S_KEY_N  LD BC,$FBFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $08         ; r
         JP NZ,R_KEY_N   ; not pressed
                LD      A,(ZINC)
                INC     A
                LD      (ZINC),A

R_KEY_N  LD BC,$FDFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $08         ; f
         JP NZ,F_KEY_N   ; not pressed
                LD      A,(ZINC)
                DEC     A
                LD      (ZINC),A

F_KEY_N RET

V_BLANK LD      DE,$1140                ; attr into D, MSB of port addr into E
FB_LP           INC     HL              ; padding instruction
                LD      A,E             ; MSB of port addr into A
                IN      A,($ff)         ; read port 0x40FF into A
                CP      D               ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
                JP      NZ, FB_LP       ; no? keep trying
        RET

; L = lsb
; H = psb
; A = msb

BIT24TOFP       LD      B,A             ; BACKUP SIGN
                AND     A,A     ;                        test    dl,dl                   ; test msb
                JP P,   BIT24FP_POS        ;                        jns             .BIT24FP_POS
                        XOR     A
                                                SUB     L
                        LD      L,A
                        LD      A,0
                        SBC     A,H
                        LD      H,A
                        LD      A,0
                        SBC     A,B

BIT24FP_POS     JP NZ,  BIT24                            ;;.BIT24FP_POS:   jnz             .BIT24

                LD      A,H
                AND     A,A
                JP NZ,  BIT16

                LD      A,L
                AND     A,A
                JP Z,   BIT00

                LD      H,L
                LD      L,0
                LD      A,8
                JP      EXP_LOOP

BIT24           LD      L,H
                LD      H,A
                LD      A,24

EXP_LOOP        DEC     A                     ; find exponent - start from whatever x is passed in and reduce until x a bit set found
                ADD     HL,HL
                JP NC,  EXP_LOOP

                SLA     A             ; exponent *2
                SLA     B                    ;       ASL     ab06
                RL      A                    ;       ROL                     ; rotate carry into first bit of exponent - bit 0 is sign
                LD      L,A
BIT00           RET                                  ; overflow

BIT16           LD      A,16
                JP      EXP_LOOP

FPTO16BIT       LD      A,L
                AND     A,A
                JP      M, FP16NEG       ; if negative

                LD      B,A           ; store in temp var for sign

                SRL     A
                SRL     A           ; /4

                CP      15           ; more than 16 bits
                JP NC,  FP16ERR                ; yes so error so exit with carry set

                LD      L,A
                ADD     A,A
                ADD     A,L           ; *3
                LD      (FP16_JMP+1),A

                XOR     A,A
                SCF
                RR      H
                RRA

FP16_JMP        JR      FP16JMP
                SRL     H   ;1
                RRA         ;l
FP16JMP         SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H
                RRA
                SRL     H   ;15
                RRA         ;15

                LD      L,A

                SRL     B            ; move input sign into carry
                JP NC,  FP16POS
                        LD      A,L
                        XOR     255
                        LD      L,A
                        LD      A,H
                        XOR     255
                        LD      H,A
;;                               neg             ax
FP16POS         RET

FP16NEG         SRL     B            ; 1st bit to carry
                JP NC,  FP16_POS
                        LD      HL,0ffffh
                        RET
FP16_POS:
FP16ERR         LD      HL,0
                RET

FPTO8BIT        LD      A,L
                AND     A,A
                JP      M, FP8NEG       ; if negative

                LD      B,A           ; store in temp var for sign

                SRL     A
                SRL     A           ; /4

                CP      7           ; more than 8 bits
                JP NC,  FP8ERR                ; yes so error so exit with carry set

                LD      (FP8_JMP+1),A

                LD      A,H
                SCF
                RRA

FP8_JMP         JR      FP8JMP
                RRA     ;l
FP8JMP          RRA
                RRA
                RRA
                RRA
                RRA
                RRA     ;7

FP8CONT         ADC     0
                JP M,   FP8ERR

                SRL     B
                JP NC,  FP8POS
                        NEG
FP8POS          RET

FP8NEG:
                CP      $FC
                XOR     A,A
                JP      FP8CONT

FP8ERR          XOR     A,A
                RET











;FP_ADD_ERR              mov             al,dh                   ;               TYA
;                                shl             al,1                    ;                       ASL
;                                jle             FP_ADD_ERR_1ST  ;                       BCC     FP_ADD_ERR_1ST
;                                jmp             FP_ADD_ERR_2ND  ;                       JMP     FP_ADD_ERR_2ND

MATH_LIMIT_EXIT LD              H,0
                                LD              L,A
                                RET

FP_ADD_SAME_POWER       LD              A,E
                                        SRL             A
                                        JP C,    FP_ADD_SAME_POWER_LESS_VALUE

                                        LD              A,H
                                        ADC             B
                                        RRA
                                        LD              H,A
                                        LD              A,4
                                        ADD             A,L
                                        LD              L,A
                                        RET

FP_ADD_SAME_POWER_LESS_VALUE:   LD              A,H
                                                                CCF
                                                                SBC             A,B
                                                                JP Z,   FPADD_CONT3
                                                                CCF                                     ; not needed?
                                                                JP C,   FPADD_CONT6     ; change to nc?
                                                                LD              L,C
                                                                NEG
                                                                JP              FPADD_CONT6

FPADD_CONT3:                                    LD              A,L
                                                                SUB             24h
;; BVS   _MATH_LIMIT_POS
                                                                JP              MATH_LIMIT_EXIT

;; A = AL  A
;; X = DL  L
;; Y = DH  H
;; 08 = BL C
;; 09 = BH B
;; ab18 = CL E
;; ab06 = CH D

FPADD:                  LD      A,L
                                SUB     A,C
;; BVS   FP_ADD_ERR
                                LD              E,A
                                CCF                     ; complement carry flag
                                ADC     A,1
;; BVS   FP_ADD_ERR
                                JP M,   FPADD_CONT8

                                SRL     A
                                SRL     A
                                JP Z,   FP_ADD_SAME_POWER

                                CP              09h
                                JP NC,  FP_ADD_ERR_1ST

                                XOR     15
                                ADD     A,A
                                LD      (FP_ADD_JMP1+1),A

                                LD      A,B
                                SCF
                                RRA

FP_ADD_JMP1     JR      b83B3
                NOP
                                NOP

                NOP
                                NOP

                NOP
                                NOP

                NOP
                                NOP

                NOP
                                NOP

                NOP
                                NOP

                NOP
                                NOP

b83B3           SRL     A
                SRL     A
                SRL     A
                SRL     A
                SRL     A
                SRL     A
                SRL     A

                LD              D,A
                LD              A,E

                SRL     A
                LD              A,H
                JP C,   FPADD_CONT5

FPADD_CONT4     ADC     A,D
                JP NC,  FPADD_CONT7
                                        SRL     A
                                        LD              H,A
                                        LD              A,4
                                        ADD             A,L
                                        LD              L,A
                                        RET

FPADD_CONT7     LD              H,A
FP_ADD_ERR_1ST: RET

FPADD_CONT5     CCF
                SBC     A,D
                CCF
                JP C,   FPADD_CONT7

FPADD_CONT6     LD              D,L
                LD      L,0

FPADD_LOOP1     DEC     L
                ADD     A,A
                JP NC,  FPADD_LOOP1

                LD      H,A
                LD      A,L
                ADD     A,A
                ADD     A,A

                ADD     A,D
;; BVS   FPADD_ERR3
                LD      L,A
FP_ADD_ERR_2ND: RET

FPADD_CONT8:    CP      0E0h
                                CCF
                JP NC,  FP_ADD_ERR_2ND

                SRL             A
                SRL     A
                                AND     7
                                ADD     A,A
                                LD              (FP_ADD_JMP2+1),A
                                LD              A,H
                                SCF
                                RRA
FP_ADD_JMP2     JR b8408
                SRL     A
                SRL     A
                SRL     A
b8408           SRL     A
                                SRL     A
                                SRL     A
                                SRL     A

                                LD              D,A
                                LD              L,C
                                LD              A,E
                                SRL     A
                                LD              A,B
                                JP NC,  FPADD_CONT4
                                JP              FPADD_CONT5


;
;; bx fp1
;; dx fp2
;; ax result
;
;FP_DIV:         mov             al,bl
;                        or              al,2
;                        mov             bl,bh
;                        xor             bh,bh                                   ; STA           FP_MULTIPLY_VALUE               ; store first value
;                        mov             ah,[LOG_TAB+bx]                 ;                       LDA     LOG_TABLE,X                             ; get second value - lookup bh
;                        mov             bl,dh                                   ;                       LDX     FP_MULTIPLY_POWER_SIGN  ; get first power sign
;                        sub             ah,[LOG_TAB+bx]                 ;                       ADC     LOG_TABLE,X                             ; second value + first power sign ? - subtract dh
;                        mov             bl,ah                                   ;                       TAX                                                     ; store in x
;                        mov             ah,[EXP_TAB+bx]                 ;                       LDY     EXP_TABLE,X                             ; get ouput power sign
;                        jnc             .cont                                   ;                       BCC     _FP_MUL_CNT
;                                        sub             al,4                    ;                                       ADC     #$03                    ; add 4 (00000011)
;.cont           sub             al,dl                                   ;_FP_MUL_CNT    ADC     FP_MULTIPLY_VALUE               ; add first value
;                        and             al,0fdh                                 ;                       AND     #$FD                                    ; get output value
;                        ret                                                             ;                       RTS
;
;; bx fp1
;; dx fp2
;; ax result
;
;FP_MUL:         mov             al,bh
;                        xor             bh,bh                                   ; STA           FP_MULTIPLY_VALUE               ; store first value
;                        mov             ah,[LOG_TAB+bx]                 ;                       LDA     LOG_TABLE,X                             ; get second value
;                        mov             bl,dh                                   ;                       LDX     FP_MULTIPLY_POWER_SIGN  ; get first power sign
;                        add             ah,[LOG_TAB+bx]                 ;                       ADC     LOG_TABLE,X                             ; second value + first power sign ?
;                        mov             bl,ah                                   ;                       TAX                                                     ; store in x
;                        mov             ah,[EXP_TAB+bx]                 ;                       LDY     EXP_TABLE,X                             ; get ouput power sign
;                        jnc             .cont                                   ;                       BCC     _FP_MUL_CNT
;                                        add             dl,4                    ;                                       ADC     #$03                    ; add 4 (00000011)
;.cont           add             al,dl                                   ;_FP_MUL_CNT    ADC     FP_MULTIPLY_VALUE               ; add first value
;                        and             al,0fdh                                 ;                       AND     #$FD                                    ; get output value
;                        ret                                                             ;                       RTS














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

HEX24
 LD C,A
 PUSH BC
 LD IYL,A
 LD IYH,2
 LD A,IXH
 CALL HEX8

 POP BC
 PUSH BC
 LD IYL,C
 LD IYH,4
 LD A,IXL
 CALL HEX8

 POP BC
 LD IYL,C
 LD IYH,0
 LD A,B
 JP HEX8       ; ret

CPYLINE MACRO (START_ADDR, END_ADDR)
 LD SP,START_ADDR
 POP AF
 POP BC
 POP DE
 POP HL
 POP IX
 EXX
 POP BC
 POP DE
 POP HL
 LD SP,END_ADDR+16
 PUSH HL
 PUSH DE
 PUSH BC
 EXX
 PUSH IX
 PUSH HL
 PUSH DE
 PUSH BC
 PUSH AF

 LD SP,START_ADDR+16
 POP AF
 POP BC
 POP DE
 POP HL
 POP IX
 EXX
 POP BC
 POP DE
 POP HL
 LD SP,END_ADDR+32
 PUSH HL
 PUSH DE
 PUSH BC
 EXX
 PUSH IX
 PUSH HL
 PUSH DE
 PUSH BC
 PUSH AF
MEND

COPY_SCREEN LD (COPY_SCREEN_END+1),SP ; This is some self-modifying code; stores the stack pointer in an LD SP,nn instruction at the end
CPYLINE (SCRBUFFER+(0*32), SCREEN+(0*32))
CPYLINE (SCRBUFFER+(1*32), SCREEN+(1*32))
CPYLINE (SCRBUFFER+(2*32), SCREEN+(2*32))
CPYLINE (SCRBUFFER+(3*32), SCREEN+(3*32))
CPYLINE (SCRBUFFER+(4*32), SCREEN+(4*32))
CPYLINE (SCRBUFFER+(5*32), SCREEN+(5*32))
CPYLINE (SCRBUFFER+(6*32), SCREEN+(6*32))
CPYLINE (SCRBUFFER+(7*32), SCREEN+(7*32))
CPYLINE (SCRBUFFER+(8*32), SCREEN+(8*32))
CPYLINE (SCRBUFFER+(9*32), SCREEN+(9*32))
CPYLINE (SCRBUFFER+(10*32), SCREEN+(10*32))
CPYLINE (SCRBUFFER+(11*32), SCREEN+(11*32))
CPYLINE (SCRBUFFER+(12*32), SCREEN+(12*32))
CPYLINE (SCRBUFFER+(13*32), SCREEN+(13*32))
CPYLINE (SCRBUFFER+(14*32), SCREEN+(14*32))
CPYLINE (SCRBUFFER+(15*32), SCREEN+(15*32))
CPYLINE (SCRBUFFER+(16*32), SCREEN+(16*32))
CPYLINE (SCRBUFFER+(17*32), SCREEN+(17*32))
CPYLINE (SCRBUFFER+(18*32), SCREEN+(18*32))
CPYLINE (SCRBUFFER+(19*32), SCREEN+(19*32))
CPYLINE (SCRBUFFER+(20*32), SCREEN+(20*32))
CPYLINE (SCRBUFFER+(21*32), SCREEN+(21*32))
CPYLINE (SCRBUFFER+(22*32), SCREEN+(22*32))
CPYLINE (SCRBUFFER+(23*32), SCREEN+(23*32))
CPYLINE (SCRBUFFER+(24*32), SCREEN+(24*32))
CPYLINE (SCRBUFFER+(25*32), SCREEN+(25*32))
CPYLINE (SCRBUFFER+(26*32), SCREEN+(26*32))
CPYLINE (SCRBUFFER+(27*32), SCREEN+(27*32))
CPYLINE (SCRBUFFER+(28*32), SCREEN+(28*32))
CPYLINE (SCRBUFFER+(29*32), SCREEN+(29*32))
CPYLINE (SCRBUFFER+(30*32), SCREEN+(30*32))
CPYLINE (SCRBUFFER+(31*32), SCREEN+(31*32))
CPYLINE (SCRBUFFER+(32*32), SCREEN+(32*32))
CPYLINE (SCRBUFFER+(33*32), SCREEN+(33*32))
CPYLINE (SCRBUFFER+(34*32), SCREEN+(34*32))
CPYLINE (SCRBUFFER+(35*32), SCREEN+(35*32))
CPYLINE (SCRBUFFER+(36*32), SCREEN+(36*32))
CPYLINE (SCRBUFFER+(37*32), SCREEN+(37*32))
CPYLINE (SCRBUFFER+(38*32), SCREEN+(38*32))
CPYLINE (SCRBUFFER+(39*32), SCREEN+(39*32))
CPYLINE (SCRBUFFER+(40*32), SCREEN+(40*32))
CPYLINE (SCRBUFFER+(41*32), SCREEN+(41*32))
CPYLINE (SCRBUFFER+(42*32), SCREEN+(42*32))
CPYLINE (SCRBUFFER+(43*32), SCREEN+(43*32))
CPYLINE (SCRBUFFER+(44*32), SCREEN+(44*32))
CPYLINE (SCRBUFFER+(45*32), SCREEN+(45*32))
CPYLINE (SCRBUFFER+(46*32), SCREEN+(46*32))
CPYLINE (SCRBUFFER+(47*32), SCREEN+(47*32))
CPYLINE (SCRBUFFER+(48*32), SCREEN+(48*32))
CPYLINE (SCRBUFFER+(49*32), SCREEN+(49*32))
CPYLINE (SCRBUFFER+(50*32), SCREEN+(50*32))
CPYLINE (SCRBUFFER+(51*32), SCREEN+(51*32))
CPYLINE (SCRBUFFER+(52*32), SCREEN+(52*32))
CPYLINE (SCRBUFFER+(53*32), SCREEN+(53*32))
CPYLINE (SCRBUFFER+(54*32), SCREEN+(54*32))
CPYLINE (SCRBUFFER+(55*32), SCREEN+(55*32))
CPYLINE (SCRBUFFER+(56*32), SCREEN+(56*32))
CPYLINE (SCRBUFFER+(57*32), SCREEN+(57*32))
CPYLINE (SCRBUFFER+(58*32), SCREEN+(58*32))
CPYLINE (SCRBUFFER+(59*32), SCREEN+(59*32))
CPYLINE (SCRBUFFER+(60*32), SCREEN+(60*32))
CPYLINE (SCRBUFFER+(61*32), SCREEN+(61*32))
CPYLINE (SCRBUFFER+(62*32), SCREEN+(62*32))
CPYLINE (SCRBUFFER+(63*32), SCREEN+(63*32))
CPYLINE (SCRBUFFER+(64*32), SCREEN+(64*32))
CPYLINE (SCRBUFFER+(65*32), SCREEN+(65*32))
CPYLINE (SCRBUFFER+(66*32), SCREEN+(66*32))
CPYLINE (SCRBUFFER+(67*32), SCREEN+(67*32))
CPYLINE (SCRBUFFER+(68*32), SCREEN+(68*32))
CPYLINE (SCRBUFFER+(69*32), SCREEN+(69*32))
CPYLINE (SCRBUFFER+(70*32), SCREEN+(70*32))
CPYLINE (SCRBUFFER+(71*32), SCREEN+(71*32))
CPYLINE (SCRBUFFER+(72*32), SCREEN+(72*32))
CPYLINE (SCRBUFFER+(73*32), SCREEN+(73*32))
CPYLINE (SCRBUFFER+(74*32), SCREEN+(74*32))
CPYLINE (SCRBUFFER+(75*32), SCREEN+(75*32))
CPYLINE (SCRBUFFER+(76*32), SCREEN+(76*32))
CPYLINE (SCRBUFFER+(77*32), SCREEN+(77*32))
CPYLINE (SCRBUFFER+(78*32), SCREEN+(78*32))
CPYLINE (SCRBUFFER+(79*32), SCREEN+(79*32))
CPYLINE (SCRBUFFER+(80*32), SCREEN+(80*32))
CPYLINE (SCRBUFFER+(81*32), SCREEN+(81*32))
CPYLINE (SCRBUFFER+(82*32), SCREEN+(82*32))
CPYLINE (SCRBUFFER+(83*32), SCREEN+(83*32))
CPYLINE (SCRBUFFER+(84*32), SCREEN+(84*32))
CPYLINE (SCRBUFFER+(85*32), SCREEN+(85*32))
CPYLINE (SCRBUFFER+(86*32), SCREEN+(86*32))
CPYLINE (SCRBUFFER+(87*32), SCREEN+(87*32))
CPYLINE (SCRBUFFER+(88*32), SCREEN+(88*32))
CPYLINE (SCRBUFFER+(89*32), SCREEN+(89*32))
CPYLINE (SCRBUFFER+(90*32), SCREEN+(90*32))
CPYLINE (SCRBUFFER+(91*32), SCREEN+(91*32))
CPYLINE (SCRBUFFER+(92*32), SCREEN+(92*32))
CPYLINE (SCRBUFFER+(93*32), SCREEN+(93*32))
CPYLINE (SCRBUFFER+(94*32), SCREEN+(94*32))
CPYLINE (SCRBUFFER+(95*32), SCREEN+(95*32))
CPYLINE (SCRBUFFER+(96*32), SCREEN+(96*32))
CPYLINE (SCRBUFFER+(97*32), SCREEN+(97*32))
CPYLINE (SCRBUFFER+(98*32), SCREEN+(98*32))
CPYLINE (SCRBUFFER+(99*32), SCREEN+(99*32))

CPYLINE (SCRBUFFER+(100*32), SCREEN+(100*32))
CPYLINE (SCRBUFFER+(101*32), SCREEN+(101*32))
CPYLINE (SCRBUFFER+(102*32), SCREEN+(102*32))
CPYLINE (SCRBUFFER+(103*32), SCREEN+(103*32))
CPYLINE (SCRBUFFER+(104*32), SCREEN+(104*32))
CPYLINE (SCRBUFFER+(105*32), SCREEN+(105*32))
CPYLINE (SCRBUFFER+(106*32), SCREEN+(106*32))
CPYLINE (SCRBUFFER+(107*32), SCREEN+(107*32))
CPYLINE (SCRBUFFER+(108*32), SCREEN+(108*32))
CPYLINE (SCRBUFFER+(109*32), SCREEN+(109*32))
CPYLINE (SCRBUFFER+(110*32), SCREEN+(110*32))
CPYLINE (SCRBUFFER+(111*32), SCREEN+(111*32))

CPYLINE (SCRBUFFER+(112*32), SCREEN+(112*32))

CPYLINE (SCRBUFFER+(113*32), SCREEN+(113*32))
CPYLINE (SCRBUFFER+(114*32), SCREEN+(114*32))
CPYLINE (SCRBUFFER+(115*32), SCREEN+(115*32))
CPYLINE (SCRBUFFER+(116*32), SCREEN+(116*32))
CPYLINE (SCRBUFFER+(117*32), SCREEN+(117*32))
CPYLINE (SCRBUFFER+(118*32), SCREEN+(118*32))
CPYLINE (SCRBUFFER+(119*32), SCREEN+(119*32))
CPYLINE (SCRBUFFER+(120*32), SCREEN+(120*32))
CPYLINE (SCRBUFFER+(121*32), SCREEN+(121*32))
CPYLINE (SCRBUFFER+(122*32), SCREEN+(122*32))
CPYLINE (SCRBUFFER+(123*32), SCREEN+(123*32))
CPYLINE (SCRBUFFER+(124*32), SCREEN+(124*32))
CPYLINE (SCRBUFFER+(125*32), SCREEN+(125*32))
CPYLINE (SCRBUFFER+(126*32), SCREEN+(126*32))
CPYLINE (SCRBUFFER+(127*32), SCREEN+(127*32))
CPYLINE (SCRBUFFER+(128*32), SCREEN+(128*32))
CPYLINE (SCRBUFFER+(129*32), SCREEN+(129*32))
CPYLINE (SCRBUFFER+(130*32), SCREEN+(130*32))
CPYLINE (SCRBUFFER+(131*32), SCREEN+(131*32))
CPYLINE (SCRBUFFER+(132*32), SCREEN+(132*32))
CPYLINE (SCRBUFFER+(133*32), SCREEN+(133*32))
CPYLINE (SCRBUFFER+(134*32), SCREEN+(134*32))
CPYLINE (SCRBUFFER+(135*32), SCREEN+(135*32))
CPYLINE (SCRBUFFER+(136*32), SCREEN+(136*32))
CPYLINE (SCRBUFFER+(137*32), SCREEN+(137*32))
CPYLINE (SCRBUFFER+(138*32), SCREEN+(138*32))
CPYLINE (SCRBUFFER+(139*32), SCREEN+(139*32))
CPYLINE (SCRBUFFER+(140*32), SCREEN+(140*32))
CPYLINE (SCRBUFFER+(141*32), SCREEN+(141*32))
CPYLINE (SCRBUFFER+(142*32), SCREEN+(142*32))
CPYLINE (SCRBUFFER+(143*32), SCREEN+(143*32))
CPYLINE (SCRBUFFER+(144*32), SCREEN+(144*32))
CPYLINE (SCRBUFFER+(145*32), SCREEN+(145*32))
CPYLINE (SCRBUFFER+(146*32), SCREEN+(146*32))
CPYLINE (SCRBUFFER+(147*32), SCREEN+(147*32))
CPYLINE (SCRBUFFER+(148*32), SCREEN+(148*32))
CPYLINE (SCRBUFFER+(149*32), SCREEN+(149*32))
CPYLINE (SCRBUFFER+(150*32), SCREEN+(150*32))
CPYLINE (SCRBUFFER+(151*32), SCREEN+(151*32))
CPYLINE (SCRBUFFER+(152*32), SCREEN+(152*32))
CPYLINE (SCRBUFFER+(153*32), SCREEN+(153*32))
CPYLINE (SCRBUFFER+(154*32), SCREEN+(154*32))
CPYLINE (SCRBUFFER+(155*32), SCREEN+(155*32))
CPYLINE (SCRBUFFER+(156*32), SCREEN+(156*32))
CPYLINE (SCRBUFFER+(157*32), SCREEN+(157*32))
CPYLINE (SCRBUFFER+(158*32), SCREEN+(158*32))
CPYLINE (SCRBUFFER+(159*32), SCREEN+(159*32))
CPYLINE (SCRBUFFER+(160*32), SCREEN+(160*32))
CPYLINE (SCRBUFFER+(161*32), SCREEN+(161*32))
CPYLINE (SCRBUFFER+(162*32), SCREEN+(162*32))
CPYLINE (SCRBUFFER+(163*32), SCREEN+(163*32))
CPYLINE (SCRBUFFER+(164*32), SCREEN+(164*32))
CPYLINE (SCRBUFFER+(165*32), SCREEN+(165*32))
CPYLINE (SCRBUFFER+(166*32), SCREEN+(166*32))
CPYLINE (SCRBUFFER+(167*32), SCREEN+(167*32))
CPYLINE (SCRBUFFER+(168*32), SCREEN+(168*32))
CPYLINE (SCRBUFFER+(169*32), SCREEN+(169*32))
CPYLINE (SCRBUFFER+(170*32), SCREEN+(170*32))
CPYLINE (SCRBUFFER+(171*32), SCREEN+(171*32))
CPYLINE (SCRBUFFER+(172*32), SCREEN+(172*32))
CPYLINE (SCRBUFFER+(173*32), SCREEN+(173*32))
CPYLINE (SCRBUFFER+(174*32), SCREEN+(174*32))
CPYLINE (SCRBUFFER+(175*32), SCREEN+(175*32))
CPYLINE (SCRBUFFER+(176*32), SCREEN+(176*32))
CPYLINE (SCRBUFFER+(177*32), SCREEN+(177*32))
CPYLINE (SCRBUFFER+(178*32), SCREEN+(178*32))
CPYLINE (SCRBUFFER+(179*32), SCREEN+(179*32))
CPYLINE (SCRBUFFER+(180*32), SCREEN+(180*32))
CPYLINE (SCRBUFFER+(181*32), SCREEN+(181*32))
CPYLINE (SCRBUFFER+(182*32), SCREEN+(182*32))
CPYLINE (SCRBUFFER+(183*32), SCREEN+(183*32))
CPYLINE (SCRBUFFER+(184*32), SCREEN+(184*32))
CPYLINE (SCRBUFFER+(185*32), SCREEN+(185*32))
CPYLINE (SCRBUFFER+(186*32), SCREEN+(186*32))
CPYLINE (SCRBUFFER+(187*32), SCREEN+(187*32))
CPYLINE (SCRBUFFER+(188*32), SCREEN+(188*32))
CPYLINE (SCRBUFFER+(189*32), SCREEN+(189*32))
CPYLINE (SCRBUFFER+(190*32), SCREEN+(190*32))
CPYLINE (SCRBUFFER+(191*32), SCREEN+(191*32))

COPY_SCREEN_END LD SP,$0000
 RET

CLEAR_SCREEN LD (CLEAR_SCREEN_END+1),SP ; Store the stack (self modding code)
 LD SP,SCRBUFFER+SCREEN_SIZE            ; Set stack to end of screen
 LD DE,$0000                            ; set DE to 0
 LD B,24                                ; loop 24 times - 96 words * 128 = 6144 bytes

CLEAR_SCREEN_LOOP PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 DJNZ CLEAR_SCREEN_LOOP

 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
 PUSH DE
CLEAR_SCREEN_END LD SP, $0000         ; Restore the stack
            RET

Hex0    DB 11111100b,00110000b,11111100b,11111100b,11001100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11111100b,11110000b,11111100b,11111100b
Hex1    DB 11001100b,11110000b,00001100b,00001100b,11001100b,11000000b,11000000b,00001100b,11001100b,11001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex2    DB 11001100b,00110000b,11111100b,11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,11111100b,11110000b,11000000b,11001100b,11111100b,11111100b
Hex3    DB 11001100b,00110000b,11000000b,00001100b,00001100b,00001100b,11001100b,00001100b,11001100b,00001100b,11001100b,11001100b,11000000b,11001100b,11000000b,11000000b
Hex4    DB 11111100b,11111100b,11111100b,11111100b,00001100b,11111100b,11111100b,00001100b,11111100b,11111100b,11001100b,11111100b,11111100b,11110000b,11111100b,11000000b

ALIGN $100

ScrBufH DEFB (SCRBUFFER+(0*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(0*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(0*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(1*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(1*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(2*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(2*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(3*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(3*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(4*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(4*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(5*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(5*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(6*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(6*32))/256

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(7*32))/256
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(7*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(0*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(0*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(1*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(1*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(2*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(2*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(3*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(3*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(4*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(4*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(5*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(5*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(6*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(6*32))/256

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(7*32))/256
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(7*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(0*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(0*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(1*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(1*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(2*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(2*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(3*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(3*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(4*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(4*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(5*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(5*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(6*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(6*32))/256

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(7*32))/256
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(7*32))/256

ALIGN $100

ScrBufL DEFB (SCRBUFFER+(0*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(0*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(0*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(1*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(1*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(2*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(2*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(3*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(3*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(4*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(4*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(5*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(5*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(6*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(6*32))&255

 DEFB (SCRBUFFER+(0*2048)+(0*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(1*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(2*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(3*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(4*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(5*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(6*256)+(7*32))&255
 DEFB (SCRBUFFER+(0*2048)+(7*256)+(7*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(0*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(0*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(1*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(1*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(2*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(2*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(3*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(3*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(4*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(4*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(5*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(5*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(6*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(6*32))&255

 DEFB (SCRBUFFER+(1*2048)+(0*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(1*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(2*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(3*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(4*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(5*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(6*256)+(7*32))&255
 DEFB (SCRBUFFER+(1*2048)+(7*256)+(7*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(0*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(0*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(1*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(1*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(2*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(2*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(3*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(3*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(4*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(4*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(5*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(5*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(6*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(6*32))&255

 DEFB (SCRBUFFER+(2*2048)+(0*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(1*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(2*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(3*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(4*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(5*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(6*256)+(7*32))&255
 DEFB (SCRBUFFER+(2*2048)+(7*256)+(7*32))&255

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

ScrBufOR DEFB 128,64,32,16,8,4,2,1
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
DEFB 128,64,32,16,8,4,2 ;,1

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

XVAL
DEFB $00,$02,$90
XINC
DEFB 1
ZVAL
DEFB $FF,$FC,$D4
ZINC
DEFB 1

XFP
DEFB 0,0
ZFP
DEFB 0,0

XFP16B
DEFB 0,0
ZFP16B
DEFB 0,0

FP16ADD
DEFB 0,0

XFP8B
DEFB 0
ZFP8B
DEFB 0

ALIGN $100
SCRBUFFER DEFS SCREEN_SIZE+32, 0

STACK DEFS 1024,0 ; stack buffer

MemTop DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.

Zeus_PC                           EQU Start                             ; Tell the emulator where to start.
Zeus_SP                           EQU MemTop                               ; Tell the emulator where to put the stack.



