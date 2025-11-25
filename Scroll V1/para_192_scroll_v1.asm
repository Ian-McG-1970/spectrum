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

SCRBUFFER               EQU 56*1024

CC_OFF_LEFT             EQU 1
CC_OFF_RIGHT    EQU 2
CC_OFF_TOP              EQU 4
CC_OFF_BOTTOM   EQU 8
CC_BEHIND               EQU     16

MID_H           EQU     128
MID_V           EQU     96

SIGN_EXTEND MACRO ()
        LD      L, A  ; Store low byte
    ADD     A, A  ; Push sign into carry
    SBC     A     ; Turn it into 0 or -1
    LD      H, A  ; Store high byte
MEND

BORDER MACRO (COLOUR)
 LD A,COLOUR
 OUT (254),A
MEND

COPY_WORD_REG MACRO (SRC_HI, SRC_LO, DST_HI, DST_LO)
 LD DST_LO, SRC_LO
 LD DST_HI, SRC_HI
MEND

Start   DI                      ; interrupts off
        LD SP,MemTop            ; set stack to end

        LD HL,SCREEN            ; attr start                             ; Clear the attributes
        LD DE,SCREEN+1          ; sttr start +1
        LD BC,SCREEN_SIZE-1     ; attr size -1
        LD (HL),0               ; clear first attr to white
        LDIR                    ; copy

        LD HL,ATTRIB            ; attr start                             ; Clear the attributes
        LD DE,ATTRIB+1          ; sttr start +1
        LD BC,ATTRIB_SIZE-1     ; attr size -1
        LD (HL),7               ; clear first attr to white
        LDIR                    ; copy

 BORDER (0)

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0+00), A
 LD ($5ae1+00), A
 LD ($5ae2+00), A

MAIN_LOOP
 CALL V_BLANK

 BORDER (2)
 CALL DRAW_SCREEN_192
 BORDER (4)

 CALL BUILD_SCREEN_LINES_FROM_CHAR_MAP_SCROLL
 BORDER (3)
 CALL SCREEN_BYTE_TO_WORD

; LD IX,(X_ROT)
; LD A, 1
; CALL HEX16
; LD IX,(Y_ROT)
; LD A, 2
; CALL HEX16
; LD IX,(Z_ROT)
; LD A, 3
; CALL HEX16

 CALL KEYBOARD
 JP MAIN_LOOP


GET_KEY MACRO (ADDR, AND_VAL)
                        LD      BC,ADDR     ; Load BC with the row port address
                        IN      A, (C)          ; Read the port into the accumulator
                        AND AND_VAL             ; q key
ENDM

KEYBOARD PROC
                        GET_KEY ($FBFE, $01) ; Q key
                        JP NZ, Q_KEY_N   ; not pressed
;                                ROTATE_POINT_INC (X_ROT)
        NOP
Q_KEY_N         GET_KEY ($FEFE, $02) ; Z key
                        JP NZ, Z_KEY_N   ; not pressed
;                                ROTATE_POINT_DEC (X_ROT)
        NOP

Z_KEY_N         GET_KEY ($FDFE, $01) ; A key
                        JP NZ, A_KEY_N   ; not pressed
;                                ROTATE_POINT_DEC (Y_ROT)
        NOP

A_KEY_N         GET_KEY ($FDFE, $04) ; D key
                        JP NZ, D_KEY_N   ; not pressed
;                                ROTATE_POINT_INC (Y_ROT)
        NOP

D_KEY_N         GET_KEY ($FBFE, $02) ; W key
                        JP NZ, W_KEY_N   ; not pressed
;                                ROTATE_POINT_INC (Z_ROT)
        NOP

W_KEY_N         GET_KEY ($FDFE, $02) ; S key
                        JP NZ, S_KEY_N   ; not pressed
;                                ROTATE_POINT_DEC (Z_ROT)
        NOP

S_KEY_N         GET_KEY ($DFFE, $04) ; I key
                        JP NZ, I_KEY_N   ; not pressed
;                                OFFSET_DEC (x_offset)
        NOP

I_KEY_N         GET_KEY ($BFFE, $04) ; K key
                        JP NZ, K_KEY_N   ; not pressed
;                                OFFSET_INC (x_offset)
        NOP

K_KEY_N         GET_KEY ($BFFE, $08) ; J key
                        JP NZ, J_KEY_N   ; not pressed
;                                OFFSET_DEC (y_offset)
        NOP

J_KEY_N         GET_KEY ($BFFE, $02) ; L key
                        JP NZ, L_KEY_N   ; not pressed
;                                OFFSET_INC (y_offset)
        NOP

L_KEY_N         GET_KEY ($DFFE, $08) ; U key
                        JP NZ, U_KEY_N   ; not pressed
;                                OFFSET_INC (z_offset)
        NOP

U_KEY_N         GET_KEY ($7FFE, $08) ; N key
                        JP NZ, N_KEY_N   ; not pressed
;                                OFFSET_DEC (z_offset)
        NOP

N_KEY_N         RET
                ENDP

V_BLANK PROC
                        LD      DE, $1140               ; attr into D, MSB of port addr into E
NEXT_LOOP       INC     HL              ; padding instruction
                LD      A, E            ; MSB of port addr into A
                IN      A, ($FF)                ; read port 0x40FF into A
                CP      D               ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
                JP      NZ, NEXT_LOOP   ; no? keep trying
                        RET
                ENDP

;PLOT
; LD H, HIGH ScrBufH     ; SCREEN V TABLE
; LD D,(HL)              ; L=VPOS
; INC H    ; ScrBufL
; LD E,(HL)              ; DE = SCREEN POS
;
; LD L,C                 ; C=HPOS
; INC H    ; ScrBufY
;
; LD L, (HL)             ; HOR BYTE POS
; LD H,0                 ; CLEAR
; ADD HL,DE              ; SCREEN POS + HOR BYTE POS
;
; LD B, HIGH ScrBufOR    ; SCREEN OR TABLE
; LD A, (BC)
; OR (HL)
; LD (HL), A
;RET

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
; JP HEX8       ; ret

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
; JP HEXCHAR    ; ret

; L=Ver
; IXL=Hor
; C=Val

HEXCHAR
 LD A,C
 ADD A,A ; *2
 ADD A,A ; *4
 ADD A,C ; *5
 LD C,A

;;;; LD H, HIGH ScrBufH     ; SCREEN V TABLE
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

;HEX24
; LD C,A
; PUSH BC
; LD IYL,A
; LD IYH,2
; LD A,IXH
; CALL HEX8

; POP BC
; PUSH BC
; LD IYL,C
; LD IYH,4
; LD A,IXL
; CALL HEX8

; POP BC
; LD IYL,C
; LD IYH,0
; LD A,B
; JP HEX8       ; ret

SCREEN_BYTE_TO_WORD PROC
    LD  H, HIGH PATTERN_HI

    LD  BC, MAP_16X96_00 +11
    LD  IX, MAP_16X96_00 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_01 +11
    LD  IX, MAP_16X96_01 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_02 +11
    LD  IX, MAP_16X96_02 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_03 +11
    LD  IX, MAP_16X96_03 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_04 +11
    LD  IX, MAP_16X96_04 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_05 +11
    LD  IX, MAP_16X96_05 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_06 +11
    LD  IX, MAP_16X96_06 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_07 +11
    LD  IX, MAP_16X96_07 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_08 +11
    LD  IX, MAP_16X96_08 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_09 +11
    LD  IX, MAP_16X96_09 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_10 +11
    LD  IX, MAP_16X96_10 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_11 +11
    LD  IX, MAP_16X96_11 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_12 +11
    LD  IX, MAP_16X96_12 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_13 +11
    LD  IX, MAP_16X96_13 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_14 +11
    LD  IX, MAP_16X96_14 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_15 +11
    LD  IX, MAP_16X96_15 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_16 +11
    LD  IX, MAP_16X96_16 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_17 +11
    LD  IX, MAP_16X96_17 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_18 +11
    LD  IX, MAP_16X96_18 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_19 +11
    LD  IX, MAP_16X96_19 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_20 +11
    LD  IX, MAP_16X96_20 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_21 +11
    LD  IX, MAP_16X96_21 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_22 +11
    LD  IX, MAP_16X96_22 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_23 +11
    LD  IX, MAP_16X96_23 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_24 +11
    LD  IX, MAP_16X96_24 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_25 +11
    LD  IX, MAP_16X96_25 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_26 +11
    LD  IX, MAP_16X96_26 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_27 +11
    LD  IX, MAP_16X96_27 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_28 +11
    LD  IX, MAP_16X96_28 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_29 +11
    LD  IX, MAP_16X96_29 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_30 +11
    LD  IX, MAP_16X96_30 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_31 +11
    LD  IX, MAP_16X96_31 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_32 +11
    LD  IX, MAP_16X96_32 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_33 +11
    LD  IX, MAP_16X96_33 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_34 +11
    LD  IX, MAP_16X96_34 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_35 +11
    LD  IX, MAP_16X96_35 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_36 +11
    LD  IX, MAP_16X96_36 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_37 +11
    LD  IX, MAP_16X96_37 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_38 +11
    LD  IX, MAP_16X96_38 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_39 +11
    LD  IX, MAP_16X96_39 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_40 +11
    LD  IX, MAP_16X96_40 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_41 +11
    LD  IX, MAP_16X96_41 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_42 +11
    LD  IX, MAP_16X96_42 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_43 +11
    LD  IX, MAP_16X96_43 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_44 +11
    LD  IX, MAP_16X96_44 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_45 +11
    LD  IX, MAP_16X96_45 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_46 +11
    LD  IX, MAP_16X96_46 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_47 +11
    LD  IX, MAP_16X96_47 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_48 +11
    LD  IX, MAP_16X96_48 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_49 +11
    LD  IX, MAP_16X96_49 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_50 +11
    LD  IX, MAP_16X96_50 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_51 +11
    LD  IX, MAP_16X96_51 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_52 +11
    LD  IX, MAP_16X96_52 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_53 +11
    LD  IX, MAP_16X96_53 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_54 +11
    LD  IX, MAP_16X96_54 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_55 +11
    LD  IX, MAP_16X96_55 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_56 +11
    LD  IX, MAP_16X96_56 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_57 +11
    LD  IX, MAP_16X96_57 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_58 +11
    LD  IX, MAP_16X96_58 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_59 +11
    LD  IX, MAP_16X96_59 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_60 +11
    LD  IX, MAP_16X96_60 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_61 +11
    LD  IX, MAP_16X96_61 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_62 +11
    LD  IX, MAP_16X96_62 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_63 +11
    LD  IX, MAP_16X96_63 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_64 +11
    LD  IX, MAP_16X96_64 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_65 +11
    LD  IX, MAP_16X96_65 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_66 +11
    LD  IX, MAP_16X96_66 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_67 +11
    LD  IX, MAP_16X96_67 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_68 +11
    LD  IX, MAP_16X96_68 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_69 +11
    LD  IX, MAP_16X96_69 +24
    CALL SCREEN_LINE_BYTE_TO_WORD



    LD  BC, MAP_16X96_70 +11
    LD  IX, MAP_16X96_70 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_71 +11
    LD  IX, MAP_16X96_71 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_72 +11
    LD  IX, MAP_16X96_72 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_73 +11
    LD  IX, MAP_16X96_73 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_74 +11
    LD  IX, MAP_16X96_74 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_75 +11
    LD  IX, MAP_16X96_75 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_76 +11
    LD  IX, MAP_16X96_76 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_77 +11
    LD  IX, MAP_16X96_77 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_78 +11
    LD  IX, MAP_16X96_78 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_79 +11
    LD  IX, MAP_16X96_79 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_80 +11
    LD  IX, MAP_16X96_80 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_81 +11
    LD  IX, MAP_16X96_81 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_82 +11
    LD  IX, MAP_16X96_82 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_83 +11
    LD  IX, MAP_16X96_83 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_84 +11
    LD  IX, MAP_16X96_84 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_85 +11
    LD  IX, MAP_16X96_85 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_86 +11
    LD  IX, MAP_16X96_86 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_87 +11
    LD  IX, MAP_16X96_87 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_88 +11
    LD  IX, MAP_16X96_88 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_89 +11
    LD  IX, MAP_16X96_89 +24
    CALL SCREEN_LINE_BYTE_TO_WORD


    LD  BC, MAP_16X96_90 +11
    LD  IX, MAP_16X96_90 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_91 +11
    LD  IX, MAP_16X96_91 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_92 +11
    LD  IX, MAP_16X96_92 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_93 +11
    LD  IX, MAP_16X96_93 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_94 +11
    LD  IX, MAP_16X96_94 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

    LD  BC, MAP_16X96_95 +11
    LD  IX, MAP_16X96_95 +24
    CALL SCREEN_LINE_BYTE_TO_WORD

        RET
ENDP

SCREEN_LINE_BYTE_TO_WORD PROC
        LD (STACK_PTR +1),SP
        LD      SP, IX

        LD A,(BC)     ; #11
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #10
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #1
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H

        LD A,(BC)     ; #0
        PUSH DE
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        DEC H
        PUSH DE

STACK_PTR LD SP,#0000 ; 10 / 3
        RET
ENDP


;loop
; get the char from the map (1)
; get the byte for the line for that char (2)
; get the low char pos (3)
; or it with a (4)
; store in the map line (5)
; get the high char pos in a (6)
; next map line pos (7)
; next loop (8)

;hl holds map address (12x12)
;b holds the address of the vertical position (0-7) of the 255 values for the 255 chars
;d holds the address of the 255 chars for the scroll position (0-7)
;hl' holds the map screen address (24x96)

;ld c,(hl)      ; get the char from the map (1)
;ld a,(bc)  ; get the byte for the line for that char (2)
;ld e,a
;ld c,(de)      ; get the low char pos (3)
;or a,c         ; or it with a (4)
;exx
;ld (hl),a      ; store in the map line (5)
;inc hl         ; next map line pos (7)
;exx
;inc d
;ld a,(de)      ; get the high char pos in a (6)
;dec d

CHAR_MAP_TO_SCREEN_LINE_SCROLL PROC     ;HL= map address (12x12) ;B= address of the vertical position (0-7) of the 255 values for the 255 chars ; D= address of the 255 chars for the scroll position (0-7) ;HL'= map screen address (24x96)

;loop
; get the char from the map (1)
; get the byte for the line for that char (2)
; get the low char pos (3)
; or it with a (4)
; store in the map line (5)
; get the high char pos in a (6)
; next map line pos (7)
; next loop (8)

; char #0
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #1
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #2
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #3
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #4
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #5
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #6
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #7
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #8
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #9
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #10
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #11
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

; char #12
        LD C, (HL)      ; get the char from the map (1)
        INC L
        LD A, (BC)      ; get the byte for the line for that char (2)
        LD E, A
        LD A, (DE)      ; get the low char pos (3)
        LD C, A
        EX AF,AF'
        OR A, C         ; or it with a (4)
        EXX
        LD (HL), A      ; store in the map line (5)
        INC HL          ; next map line pos (7)
        EXX
        INC D
        LD A, (DE)      ; get the high char pos in a (6)
        EX AF,AF'
        DEC D

        RET
ENDP

CHAR_MAP_TO_SCREEN_LINE PROC ; HL=CHAR MAP ADDRESS (SRC) / B=CHARSET LINE ADDRESS / DE = MAP LINE ADDRESS (DST)
        LD      C, (HL) ; get map char pointed to be L
        LD      A, (BC) ; get char pointed to by c in charset pointed to by bc
        LD (DE), A      ; store in screen map line
        INC L           ; next char to read
        INC E           ; next char to write

        LD      C, (HL) ; char #1
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #2
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #3
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #4
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #5
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #6
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #7
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #8
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #9
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #10
        LD      A, (BC)
        LD (DE), A
        INC L
        INC E

        LD      C, (HL) ; char #11
        LD      A, (BC)
        LD (DE), A

        RET
ENDP

BUILD_SCREEN_LINES_FROM_CHAR_MAP_SCROLL PROC
        LD      D, HIGH SCROLL_CHARSET_0_LO

        LD      B, HIGH CHARSET_00

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_00
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_08
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_16
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_24
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_32
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_40
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_48
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_56
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_64
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_72
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_80
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_88
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_01

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_01
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_09
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_17
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_25
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_33
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_41
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_49
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_57
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_65
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_73
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_81
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_89
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_02

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_02
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_10
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_18
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_26
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_34
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_42
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_50
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_58
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_66
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_74
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_82
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_90
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_03

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_03
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_11
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_19
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_27
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_35
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_43
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_51
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_59
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_67
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_75
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_83
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_91
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_04

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_04
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_12
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_20
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_28
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_36
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_44
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_52
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_60
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_68
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_76
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_84
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_92
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_05

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_05
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_13
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_21
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_29
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_37
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_45
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_53
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_61
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_69
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_77
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_85
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_93
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_06

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_06
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_14
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_22
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_30
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_38
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_46
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_54
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_62
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_70
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_78
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_86
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_94
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL

        LD      B, HIGH CHARSET_07

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_07
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_15
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_23
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_31
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_39
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_47
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_55
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_63
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_71
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_79
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_87
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_95
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL














        RET
ENDP

BUILD_SCREEN_LINES_FROM_CHAR_MAP PROC
        LD      B, HIGH CHARSET_00
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_00
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_08
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_16
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_24
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_32
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_40
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_48
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_56
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_64
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_72
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_80
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_88
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_01
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_01
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_09
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_17
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_25
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_33
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_41
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_49
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_57
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_65
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_73
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_81
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_89
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_02
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_02
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_10
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_18
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_26
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_34
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_42
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_50
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_58
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_66
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_74
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_82
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_90
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_03
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_03
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_11
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_19
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_27
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_35
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_43
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_51
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_59
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_67
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_75
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_83
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_91
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_04
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_04
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_12
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_20
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_28
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_36
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_44
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_52
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_60
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_68
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_76
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_84
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_92
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_05
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_05
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_13
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_21
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_29
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_37
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_45
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_53
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_61
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_69
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_77
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_85
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_93
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_06
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_06
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_14
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_22
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_30
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_38
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_46
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_54
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_62
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_70
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_78
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_86
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_94
        CALL    CHAR_MAP_TO_SCREEN_LINE

        LD      B, HIGH CHARSET_07
        LD      HL, MAP_16X16_00
        LD      DE, MAP_16X96_07
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_01
        LD      DE, MAP_16X96_15
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_02
        LD      DE, MAP_16X96_23
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_03
        LD      DE, MAP_16X96_31
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_04
        LD      DE, MAP_16X96_39
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_05
        LD      DE, MAP_16X96_47
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_06
        LD      DE, MAP_16X96_55
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_07
        LD      DE, MAP_16X96_63
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_08
        LD      DE, MAP_16X96_71
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_09
        LD      DE, MAP_16X96_79
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_10
        LD      DE, MAP_16X96_87
        CALL    CHAR_MAP_TO_SCREEN_LINE
        LD      HL, MAP_16X16_11
        LD      DE, MAP_16X96_95
        CALL    CHAR_MAP_TO_SCREEN_LINE

        RET
ENDP

DRAW_LINE_192 MACRO (SCN_ADDR, MAP_ADDR)
        LD SP,MAP_ADDR
        POP BC
        POP DE
        POP HL
        EXX
        POP BC
        POP DE
        POP HL

        LD SP,SCN_ADDR+12
        PUSH HL
        PUSH DE
        PUSH BC
        EXX
        PUSH HL
        PUSH DE
        PUSH BC

        LD SP,MAP_ADDR+12
        POP BC
        POP DE
        POP HL
        EXX
        POP BC
        POP DE
        POP HL

        LD SP,SCN_ADDR+24
        PUSH HL
        PUSH DE
        PUSH BC
        EXX
        PUSH HL
        PUSH DE
        PUSH BC
MEND

DRAW_SCREEN_192 PROC
 LD (STACK_PTR +1),SP

 DRAW_LINE_192 (SCR_LINE_000, MAP_16X96_00)
 DRAW_LINE_192 (SCR_LINE_002, MAP_16X96_01)
 DRAW_LINE_192 (SCR_LINE_004, MAP_16X96_02)
 DRAW_LINE_192 (SCR_LINE_006, MAP_16X96_03)
 DRAW_LINE_192 (SCR_LINE_008, MAP_16X96_04)
 DRAW_LINE_192 (SCR_LINE_010, MAP_16X96_05)
 DRAW_LINE_192 (SCR_LINE_012, MAP_16X96_06)
 DRAW_LINE_192 (SCR_LINE_014, MAP_16X96_07)

 DRAW_LINE_192 (SCR_LINE_016, MAP_16X96_08)
 DRAW_LINE_192 (SCR_LINE_018, MAP_16X96_09)
 DRAW_LINE_192 (SCR_LINE_020, MAP_16X96_10)
 DRAW_LINE_192 (SCR_LINE_022, MAP_16X96_11)
 DRAW_LINE_192 (SCR_LINE_024, MAP_16X96_12)
 DRAW_LINE_192 (SCR_LINE_026, MAP_16X96_13)
 DRAW_LINE_192 (SCR_LINE_028, MAP_16X96_14)
 DRAW_LINE_192 (SCR_LINE_030, MAP_16X96_15)

 DRAW_LINE_192 (SCR_LINE_032, MAP_16X96_16)
 DRAW_LINE_192 (SCR_LINE_034, MAP_16X96_17)
 DRAW_LINE_192 (SCR_LINE_036, MAP_16X96_18)
 DRAW_LINE_192 (SCR_LINE_038, MAP_16X96_19)
 DRAW_LINE_192 (SCR_LINE_040, MAP_16X96_20)
 DRAW_LINE_192 (SCR_LINE_042, MAP_16X96_21)
 DRAW_LINE_192 (SCR_LINE_044, MAP_16X96_22)
 DRAW_LINE_192 (SCR_LINE_046, MAP_16X96_23)

 DRAW_LINE_192 (SCR_LINE_048, MAP_16X96_24)
 DRAW_LINE_192 (SCR_LINE_050, MAP_16X96_25)
 DRAW_LINE_192 (SCR_LINE_052, MAP_16X96_26)
 DRAW_LINE_192 (SCR_LINE_054, MAP_16X96_27)
 DRAW_LINE_192 (SCR_LINE_056, MAP_16X96_28)
 DRAW_LINE_192 (SCR_LINE_058, MAP_16X96_29)
 DRAW_LINE_192 (SCR_LINE_060, MAP_16X96_30)
 DRAW_LINE_192 (SCR_LINE_062, MAP_16X96_31)

 DRAW_LINE_192 (SCR_LINE_064, MAP_16X96_32)
 DRAW_LINE_192 (SCR_LINE_066, MAP_16X96_33)
 DRAW_LINE_192 (SCR_LINE_068, MAP_16X96_34)
 DRAW_LINE_192 (SCR_LINE_070, MAP_16X96_35)
 DRAW_LINE_192 (SCR_LINE_072, MAP_16X96_36)
 DRAW_LINE_192 (SCR_LINE_074, MAP_16X96_37)
 DRAW_LINE_192 (SCR_LINE_076, MAP_16X96_38)
 DRAW_LINE_192 (SCR_LINE_078, MAP_16X96_39)

 DRAW_LINE_192 (SCR_LINE_080, MAP_16X96_40)
 DRAW_LINE_192 (SCR_LINE_082, MAP_16X96_41)
 DRAW_LINE_192 (SCR_LINE_084, MAP_16X96_42)
 DRAW_LINE_192 (SCR_LINE_086, MAP_16X96_43)
 DRAW_LINE_192 (SCR_LINE_088, MAP_16X96_44)
 DRAW_LINE_192 (SCR_LINE_090, MAP_16X96_45)
 DRAW_LINE_192 (SCR_LINE_092, MAP_16X96_46)
 DRAW_LINE_192 (SCR_LINE_094, MAP_16X96_47)

 DRAW_LINE_192 (SCR_LINE_096, MAP_16X96_48)
 DRAW_LINE_192 (SCR_LINE_098, MAP_16X96_49)
 DRAW_LINE_192 (SCR_LINE_100, MAP_16X96_50)
 DRAW_LINE_192 (SCR_LINE_102, MAP_16X96_51)
 DRAW_LINE_192 (SCR_LINE_104, MAP_16X96_52)
 DRAW_LINE_192 (SCR_LINE_106, MAP_16X96_53)
 DRAW_LINE_192 (SCR_LINE_108, MAP_16X96_54)
 DRAW_LINE_192 (SCR_LINE_110, MAP_16X96_55)

 DRAW_LINE_192 (SCR_LINE_112, MAP_16X96_56)
 DRAW_LINE_192 (SCR_LINE_114, MAP_16X96_57)
 DRAW_LINE_192 (SCR_LINE_116, MAP_16X96_58)
 DRAW_LINE_192 (SCR_LINE_118, MAP_16X96_59)
 DRAW_LINE_192 (SCR_LINE_120, MAP_16X96_60)
 DRAW_LINE_192 (SCR_LINE_122, MAP_16X96_61)
 DRAW_LINE_192 (SCR_LINE_124, MAP_16X96_62)
 DRAW_LINE_192 (SCR_LINE_126, MAP_16X96_63)

 DRAW_LINE_192 (SCR_LINE_128, MAP_16X96_64)
 DRAW_LINE_192 (SCR_LINE_130, MAP_16X96_65)
 DRAW_LINE_192 (SCR_LINE_132, MAP_16X96_66)
 DRAW_LINE_192 (SCR_LINE_134, MAP_16X96_67)
 DRAW_LINE_192 (SCR_LINE_136, MAP_16X96_68)
 DRAW_LINE_192 (SCR_LINE_138, MAP_16X96_69)
 DRAW_LINE_192 (SCR_LINE_140, MAP_16X96_70)
 DRAW_LINE_192 (SCR_LINE_142, MAP_16X96_71)

 DRAW_LINE_192 (SCR_LINE_144, MAP_16X96_72)
 DRAW_LINE_192 (SCR_LINE_146, MAP_16X96_73)
 DRAW_LINE_192 (SCR_LINE_148, MAP_16X96_74)
 DRAW_LINE_192 (SCR_LINE_150, MAP_16X96_75)
 DRAW_LINE_192 (SCR_LINE_152, MAP_16X96_76)
 DRAW_LINE_192 (SCR_LINE_154, MAP_16X96_77)
 DRAW_LINE_192 (SCR_LINE_156, MAP_16X96_78)
 DRAW_LINE_192 (SCR_LINE_158, MAP_16X96_79)

 DRAW_LINE_192 (SCR_LINE_160, MAP_16X96_80)
 DRAW_LINE_192 (SCR_LINE_162, MAP_16X96_81)
 DRAW_LINE_192 (SCR_LINE_164, MAP_16X96_82)
 DRAW_LINE_192 (SCR_LINE_166, MAP_16X96_83)
 DRAW_LINE_192 (SCR_LINE_168, MAP_16X96_84)
 DRAW_LINE_192 (SCR_LINE_170, MAP_16X96_85)
 DRAW_LINE_192 (SCR_LINE_172, MAP_16X96_86)
 DRAW_LINE_192 (SCR_LINE_174, MAP_16X96_87)

 DRAW_LINE_192 (SCR_LINE_176, MAP_16X96_88)
 DRAW_LINE_192 (SCR_LINE_178, MAP_16X96_89)
 DRAW_LINE_192 (SCR_LINE_180, MAP_16X96_90)
 DRAW_LINE_192 (SCR_LINE_182, MAP_16X96_91)
 DRAW_LINE_192 (SCR_LINE_184, MAP_16X96_92)
 DRAW_LINE_192 (SCR_LINE_186, MAP_16X96_93)
 DRAW_LINE_192 (SCR_LINE_188, MAP_16X96_94)
 DRAW_LINE_192 (SCR_LINE_190, MAP_16X96_95)

STACK_PTR LD SP,#0000 ; 10 / 3
        RET ; 10 / 1
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

ALIGN $100
MAP_16X16       ; chars copied from map
MAP_16X16_00 DEFB 0,1,2,3,4,5,6,7,8,9,10,11,12,0,0,0 ; 2,3,1,2,3,2,2,3,1,2,1,0,0,0,0
MAP_16X16_01 DEFB 1,0,1,2,1,2,1,2,1,2,1,2,0,0,0,0
MAP_16X16_02 DEFB 1,1,0,2,2,1,1,2,2,1,1,2,0,0,0,0
MAP_16X16_03 DEFB 2,2,2,0,3,3,3,2,2,2,3,3,0,0,0,0
MAP_16X16_04 DEFB 3,3,3,3,0,1,1,1,1,3,3,3,0,0,0,0
MAP_16X16_05 DEFB 2,2,2,2,2,0,3,3,3,3,3,3,0,0,0,0
MAP_16X16_06 DEFB 2,2,2,2,2,2,0,1,1,1,1,1,0,0,0,0
MAP_16X16_07 DEFB 3,3,3,3,3,3,3,0,2,2,2,2,0,0,0,0
MAP_16X16_08 DEFB 2,2,2,2,2,2,2,2,0,1,1,1,0,0,0,0
MAP_16X16_09 DEFB 1,1,1,1,1,1,1,1,1,0,2,2,0,0,0,0
MAP_16X16_10 DEFB 2,2,2,2,2,2,2,2,2,2,0,1,0,0,0,0
MAP_16X16_11 DEFB 3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0
MAP_16X16_12 DEFB 2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0

ALIGN $100


MAP_16X96 ; double lines of words converted from 16x16 map of chars
MAP_16X96_00 DEFS 32,0
MAP_16X96_01 DEFS 32,0
MAP_16X96_02 DEFS 32,0
MAP_16X96_03 DEFS 32,0
MAP_16X96_04 DEFS 32,0
MAP_16X96_05 DEFS 32,0
MAP_16X96_06 DEFS 32,0
MAP_16X96_07 DEFS 32,0
MAP_16X96_08 DEFS 32,0
MAP_16X96_09 DEFS 32,0

MAP_16X96_10 DEFS 32,0
MAP_16X96_11 DEFS 32,0
MAP_16X96_12 DEFS 32,0
MAP_16X96_13 DEFS 32,0
MAP_16X96_14 DEFS 32,0
MAP_16X96_15 DEFS 32,0
MAP_16X96_16 DEFS 32,0
MAP_16X96_17 DEFS 32,0
MAP_16X96_18 DEFS 32,0
MAP_16X96_19 DEFS 32,0

MAP_16X96_20 DEFS 32,0
MAP_16X96_21 DEFS 32,0
MAP_16X96_22 DEFS 32,0
MAP_16X96_23 DEFS 32,0
MAP_16X96_24 DEFS 32,0
MAP_16X96_25 DEFS 32,0
MAP_16X96_26 DEFS 32,0
MAP_16X96_27 DEFS 32,0
MAP_16X96_28 DEFS 32,0
MAP_16X96_29 DEFS 32,0

MAP_16X96_30 DEFS 32,0
MAP_16X96_31 DEFS 32,0
MAP_16X96_32 DEFS 32,0
MAP_16X96_33 DEFS 32,0
MAP_16X96_34 DEFS 32,0
MAP_16X96_35 DEFS 32,0
MAP_16X96_36 DEFS 32,0
MAP_16X96_37 DEFS 32,0
MAP_16X96_38 DEFS 32,0
MAP_16X96_39 DEFS 32,0

MAP_16X96_40 DEFS 32,0
MAP_16X96_41 DEFS 32,0
MAP_16X96_42 DEFS 32,0
MAP_16X96_43 DEFS 32,0
MAP_16X96_44 DEFS 32,0
MAP_16X96_45 DEFS 32,0
MAP_16X96_46 DEFS 32,0
MAP_16X96_47 DEFS 32,0
MAP_16X96_48 DEFS 32,0
MAP_16X96_49 DEFS 32,0

MAP_16X96_50 DEFS 32,0
MAP_16X96_51 DEFS 32,0
MAP_16X96_52 DEFS 32,0
MAP_16X96_53 DEFS 32,0
MAP_16X96_54 DEFS 32,0
MAP_16X96_55 DEFS 32,0
MAP_16X96_56 DEFS 32,0
MAP_16X96_57 DEFS 32,0
MAP_16X96_58 DEFS 32,0
MAP_16X96_59 DEFS 32,0

MAP_16X96_60 DEFS 32,0
MAP_16X96_61 DEFS 32,0
MAP_16X96_62 DEFS 32,0
MAP_16X96_63 DEFS 32,0
MAP_16X96_64 DEFS 32,0
MAP_16X96_65 DEFS 32,0
MAP_16X96_66 DEFS 32,0
MAP_16X96_67 DEFS 32,0
MAP_16X96_68 DEFS 32,0
MAP_16X96_69 DEFS 32,0

MAP_16X96_70 DEFS 32,0
MAP_16X96_71 DEFS 32,0
MAP_16X96_72 DEFS 32,0
MAP_16X96_73 DEFS 32,0
MAP_16X96_74 DEFS 32,0
MAP_16X96_75 DEFS 32,0
MAP_16X96_76 DEFS 32,0
MAP_16X96_77 DEFS 32,0
MAP_16X96_78 DEFS 32,0
MAP_16X96_79 DEFS 32,0

MAP_16X96_80 DEFS 32,0
MAP_16X96_81 DEFS 32,0
MAP_16X96_82 DEFS 32,0
MAP_16X96_83 DEFS 32,0
MAP_16X96_84 DEFS 32,0
MAP_16X96_85 DEFS 32,0
MAP_16X96_86 DEFS 32,0
MAP_16X96_87 DEFS 32,0
MAP_16X96_88 DEFS 32,0
MAP_16X96_89 DEFS 32,0

MAP_16X96_90 DEFS 32,0
MAP_16X96_91 DEFS 32,0
MAP_16X96_92 DEFS 32,0
MAP_16X96_93 DEFS 32,0
MAP_16X96_94 DEFS 32,0
MAP_16X96_95 DEFS 32,0

;MACRO
; ld a,(bc)
; dec c
; ld l,a
; ld d,(hl)
; inc h
; ld e,(hl)
; dec h
; push de
;ENDM

;map
; 16 across (10 used?) x 88 down (80 used?)

; todo - copy 10 byte map src line to 20 byte / 10 word dst line - * 80 down

;build_map
; ld #src_line+00,bc
; ld #dst_line+00,de
; call build_map_line
;...
; ld #src_line+80,bc
; ld #dst_line+80,de
; call build_map_line

;build_map_line
; fill map from bytes to words sp=dest bc=src hl=word table address pointer
; ld spaddr,sp
; ld sp,de
; ld h, HIGH(Pattern_hi)

; #9
; ld a,(bc)     ; 7
; dec c         ; 4
; ld l,a
; ld d,(hl)
; inc h
; ld e,(hl)
; dec h
; push de
; ...
; #0
; ld a,(bc)     ; 7
; dec c         ; 4
; ld l,a
; ld d,(hl)
; inc h
; ld e,(hl)
; dec h
; push de

; ld sp,apaddr
; ret

ALIGN $100
PATTERN_HI
DEFB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
DEFB %00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011,%00000011
DEFB %00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100,%00001100
DEFB %00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111,%00001111
DEFB %00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000,%00110000
DEFB %00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011,%00110011
DEFB %00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100,%00111100
DEFB %00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111,%00111111
DEFB %11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000,%11000000
DEFB %11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011,%11000011
DEFB %11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100,%11001100
DEFB %11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111,%11001111
DEFB %11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000,%11110000
DEFB %11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011,%11110011
DEFB %11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100,%11111100
DEFB %11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111
ALIGN $100
PAAERN_LO
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111
DEFB %00000000,%00000011,%00001100,%00001111,%00110000,%00110011,%00111100,%00111111,%11000000,%11000011,%11001100,%11001111,%11110000,%11110011,%11111100,%11111111

ALIGN $100
CHARSET_00
DEFB %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000

ALIGN $100
CHARSET_01
DEFB %01111111,%00001000,%01111111,%01111111,%01000001,%01111111,%01111111,%01111111,%01111111,%01111111,%01111111,%01111110,%01111111,%01111110,%01111111,%01111111

ALIGN $100
CHARSET_02
DEFB %01000001,%00011000,%00000001,%00000001,%01000001,%01000000,%01000000,%00000001,%01000001,%01000001,%01000001,%01000001,%01000000,%01000001,%01000000,%01000000

ALIGN $100
CHARSET_03
DEFB %01000001,%00001000,%00000001,%00000001,%01000001,%01000000,%01000000,%00000001,%01000001,%01000001,%01000001,%01000001,%01000001,%01000001,%01000000,%01000000

ALIGN $100
CHARSET_04
DEFB %01000001,%00001000,%01111111,%01111111,%01111111,%01111111,%01111111,%00000001,%01111111,%01111111,%01111111,%01111110,%01000000,%01000001,%01111111,%01111111

ALIGN $100
CHARSET_05
DEFB %01000001,%00001000,%01000000,%00000001,%00000001,%00000001,%01000001,%00000001,%01000001,%00000001,%01000001,%01000001,%01000000,%01000001,%01000000,%01000000

ALIGN $100
CHARSET_06
DEFB %01000001,%00001000,%01000000,%00000001,%00000001,%00000001,%01000001,%00000001,%01000001,%00000001,%01000001,%01000001,%01000000,%01000001,%01000000,%01000000

ALIGN $100
CHARSET_07
DEFB %01111111,%01111111,%01111111,%01111111,%00000001,%01111111,%01111111,%00000001,%01111111,%00000001,%01000001,%01111110,%01111111,%01111110,%01111111,%01000000

ALIGN $100
SCROLL_CHARSET_0_LO
 DB 0
 DB 1
 DB 2
 DB 3
 DB 4
 DB 5
 DB 6
 DB 7
 DB 8
 DB 9
 DB 10
 DB 11
 DB 12
 DB 13
 DB 14
 DB 15
 DB 16
 DB 17
 DB 18
 DB 19
 DB 20
 DB 21
 DB 22
 DB 23
 DB 24
 DB 25
 DB 26
 DB 27
 DB 28
 DB 29
 DB 30
 DB 31
 DB 32
 DB 33
 DB 34
 DB 35
 DB 36
 DB 37
 DB 38
 DB 39
 DB 40
 DB 41
 DB 42
 DB 43
 DB 44
 DB 45
 DB 46
 DB 47
 DB 48
 DB 49
 DB 50
 DB 51
 DB 52
 DB 53
 DB 54
 DB 55
 DB 56
 DB 57
 DB 58
 DB 59
 DB 60
 DB 61
 DB 62
 DB 63
 DB 64
 DB 65
 DB 66
 DB 67
 DB 68
 DB 69
 DB 70
 DB 71
 DB 72
 DB 73
 DB 74
 DB 75
 DB 76
 DB 77
 DB 78
 DB 79
 DB 80
 DB 81
 DB 82
 DB 83
 DB 84
 DB 85
 DB 86
 DB 87
 DB 88
 DB 89
 DB 90
 DB 91
 DB 92
 DB 93
 DB 94
 DB 95
 DB 96
 DB 97
 DB 98
 DB 99
 DB 100
 DB 101
 DB 102
 DB 103
 DB 104
 DB 105
 DB 106
 DB 107
 DB 108
 DB 109
 DB 110
 DB 111
 DB 112
 DB 113
 DB 114
 DB 115
 DB 116
 DB 117
 DB 118
 DB 119
 DB 120
 DB 121
 DB 122
 DB 123
 DB 124
 DB 125
 DB 126
 DB 127
 DB 128
 DB 129
 DB 130
 DB 131
 DB 132
 DB 133
 DB 134
 DB 135
 DB 136
 DB 137
 DB 138
 DB 139
 DB 140
 DB 141
 DB 142
 DB 143
 DB 144
 DB 145
 DB 146
 DB 147
 DB 148
 DB 149
 DB 150
 DB 151
 DB 152
 DB 153
 DB 154
 DB 155
 DB 156
 DB 157
 DB 158
 DB 159
 DB 160
 DB 161
 DB 162
 DB 163
 DB 164
 DB 165
 DB 166
 DB 167
 DB 168
 DB 169
 DB 170
 DB 171
 DB 172
 DB 173
 DB 174
 DB 175
 DB 176
 DB 177
 DB 178
 DB 179
 DB 180
 DB 181
 DB 182
 DB 183
 DB 184
 DB 185
 DB 186
 DB 187
 DB 188
 DB 189
 DB 190
 DB 191
 DB 192
 DB 193
 DB 194
 DB 195
 DB 196
 DB 197
 DB 198
 DB 199
 DB 200
 DB 201
 DB 202
 DB 203
 DB 204
 DB 205
 DB 206
 DB 207
 DB 208
 DB 209
 DB 210
 DB 211
 DB 212
 DB 213
 DB 214
 DB 215
 DB 216
 DB 217
 DB 218
 DB 219
 DB 220
 DB 221
 DB 222
 DB 223
 DB 224
 DB 225
 DB 226
 DB 227
 DB 228
 DB 229
 DB 230
 DB 231
 DB 232
 DB 233
 DB 234
 DB 235
 DB 236
 DB 237
 DB 238
 DB 239
 DB 240
 DB 241
 DB 242
 DB 243
 DB 244
 DB 245
 DB 246
 DB 247
 DB 248
 DB 249
 DB 250
 DB 251
 DB 252
 DB 253
 DB 254
 DB 255

SCROLL_CHARSET_0_HI
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0
 DB 0




ALIGN $100
TEMP    DEFS 512,0
STACK   DEFS 512,0 ; stack buffer

MemTop DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.

Zeus_PC                           EQU Start                             ; Tell the emulator where to start.
Zeus_SP                           EQU MemTop                               ; Tell the emulator where to put the stack.

SCR_LINE_000 EQU SCREEN+(0*2048)+(0*256)+(0*32)
SCR_LINE_001 EQU SCREEN+(0*2048)+(1*256)+(0*32)
SCR_LINE_002 EQU SCREEN+(0*2048)+(2*256)+(0*32)
SCR_LINE_003 EQU SCREEN+(0*2048)+(3*256)+(0*32)
SCR_LINE_004 EQU SCREEN+(0*2048)+(4*256)+(0*32)
SCR_LINE_005 EQU SCREEN+(0*2048)+(5*256)+(0*32)
SCR_LINE_006 EQU SCREEN+(0*2048)+(6*256)+(0*32)
SCR_LINE_007 EQU SCREEN+(0*2048)+(7*256)+(0*32)

SCR_LINE_008 EQU SCREEN+(0*2048)+(0*256)+(1*32)
SCR_LINE_009 EQU SCREEN+(0*2048)+(1*256)+(1*32)
SCR_LINE_010 EQU SCREEN+(0*2048)+(2*256)+(1*32)
SCR_LINE_011 EQU SCREEN+(0*2048)+(3*256)+(1*32)
SCR_LINE_012 EQU SCREEN+(0*2048)+(4*256)+(1*32)
SCR_LINE_013 EQU SCREEN+(0*2048)+(5*256)+(1*32)
SCR_LINE_014 EQU SCREEN+(0*2048)+(6*256)+(1*32)
SCR_LINE_015 EQU SCREEN+(0*2048)+(7*256)+(1*32)

SCR_LINE_016 EQU SCREEN+(0*2048)+(0*256)+(2*32)
SCR_LINE_017 EQU SCREEN+(0*2048)+(1*256)+(2*32)
SCR_LINE_018 EQU SCREEN+(0*2048)+(2*256)+(2*32)
SCR_LINE_019 EQU SCREEN+(0*2048)+(3*256)+(2*32)
SCR_LINE_020 EQU SCREEN+(0*2048)+(4*256)+(2*32)
SCR_LINE_021 EQU SCREEN+(0*2048)+(5*256)+(2*32)
SCR_LINE_022 EQU SCREEN+(0*2048)+(6*256)+(2*32)
SCR_LINE_023 EQU SCREEN+(0*2048)+(7*256)+(2*32)

SCR_LINE_024 EQU SCREEN+(0*2048)+(0*256)+(3*32)
SCR_LINE_025 EQU SCREEN+(0*2048)+(1*256)+(3*32)
SCR_LINE_026 EQU SCREEN+(0*2048)+(2*256)+(3*32)
SCR_LINE_027 EQU SCREEN+(0*2048)+(3*256)+(3*32)
SCR_LINE_028 EQU SCREEN+(0*2048)+(4*256)+(3*32)
SCR_LINE_029 EQU SCREEN+(0*2048)+(5*256)+(3*32)
SCR_LINE_030 EQU SCREEN+(0*2048)+(6*256)+(3*32)
SCR_LINE_031 EQU SCREEN+(0*2048)+(7*256)+(3*32)

SCR_LINE_032 EQU SCREEN+(0*2048)+(0*256)+(4*32)
SCR_LINE_033 EQU SCREEN+(0*2048)+(1*256)+(4*32)
SCR_LINE_034 EQU SCREEN+(0*2048)+(2*256)+(4*32)
SCR_LINE_035 EQU SCREEN+(0*2048)+(3*256)+(4*32)
SCR_LINE_036 EQU SCREEN+(0*2048)+(4*256)+(4*32)
SCR_LINE_037 EQU SCREEN+(0*2048)+(5*256)+(4*32)
SCR_LINE_038 EQU SCREEN+(0*2048)+(6*256)+(4*32)
SCR_LINE_039 EQU SCREEN+(0*2048)+(7*256)+(4*32)

SCR_LINE_040 EQU SCREEN+(0*2048)+(0*256)+(5*32)
SCR_LINE_041 EQU SCREEN+(0*2048)+(1*256)+(5*32)
SCR_LINE_042 EQU SCREEN+(0*2048)+(2*256)+(5*32)
SCR_LINE_043 EQU SCREEN+(0*2048)+(3*256)+(5*32)
SCR_LINE_044 EQU SCREEN+(0*2048)+(4*256)+(5*32)
SCR_LINE_045 EQU SCREEN+(0*2048)+(5*256)+(5*32)
SCR_LINE_046 EQU SCREEN+(0*2048)+(6*256)+(5*32)
SCR_LINE_047 EQU SCREEN+(0*2048)+(7*256)+(5*32)

SCR_LINE_048 EQU SCREEN+(0*2048)+(0*256)+(6*32)
SCR_LINE_049 EQU SCREEN+(0*2048)+(1*256)+(6*32)
SCR_LINE_050 EQU SCREEN+(0*2048)+(2*256)+(6*32)
SCR_LINE_051 EQU SCREEN+(0*2048)+(3*256)+(6*32)
SCR_LINE_052 EQU SCREEN+(0*2048)+(4*256)+(6*32)
SCR_LINE_053 EQU SCREEN+(0*2048)+(5*256)+(6*32)
SCR_LINE_054 EQU SCREEN+(0*2048)+(6*256)+(6*32)
SCR_LINE_055 EQU SCREEN+(0*2048)+(7*256)+(6*32)

SCR_LINE_056 EQU SCREEN+(0*2048)+(0*256)+(7*32)
SCR_LINE_057 EQU SCREEN+(0*2048)+(1*256)+(7*32)
SCR_LINE_058 EQU SCREEN+(0*2048)+(2*256)+(7*32)
SCR_LINE_059 EQU SCREEN+(0*2048)+(3*256)+(7*32)
SCR_LINE_060 EQU SCREEN+(0*2048)+(4*256)+(7*32)
SCR_LINE_061 EQU SCREEN+(0*2048)+(5*256)+(7*32)
SCR_LINE_062 EQU SCREEN+(0*2048)+(6*256)+(7*32)
SCR_LINE_063 EQU SCREEN+(0*2048)+(7*256)+(7*32)

SCR_LINE_064 EQU SCREEN+(1*2048)+(0*256)+(0*32)
SCR_LINE_065 EQU SCREEN+(1*2048)+(1*256)+(0*32)
SCR_LINE_066 EQU SCREEN+(1*2048)+(2*256)+(0*32)
SCR_LINE_067 EQU SCREEN+(1*2048)+(3*256)+(0*32)
SCR_LINE_068 EQU SCREEN+(1*2048)+(4*256)+(0*32)
SCR_LINE_069 EQU SCREEN+(1*2048)+(5*256)+(0*32)
SCR_LINE_070 EQU SCREEN+(1*2048)+(6*256)+(0*32)
SCR_LINE_071 EQU SCREEN+(1*2048)+(7*256)+(0*32)

SCR_LINE_072 EQU SCREEN+(1*2048)+(0*256)+(1*32)
SCR_LINE_073 EQU SCREEN+(1*2048)+(1*256)+(1*32)
SCR_LINE_074 EQU SCREEN+(1*2048)+(2*256)+(1*32)
SCR_LINE_075 EQU SCREEN+(1*2048)+(3*256)+(1*32)
SCR_LINE_076 EQU SCREEN+(1*2048)+(4*256)+(1*32)
SCR_LINE_077 EQU SCREEN+(1*2048)+(5*256)+(1*32)
SCR_LINE_078 EQU SCREEN+(1*2048)+(6*256)+(1*32)
SCR_LINE_079 EQU SCREEN+(1*2048)+(7*256)+(1*32)

SCR_LINE_080 EQU SCREEN+(1*2048)+(0*256)+(2*32)
SCR_LINE_081 EQU SCREEN+(1*2048)+(1*256)+(2*32)
SCR_LINE_082 EQU SCREEN+(1*2048)+(2*256)+(2*32)
SCR_LINE_083 EQU SCREEN+(1*2048)+(3*256)+(2*32)
SCR_LINE_084 EQU SCREEN+(1*2048)+(4*256)+(2*32)
SCR_LINE_085 EQU SCREEN+(1*2048)+(5*256)+(2*32)
SCR_LINE_086 EQU SCREEN+(1*2048)+(6*256)+(2*32)
SCR_LINE_087 EQU SCREEN+(1*2048)+(7*256)+(2*32)

SCR_LINE_088 EQU SCREEN+(1*2048)+(0*256)+(3*32)
SCR_LINE_089 EQU SCREEN+(1*2048)+(1*256)+(3*32)
SCR_LINE_090 EQU SCREEN+(1*2048)+(2*256)+(3*32)
SCR_LINE_091 EQU SCREEN+(1*2048)+(3*256)+(3*32)
SCR_LINE_092 EQU SCREEN+(1*2048)+(4*256)+(3*32)
SCR_LINE_093 EQU SCREEN+(1*2048)+(5*256)+(3*32)
SCR_LINE_094 EQU SCREEN+(1*2048)+(6*256)+(3*32)
SCR_LINE_095 EQU SCREEN+(1*2048)+(7*256)+(3*32)

SCR_LINE_096 EQU SCREEN+(1*2048)+(0*256)+(4*32)
SCR_LINE_097 EQU SCREEN+(1*2048)+(1*256)+(4*32)
SCR_LINE_098 EQU SCREEN+(1*2048)+(2*256)+(4*32)
SCR_LINE_099 EQU SCREEN+(1*2048)+(3*256)+(4*32)
SCR_LINE_100 EQU SCREEN+(1*2048)+(4*256)+(4*32)
SCR_LINE_101 EQU SCREEN+(1*2048)+(5*256)+(4*32)
SCR_LINE_102 EQU SCREEN+(1*2048)+(6*256)+(4*32)
SCR_LINE_103 EQU SCREEN+(1*2048)+(7*256)+(4*32)

SCR_LINE_104 EQU SCREEN+(1*2048)+(0*256)+(5*32)
SCR_LINE_105 EQU SCREEN+(1*2048)+(1*256)+(5*32)
SCR_LINE_106 EQU SCREEN+(1*2048)+(2*256)+(5*32)
SCR_LINE_107 EQU SCREEN+(1*2048)+(3*256)+(5*32)
SCR_LINE_108 EQU SCREEN+(1*2048)+(4*256)+(5*32)
SCR_LINE_109 EQU SCREEN+(1*2048)+(5*256)+(5*32)
SCR_LINE_110 EQU SCREEN+(1*2048)+(6*256)+(5*32)
SCR_LINE_111 EQU SCREEN+(1*2048)+(7*256)+(5*32)

SCR_LINE_112 EQU SCREEN+(1*2048)+(0*256)+(6*32)
SCR_LINE_113 EQU SCREEN+(1*2048)+(1*256)+(6*32)
SCR_LINE_114 EQU SCREEN+(1*2048)+(2*256)+(6*32)
SCR_LINE_115 EQU SCREEN+(1*2048)+(3*256)+(6*32)
SCR_LINE_116 EQU SCREEN+(1*2048)+(4*256)+(6*32)
SCR_LINE_117 EQU SCREEN+(1*2048)+(5*256)+(6*32)
SCR_LINE_118 EQU SCREEN+(1*2048)+(6*256)+(6*32)
SCR_LINE_119 EQU SCREEN+(1*2048)+(7*256)+(6*32)

SCR_LINE_120 EQU SCREEN+(1*2048)+(0*256)+(7*32)
SCR_LINE_121 EQU SCREEN+(1*2048)+(1*256)+(7*32)
SCR_LINE_122 EQU SCREEN+(1*2048)+(2*256)+(7*32)
SCR_LINE_123 EQU SCREEN+(1*2048)+(3*256)+(7*32)
SCR_LINE_124 EQU SCREEN+(1*2048)+(4*256)+(7*32)
SCR_LINE_125 EQU SCREEN+(1*2048)+(5*256)+(7*32)
SCR_LINE_126 EQU SCREEN+(1*2048)+(6*256)+(7*32)
SCR_LINE_127 EQU SCREEN+(1*2048)+(7*256)+(7*32)

SCR_LINE_128 EQU SCREEN+(2*2048)+(0*256)+(0*32)
SCR_LINE_129 EQU SCREEN+(2*2048)+(1*256)+(0*32)
SCR_LINE_130 EQU SCREEN+(2*2048)+(2*256)+(0*32)
SCR_LINE_131 EQU SCREEN+(2*2048)+(3*256)+(0*32)
SCR_LINE_132 EQU SCREEN+(2*2048)+(4*256)+(0*32)
SCR_LINE_133 EQU SCREEN+(2*2048)+(5*256)+(0*32)
SCR_LINE_134 EQU SCREEN+(2*2048)+(6*256)+(0*32)
SCR_LINE_135 EQU SCREEN+(2*2048)+(7*256)+(0*32)

SCR_LINE_136 EQU SCREEN+(2*2048)+(0*256)+(1*32)
SCR_LINE_137 EQU SCREEN+(2*2048)+(1*256)+(1*32)
SCR_LINE_138 EQU SCREEN+(2*2048)+(2*256)+(1*32)
SCR_LINE_139 EQU SCREEN+(2*2048)+(3*256)+(1*32)
SCR_LINE_140 EQU SCREEN+(2*2048)+(4*256)+(1*32)
SCR_LINE_141 EQU SCREEN+(2*2048)+(5*256)+(1*32)
SCR_LINE_142 EQU SCREEN+(2*2048)+(6*256)+(1*32)
SCR_LINE_143 EQU SCREEN+(2*2048)+(7*256)+(1*32)

SCR_LINE_144 EQU SCREEN+(2*2048)+(0*256)+(2*32)
SCR_LINE_145 EQU SCREEN+(2*2048)+(1*256)+(2*32)
SCR_LINE_146 EQU SCREEN+(2*2048)+(2*256)+(2*32)
SCR_LINE_147 EQU SCREEN+(2*2048)+(3*256)+(2*32)
SCR_LINE_148 EQU SCREEN+(2*2048)+(4*256)+(2*32)
SCR_LINE_149 EQU SCREEN+(2*2048)+(5*256)+(2*32)
SCR_LINE_150 EQU SCREEN+(2*2048)+(6*256)+(2*32)
SCR_LINE_151 EQU SCREEN+(2*2048)+(7*256)+(2*32)

SCR_LINE_152 EQU SCREEN+(2*2048)+(0*256)+(3*32)
SCR_LINE_153 EQU SCREEN+(2*2048)+(1*256)+(3*32)
SCR_LINE_154 EQU SCREEN+(2*2048)+(2*256)+(3*32)
SCR_LINE_155 EQU SCREEN+(2*2048)+(3*256)+(3*32)
SCR_LINE_156 EQU SCREEN+(2*2048)+(4*256)+(3*32)
SCR_LINE_157 EQU SCREEN+(2*2048)+(5*256)+(3*32)
SCR_LINE_158 EQU SCREEN+(2*2048)+(6*256)+(3*32)
SCR_LINE_159 EQU SCREEN+(2*2048)+(7*256)+(3*32)

SCR_LINE_160 EQU SCREEN+(2*2048)+(0*256)+(4*32)
SCR_LINE_161 EQU SCREEN+(2*2048)+(1*256)+(4*32)
SCR_LINE_162 EQU SCREEN+(2*2048)+(2*256)+(4*32)
SCR_LINE_163 EQU SCREEN+(2*2048)+(3*256)+(4*32)
SCR_LINE_164 EQU SCREEN+(2*2048)+(4*256)+(4*32)
SCR_LINE_165 EQU SCREEN+(2*2048)+(5*256)+(4*32)
SCR_LINE_166 EQU SCREEN+(2*2048)+(6*256)+(4*32)
SCR_LINE_167 EQU SCREEN+(2*2048)+(7*256)+(4*32)

SCR_LINE_168 EQU SCREEN+(2*2048)+(0*256)+(5*32)
SCR_LINE_169 EQU SCREEN+(2*2048)+(1*256)+(5*32)
SCR_LINE_170 EQU SCREEN+(2*2048)+(2*256)+(5*32)
SCR_LINE_171 EQU SCREEN+(2*2048)+(3*256)+(5*32)
SCR_LINE_172 EQU SCREEN+(2*2048)+(4*256)+(5*32)
SCR_LINE_173 EQU SCREEN+(2*2048)+(5*256)+(5*32)
SCR_LINE_174 EQU SCREEN+(2*2048)+(6*256)+(5*32)
SCR_LINE_175 EQU SCREEN+(2*2048)+(7*256)+(5*32)

SCR_LINE_176 EQU SCREEN+(2*2048)+(0*256)+(6*32)
SCR_LINE_177 EQU SCREEN+(2*2048)+(1*256)+(6*32)
SCR_LINE_178 EQU SCREEN+(2*2048)+(2*256)+(6*32)
SCR_LINE_179 EQU SCREEN+(2*2048)+(3*256)+(6*32)
SCR_LINE_180 EQU SCREEN+(2*2048)+(4*256)+(6*32)
SCR_LINE_181 EQU SCREEN+(2*2048)+(5*256)+(6*32)
SCR_LINE_182 EQU SCREEN+(2*2048)+(6*256)+(6*32)
SCR_LINE_183 EQU SCREEN+(2*2048)+(7*256)+(6*32)

SCR_LINE_184 EQU SCREEN+(2*2048)+(0*256)+(7*32)
SCR_LINE_185 EQU SCREEN+(2*2048)+(1*256)+(7*32)
SCR_LINE_186 EQU SCREEN+(2*2048)+(2*256)+(7*32)
SCR_LINE_187 EQU SCREEN+(2*2048)+(3*256)+(7*32)
SCR_LINE_188 EQU SCREEN+(2*2048)+(4*256)+(7*32)
SCR_LINE_189 EQU SCREEN+(2*2048)+(5*256)+(7*32)
SCR_LINE_190 EQU SCREEN+(2*2048)+(6*256)+(7*32)
SCR_LINE_191 EQU SCREEN+(2*2048)+(7*256)+(7*32)


