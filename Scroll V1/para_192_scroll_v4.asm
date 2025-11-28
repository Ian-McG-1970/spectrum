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

; BORDER (2)
 CALL DRAW_SCREEN_192
; BORDER (4)

 CALL BUILD_SCREEN_LINES_FROM_CHAR_MAP_SCROLL_V3 ;1 ;3
; BORDER (3)
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

 LD A, (SCROLL_HOR)
 DEC A
 LD (SCROLL_HOR),A
; OUT (254),A

 JP MAIN_LOOP

;COLOUR DB 0


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

SCREEN_LINE_BYTE_TO_WORD_MAC_START MACRO ()
        LD A,(BC)     ; #11
        DEC C
        LD L,A
        LD E,(HL)
        INC H
        LD D,(HL)
        PUSH DE
        DEC H
ENDM

SCREEN_LINE_BYTE_TO_WORD PROC
        LD (STACK_PTR +1),SP
        LD      SP, IX

        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #11
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #10
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #9
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #8
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #7
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #6
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #5
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #4
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #3
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #2
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #1
        SCREEN_LINE_BYTE_TO_WORD_MAC_START ()   ; #0

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



; get the char from the map (1)
; get the byte for the line for that char (2)
; get the low char pos (3)
; or it with a (4)
; store in the map line (5)
; get the high char pos in a (6)
; next map line pos (7)
; next loop (8)


CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 MACRO ()
        LD C, (HL)      ; 7 ; get the char from the map (1)
        INC L                   ; 4
        LD A, (BC)      ; 7 ; get the byte for the line for that char (2)
        LD E, A                 ; 4
        LD A, (DE)      ; 7 ; get the low char pos (3)
        LD C, A                 ; 4
        EX AF,AF'               ; 4
        OR A, C         ; 4 ; or it with a (4)
        EXX                             ; 4
        LD (HL), A      ; 7 ; store in the map line (5)
        INC L          ; 4 ; next map line pos (7)
        EXX                             ; 4
        INC D                   ; 4
        LD A, (DE)      ; 7 ; get the high char pos in a (6)
        EX AF,AF'               ; 4
        DEC D                   ; 4
ENDM

CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V2 MACRO ()
        LD A, (DE)      ; 7 ; get the char from the map (1)
        INC E                   ; 4
                LD C, A                 ; 4
        LD A, (BC)      ; 7 get the byte for the line for that char (2)
        LD L, A                 ; 4
        EX AF,AF                ; 4
        OR A, (HL)              ; 7         ; or it with a (4)
        EXX                             ; 4
        LD (HL), A      ; 7 ; store in the map line (5)
        INC L          ; 6 next map line pos (7)
        EXX                             ; 4
        INC B                   ; 4
        LD A, (BC)      ; 7 get the high char pos in a (6)
        EX AF,AF'               ; 4
        DEC B                   ; 4
ENDM

CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 MACRO () ; hl=char map / d=char line / bc'=screen / h'=scroll tab
        LD E, (HL)      ; 7 ; get the char from the map (1)
        INC L                   ; 4
        LD A, (DE)      ; 7 get the byte for the line for that char (2)
        EXX                             ; 4
        LD L, A                 ; 4
        EX AF,AF'               ; 4
        OR A, (HL)              ; 7         ; or it with a (4)
        LD (BC), A      ; 7 ; store in the map line (5)
        INC C          ; 6 next map line pos (7) (INC C ?)
        INC H                   ; 4
        LD A, (HL)      ; 7 get the high char pos in a (6)
        EX AF,AF'               ; 4
        DEC H                   ; 4
        EXX                             ; 4
ENDM

CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1 PROC     ;HL= map address (12x12) ;B= address of the vertical position (0-7) of the 255 values for the 255 chars ; D= address of the 255 chars for the scroll position (0-7) ;HL'= map screen address (24x96)

        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #0
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #1
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #2
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #3
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #4
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #5
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #6
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #7
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #8
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #9
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #10
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #11
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V1 ()  ; char #12
        RET
ENDP

CHAR_MAP_TO_SCREEN_LINE_MAC MACRO ()
        LD      C, (HL) ; char #1
        LD      A, (BC)
        LD (DE), A
ENDM

SCROLL_HOR DB 0

BUILD_SCREEN_LINES_FROM_CHAR_MAP_SCROLL_V1 PROC
        LD A, (SCROLL_HOR)

        AND A,  7
        ADD A,  A
        ADD A,  HIGH SCROLL_CHARSET_0_HI
        LD D,   A

        LD      B, HIGH CHARSET_00

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_00
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_08
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_16
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_24
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_32
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_40
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_48
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_56
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_64
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_72
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_80
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_88
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_01

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_01
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_09
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_17
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_25
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_33
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_41
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_49
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_57
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_65
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_73
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_81
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_89
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_02

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_02
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_10
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_18
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_26
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_34
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_42
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_50
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_58
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_66
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_74
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_82
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_90
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_03

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_03
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_11
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_19
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_27
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_35
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_43
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_51
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_59
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_67
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_75
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_83
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_91
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_04

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_04
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_12
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_20
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_28
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_36
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_44
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_52
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_60
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_68
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_76
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_84
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_92
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_05

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_05
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_13
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_21
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_29
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_37
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_45
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_53
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_61
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_69
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_77
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_85
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_93
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_06

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_06
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_14
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_22
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_30
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_38
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_46
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_54
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_62
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_70
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_78
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_86
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_94
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        LD      B, HIGH CHARSET_07

        LD      HL, MAP_16X16_00
        EXX
        LD      HL, MAP_16X96_07
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_01
        EXX
        LD      HL, MAP_16X96_15
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_02
        EXX
        LD      HL, MAP_16X96_23
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_03
        EXX
        LD      HL, MAP_16X96_31
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_04
        EXX
        LD      HL, MAP_16X96_39
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_05
        EXX
        LD      HL, MAP_16X96_47
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_06
        EXX
        LD      HL, MAP_16X96_55
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_07
        EXX
        LD      HL, MAP_16X96_63
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_08
        EXX
        LD      HL, MAP_16X96_71
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_09
        EXX
        LD      HL, MAP_16X96_79
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_10
        EXX
        LD      HL, MAP_16X96_87
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1
        LD      HL, MAP_16X16_11
        EXX
        LD      HL, MAP_16X96_95
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V1

        RET
ENDP

CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3 PROC     ;HL= map address (12x12) ;B= address of the vertical position (0-7) of the 255 values for the 255 chars ; D= address of the 255 chars for the scroll position (0-7) ;HL'= map screen address (24x96)

        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #0
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #1
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #2
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #3
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #4
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #5
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #6
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #7
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #8
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #9
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #10
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #11
        CHAR_MAP_TO_SCREEN_LINE_SCROLL_MAC_V3 ()  ; char #12
        RET
ENDP

BUILD_SCREEN_LINES_FROM_CHAR_MAP_SCROLL_V3 PROC
        LD A, (SCROLL_HOR)

        AND A,  7
        ADD A,  A
        ADD A,  HIGH SCROLL_CHARSET_0_HI
        LD H,   A
        EXX             ; b'=scroll tab

        LD      D, HIGH CHARSET_00

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_00
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3 ; hl=map / d=char line / bc'=screen / h'=scroll tab

        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_08
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_16
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_24
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_32
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_40
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_48
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_56
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_64
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_72
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_80
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_88
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_01

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_01
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_09
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_17
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_25
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_33
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_41
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_49
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_57
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_65
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_73
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_81
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_89
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_02

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_02
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_10
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_18
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_26
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_34
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_42
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_50
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_58
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_66
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_74
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_82
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_90
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_03

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_03
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_11
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_19
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_27
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_35
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_43
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_51
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_59
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_67
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_75
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_83
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_91
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_04

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_04
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_12
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_20
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_28
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_36
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_44
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_52
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_60
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_68
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_76
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_84
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_92
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_05

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_05
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_13
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_21
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_29
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_37
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_45
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_53
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_61
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_69
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_77
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_85
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_93
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_06

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_06
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_14
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_22
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_30
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_38
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_46
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_54
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_62
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_70
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_78
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_86
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_94
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

        LD      D, HIGH CHARSET_07

        LD      HL, MAP_16X16_00
        EXX
        LD      BC, MAP_16X96_07
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_01
        EXX
        LD      BC, MAP_16X96_15
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_02
        EXX
        LD      BC, MAP_16X96_23
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_03
        EXX
        LD      BC, MAP_16X96_31
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_04
        EXX
        LD      BC, MAP_16X96_39
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_05
        EXX
        LD      BC, MAP_16X96_47
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_06
        EXX
        LD      BC, MAP_16X96_55
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_07
        EXX
        LD      BC, MAP_16X96_63
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_08
        EXX
        LD      BC, MAP_16X96_71
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_09
        EXX
        LD      BC, MAP_16X96_79
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_10
        EXX
        LD      BC, MAP_16X96_87
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3
        LD      HL, MAP_16X16_11
        EXX
        LD      BC, MAP_16X96_95
        EXX
        CALL    CHAR_MAP_TO_SCREEN_LINE_SCROLL_V3

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
SCROLL_CHARSET_0_HI
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

SCROLL_CHARSET_0_LO
 DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
 DB 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,46,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
 DB 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
 DB 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
 DB 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
 DB 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
 DB 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,206,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
 DB 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255

SCROLL_CHARSET_1_HI
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

SCROLL_CHARSET_1_LO
 DB 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62
 DB 64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,92,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126
 DB 128,130,132,134,136,138,140,142,144,146,148,150,152,154,156,158,160,162,164,166,168,170,172,174,176,178,180,182,184,186,188,190
 DB 192,194,196,198,200,202,204,206,208,210,212,214,216,218,220,222,224,226,228,230,232,234,236,238,240,242,244,246,248,250,252,254
 DB 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62
 DB 64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126
 DB 128,130,132,134,136,138,140,142,144,146,148,150,152,154,156,156,160,162,164,166,168,170,172,174,176,178,180,182,184,186,188,190
 DB 192,194,196,198,200,202,204,206,208,210,212,214,216,218,220,222,224,226,228,230,232,234,236,238,240,242,244,246,248,250,252,254

SCROLL_CHARSET_2_HI
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
 DB 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
 DB 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
 DB 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3

SCROLL_CHARSET_2_LO
 DB 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124
 DB 128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,184,192,196,200,204,208,212,216,220,224,228,232,236,240,244,248,252
 DB 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124
 DB 128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,188,192,196,200,204,208,212,216,220,224,228,232,236,240,244,248,252
 DB 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124
 DB 128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,188,192,196,200,204,208,212,216,220,224,228,232,236,240,244,248,252
 DB 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,56,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124
 DB 128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,188,192,196,200,204,208,212,216,220,224,228,232,236,240,244,248,252

SCROLL_CHARSET_3_HI
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 DB 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
 DB 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
 DB 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
 DB 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
 DB 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
 DB 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7

SCROLL_CHARSET_3_LO
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,112,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,112,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248
 DB 0,8,16,24,32,40,48,56,64,72,80,88,96,104,112,120,128,136,144,152,160,168,176,184,192,200,208,216,224,232,240,248

SCROLL_CHARSET_4_HI
 DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
 DB 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
 DB 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
 DB 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
 DB 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
 DB 10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11
 DB 12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13
 DB 14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15

SCROLL_CHARSET_4_LO
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,224,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,224,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240
 DB 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240,0,16,32,48,64,80,96,112,128,144,160,176,192,208,224,240

SCROLL_CHARSET_5_HI
 DB 0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3
 DB 4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7
 DB 8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11
 DB 12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,14,15,15,15,15,15,15,15,15
 DB 16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,19
 DB 20,20,20,20,20,20,20,20,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23
 DB 24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27
 DB 28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,31,31,31,31,31,31,31,31

SCROLL_CHARSET_5_LO
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,192,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,192,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224
 DB 0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224,0,32,64,96,128,160,192,224

SCROLL_CHARSET_6_HI
 DB 0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7
 DB 8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15
 DB 16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23
 DB 24,24,24,24,25,25,25,25,26,26,26,26,27,27,27,27,28,28,28,28,29,29,29,29,30,30,30,30,31,31,31,31
 DB 32,32,32,32,33,33,33,33,34,34,34,34,35,35,35,35,36,36,36,36,37,37,37,37,38,38,38,38,39,39,39,39
 DB 40,40,40,40,41,41,41,41,42,42,42,42,43,43,43,43,44,44,44,44,45,45,45,45,46,46,46,46,47,47,47,47
 DB 48,48,48,48,49,49,49,49,50,50,50,50,51,51,51,51,52,52,52,52,53,53,53,53,54,54,54,54,55,55,55,55
 DB 56,56,56,56,57,57,57,57,58,58,58,58,59,59,59,59,60,60,60,60,61,61,61,61,62,62,62,62,63,63,63,63

SCROLL_CHARSET_6_LO
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,128,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,128,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192
 DB 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192

SCROLL_CHARSET_7_HI
 DB 0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15
 DB 16,16,17,17,18,18,19,19,20,20,21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,29,29,30,30,31,31
 DB 32,32,33,33,34,34,35,35,36,36,37,37,38,38,39,39,40,40,41,41,42,42,43,43,44,44,45,45,46,46,47,47
 DB 48,48,49,49,50,50,51,51,52,52,53,53,54,54,55,55,56,56,57,57,58,58,59,59,60,60,61,61,62,62,63,63
 DB 64,64,65,65,66,66,67,67,68,68,69,69,70,70,71,71,72,72,73,73,74,74,75,75,76,76,77,77,78,78,79,79
 DB 80,80,81,81,82,82,83,83,84,84,85,85,86,86,87,87,88,88,89,89,90,90,91,91,92,92,93,93,94,94,95,95
 DB 96,96,97,97,98,98,99,99,100,100,101,101,102,102,103,103,104,104,105,105,106,106,107,107,108,108,109,109,110,110,111,111
 DB 112,112,113,113,114,114,115,115,116,116,117,117,118,118,119,119,120,120,121,121,122,122,123,123,124,124,125,125,126,126,127,127

SCROLL_CHARSET_7_LO
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,0,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,0,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128
 DB 0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128,0,128

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


