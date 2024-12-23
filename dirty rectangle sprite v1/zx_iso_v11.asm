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

START
 DI                      ; interrupts off
 LD SP,MEMTOP            ; set stack to end STACK?

 LD HL,ATTRIB            ; attr start                             ; Clear attributes
 LD DE,ATTRIB+1          ; sttr start +1
 LD BC,ATTRIB_SIZE-1     ; attr size -1
 LD (HL),7               ; clear first attr to white
 LDIR                    ; copy

 LD HL,SCREEN            ; attr start                             ; Clear screen
 LD DE,SCREEN+1          ; sttr start +1
 LD BC,SCREEN_SIZE-1     ; attr size -1
 LD (HL),0               ; clear first attr to white
 LDIR                    ; copy

 LD HL,BCK_BUF            ; attr start                             ; Clear screen
 LD DE,BCK_BUF+1          ; sttr start +1
 LD BC,SCREEN_SIZE-1     ; attr size -1
 LD (HL),255               ; clear first attr to white
 LDIR                    ; copy

 LD HL,0                ; src is rom                                    ; random back buffer
 LD DE,BCK_BUF          ; dst is back buffer
 LD BC,SCREEN_SIZE      ; size is screen size
 LDIR                    ; copy

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

; LD A,3         ; bottom three bits of A contain the border color
; OUT (254),A

MAIN_LOOP
 CALL V_BLANK

 LD A,4         ; bottom three bits of A contain the border color
 OUT (254),A

 LD A,(pv)
 LD L,A
 LD A,(ph)
 LD E,A
 CALL BB_TB_32X40  ; SB_TB_6X6

 LD A,(ph)
 LD e, a
 LD A,(pv)
 LD c, A
 LD HL,TEST_AND_OR_SPR00
 CALL SPRITE_AND_OR_3240

 LD A,(pv)
 LD L,A
 LD A,(ph)
 LD E,A
 CALL TB_SN_32X40 ; TB_SN_6X6

 LD A,3         ; bottom three bits of A contain the border color
 OUT (254),A

 CALL KEYBOARD
JP MAIN_LOOP

sttx defb 12
stty defb 12
endx defb 95
endy defb 97
sdx defb 1
sdy defb 1
edx defb 1
edy defb 1

ph defb 10
pv defb 19 ; 96 20 to 51 = 32 / 19 to 49 = 32


; keys = port bit 0   1   2 3 4
;        fefe     sht z   x c v
;        fdfe     a   s   d f g
;        fbfe     q   w   e r t
;        f7fe     1   2   3 4 5
;        effe     0   9   8 7 6
;        dffe     p   o   1 u y
;        bffe     ent l   k j h
;        7ffe     spc sym m n b

KEYBOARD ; 1aqp

 LD BC,$FBFE     ; Load BC with the row port address
 IN A,(C)        ; Read the port into the accumulator
 AND $01         ; q
 JP NZ,Q_KEY_N   ; not pressed
  LD A, (ph)
  DEC A
  LD (ph), A
Q_KEY_N

 LD BC,$DFFE
 IN A,(C)
 AND $01         ; p
 JP NZ,P_KEY_N
  LD A, (ph)
  INC A
  LD (ph), A
P_KEY_N

  LD BC,$FDFE
 IN A,(C)
 AND $01         ; a
 JP NZ,Z_KEY_N
  LD A, (pv)
  INC A
  LD (pv), A
Z_KEY_N

 LD BC,$f7FE
 IN A,(C)
 AND $01         ; 1
 JP NZ,M_KEY_N
  LD A, (pv)
  DEC A
  LD (pv), A
M_KEY_N
 RET

V_BLANK:
 LD DE,$1140       ; attr into D, MSB of port addr into E
FB_LP
  INC HL          ; padding instruction
  LD A,E          ; MSB of port addr into A
  IN A,($ff)      ; read port 0x40FF into A
  CP D            ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
  JP NZ,FB_LP     ; no? keep trying
RET

NEXT_LINE_LDI_LDD MACRO () ; going from forwards (ldi) on this line to backwards (ldd) on next line
 DEC E
 DEC L
 INC H ; next src line down
 INC D ; next dst line down
MEND

NEXT_LINE_LDD_LDI MACRO () ; going from backwards (ldd) on this line to forwards (ldi) on next line
 INC E
 INC L
 INC H ; next src line down
 INC D ; next dst line down
MEND

COPY_LINES_0240_EVEN MACRO () ; copy 2 linse of 40 pixels (5 bytes) in a zigzag from start to end on first line and end to start on second line
 LDI ; #16
 LDI
 LDI
 LDI
 LDI
 NEXT_LINE_LDI_LDD () ; #16
 LDD ; #16
 LDD
 LDD
 LDD
 LDD
MEND

COPY_LINE_0140_ODD MACRO () ; copy 1 linse of 40 pixels (5 bytes) from start to end on first line
 LDI ; #16
 LDI
 LDI
 LDI
 LDI
MEND

BCK_TMP_BUF_POS_START_EVEN MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor
 LD A, E                       ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH TMP_BUF_EVEN_TAB_HI      ; #7
 LD D, (HL)                     ; TMP BUF HI #7
 INC H                          ; #4
 ADD A, (HL)                    ; TMP BUF LO #7
 INC H                          ; #4
 LD H, (HL)                     ; BCK BUF HI #7
 LD E, A                        ; TMP BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; BCK BUF LO #4
MEND

BCK_TMP_BUF_POS_START_ODD MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor
 LD A, E                       ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH TMP_BUF_ODD_TAB_HI  ; #7
 LD D, (HL)                     ; TMP BUF HI #7
 INC H                          ; #4
 ADD A, (HL)                    ; TMP BUF LO #7
 INC H                          ; #4
 LD H, (HL)                     ; BCK BUF HI #7
 LD E, A                        ; TMP BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; BCK BUF LO #4
MEND


BCK_TMP_BUF_POS_NEXT_LINE_EVEN MACRO () ; setup hl/de for ldi/ldd copies - A=ver A'=hor
 INC A                            ; move backup of ver 2 lines down    #4
 LD L, A                          ; restore L=ver                                              #4 8
 EX AF,AF'                                                      ;                                                                       #4 12
 LD E, A                           ; restore E=hor                                              #4 16
 EX AF,AF'                         ; backup E=hor                                               #4 20

 LD D, HIGH SCN_BUF_TAB_H       ;                                                                               #7 27
 LD A, (DE)                     ; HOR                                                                   #7 34

 LD H, HIGH TMP_BUF_EVEN_TAB_HI      ;                                                                               #7 41
 LD D, (HL)                     ; TMP BUF HI                                                    #7 48
 INC H                          ;                                                                               #4 52
 ADD A, (HL)                    ; TMP BUF LO                                                    #7 59
 INC H                          ;                                                                               #4 63
 LD H, (HL)                     ; BCK BUF HI                                                    #7 70
 LD E, A                        ; TMP BUF LO                                                    #4 74

 LD A, L                         ; backup L=ver                                                 #4 78

 LD L, E                        ; BCK BUF LO                                                    #4 82
MEND

BCK_TMP_BUF_POS_NEXT_LINE_ODD MACRO () ; setup hl/de for ldi/ldd copies - A=ver A'=hor
 INC A ;;                           ; move backup of ver 2 lines down    #4
 LD L, A                          ; restore L=ver                                              #4 8
 EX AF,AF'                                                      ;                                                                       #4 12
 LD E, A                           ; restore E=hor                                              #4 16
 EX AF,AF'                         ; backup E=hor                                               #4 20

 LD D, HIGH SCN_BUF_TAB_H       ;                                                                               #7 27
 LD A, (DE)                     ; HOR                                                                   #7 34

 LD H, HIGH TMP_BUF_ODD_TAB_HI      ;                                                                               #7 41
 LD D, (HL)                     ; TMP BUF HI                                                    #7 48
 INC H                          ;                                                                               #4 52
 ADD A, (HL)                    ; TMP BUF LO                                                    #7 59
 INC H                          ;                                                                               #4 63
 LD H, (HL)                     ; BCK BUF HI                                                    #7 70
 LD E, A                        ; TMP BUF LO                                                    #4 74

 LD A, L                         ; backup L=ver                                                 #4 78

 LD L, E                        ; BCK BUF LO                                                    #4 82
MEND


TMP_SCN_BUF_POS_START_EVEN MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor

 LD A, E                                                ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH SCN_BUF_EVEN_TAB_HI      ; #7
 LD D, (HL)                     ; SCN BUF HI #7
 LD H, HIGH BCK_BUF_EVEN_TAB_LO
; DEC H                          ; #4 ; ???? replace with ld high to save 1 cycle?
; dec h
 ADD A, (HL)                    ; SCN BUF LO #7
 DEC H                          ; #4
 LD H, (HL)                     ; TMP BUF HI #7
 LD E, A                        ; SCN BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; TMP BUF LO #4
MEND

TMP_SCN_BUF_POS_START_ODD MACRO () ; setup hl/de for ldi/ldd copies - L=ver E=hor

 LD A, E                                                ; backup E=hor
 EX AF,AF'

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH SCN_BUF_ODD_TAB_HI      ; #7
 LD D, (HL)                     ; SCN BUF HI #7
 LD H, HIGH BCK_BUF_ODD_TAB_LO
; DEC H                          ; #4 ; ???? replace with ld high to save 1 cycle? BCK_BUF_ODD_TAB_LO
; dec h
 ADD A, (HL)                    ; SCN BUF LO #7
 DEC H                          ; #4
 LD H, (HL)                     ; TMP BUF HI #7
 LD E, A                        ; SCN BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; TMP BUF LO #4
MEND

TMP_SCN_BUF_POS_NEXT_LINE_EVEN MACRO () ; setup hl/de for ldi/ldd copies - A=ver A'=hor
 INC A                          ; move backup of ver 2 lines down
 LD L, A                           ; restore L=ver
 EX AF,AF'
 LD E, A                           ; restore E=hor
 EX AF,AF'                         ; backup E=hor

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH SCN_BUF_EVEN_TAB_HI      ; #7
 LD D, (HL)                     ; SCN BUF HI #7
 LD H, HIGH BCK_BUF_EVEN_TAB_LO
; DEC H                          ; #4 ; ???? replace with ld high to save 1 cycle?
; dec h
 ADD A, (HL)                    ; SCN BUF LO #7
 DEC H                          ; #4
 LD H, (HL)                     ; TMP BUF HI #7
 LD E, A                        ; SCN BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; TMP BUF LO #4
MEND

TMP_SCN_BUF_POS_NEXT_LINE_ODD MACRO () ; setup hl/de for ldi/ldd copies - A=ver A'=hor
 INC A  ;;                        ; move backup of ver 2 lines down
 LD L, A                           ; restore L=ver
 EX AF,AF'
 LD E, A                           ; restore E=hor
 EX AF,AF'                         ; backup E=hor

 LD D, HIGH SCN_BUF_TAB_H       ; #7
 LD A, (DE)                     ; HOR #7

 LD H, HIGH SCN_BUF_ODD_TAB_HI      ; #7
 LD D, (HL)                     ; SCN BUF HI #7
 LD H, HIGH BCK_BUF_ODD_TAB_LO
; DEC H                          ; #4 ; ???? replace with ld high to save 1 cycle?
; dec h
 ADD A, (HL)                    ; SCN BUF LO #7
 DEC H                          ; #4
 LD H, (HL)                     ; TMP BUF HI #7
 LD E, A                        ; SCN BUF LO #4

 LD A, L                                                ; backup L=ver

 LD L, E                        ; TMP BUF LO #4
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

BB_TB_32X40 PROC ; copy from back buffer to temp buffer - L=ver E=hor

        SRL L
        JP  C, DRAW_ODD

DRAW_EVEN:
        BCK_TMP_BUF_POS_START_EVEN ()               ;0
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;2
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;4
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;6
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;8
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;10
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;12
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;14
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;16
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;18
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;20
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;22
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;24
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;26
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;28
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;30
        COPY_LINES_0240_EVEN ()

        RET
DRAW_ODD:
        BCK_TMP_BUF_POS_START_ODD ()            ;0
        COPY_LINE_0140_ODD ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;1
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;3
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;5
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;7
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;9
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;11
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;13
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;15
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;17
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;19
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;21
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;23
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;25
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;27
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;29
        COPY_LINES_0240_EVEN ()
        BCK_TMP_BUF_POS_NEXT_LINE_EVEN ()   ;31 ;        BCK_TMP_BUF_POS_NEXT_LINE_ODD ()        ;31
        COPY_LINE_0140_ODD ()
        RET
        ENDP

TB_SN_32X40 PROC ; copy from temp buffer to screen - L=ver E=hor

        SRL L
        JP  C, DRAW_ODD

DRAW_EVEN:
        TMP_SCN_BUF_POS_START_EVEN ()               ;0
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;2
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;4
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;6
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;8
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;10
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;12
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;14
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;16
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;18
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;20
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;22
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;24
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;26
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;28
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;30
        COPY_LINES_0240_EVEN ()
        RET

DRAW_ODD:
        TMP_SCN_BUF_POS_START_ODD ()
        COPY_LINE_0140_ODD ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;1
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;3
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;5
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;7
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;9
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;11
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;13
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;15
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;17
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;19
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;21
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;23
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;25
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;27
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;29
        COPY_LINES_0240_EVEN ()
        TMP_SCN_BUF_POS_NEXT_LINE_EVEN ()   ;31 ; TMP_SCN_BUF_POS_NEXT_LINE_ODD ()        ;31
        COPY_LINE_0140_ODD ()
        RET
                ENDP

SPR_LINE_EVEN MACRO ()
 LD H, HIGH  BCK_BUF_EVEN_TAB_LO ; ScrBufEvenL     ; SCREEN V TABLE LO  #7 7 ????
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18
 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND

SPR_LINE_EVEN_ODD_40 MACRO ()
 LD H, HIGH  BCK_BUF_EVEN_TAB_LO ; ScrBufEvenL     ; SCREEN V TABLE LO  #7 7 ????
 LD L, C            ; VPOS                       #4 11
 LD A, (HL)         ; LO BYTE POS                #7 18

; ADD A,4                        ; for odd lines start at end of line and update backwards

 DEC H              ;                            #4 22
 LD H, (HL)         ; SCREEN V TABLE HI          #7 29
 ADD A, B           ; LO BYTE POS + HOR BYTE POS #4 33
 LD L, A            ;                            #4 37
MEND

SPR_AND_OR MACRO ()
        POP     DE      ; SPRITE AND_OR
        LD      A, (HL) ; GET SCREEN BYTE
        AND     e ;d ;e ;D
        OR      d ;e ;d ;E
        LD      (HL), A ; PUT SCREEN BYTE
ENDM

SPR_AND_OR_FORWARDS_BACKAWRDS_40 MACRO ()
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC     H
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
MEND

SPR_AND_OR_BACKWARDS_FORWARDS_40 MACRO ()
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        INC     H
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
MEND

SPR_AND_OR_FORWARDS_40 MACRO ()
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
        INC    L
        SPR_AND_OR ()
MEND

SPR_AND_OR_BACKWARDS_40 MACRO ()
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
        DEC    L
        SPR_AND_OR ()
MEND

SPRITE_AND_OR_3240  PROC    ; C=ver - E=hor - HL = sprite table ADDRESS
                    LD D, HIGH ScrBufSprISOY  ; NO OF BYTES TO JUMP FORWARD to point to correct sprite definition             #4
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

                    LD  H, HIGH SCN_BUF_TAB_H                 ; HOR BYTES
                    LD  L, E                            ; HOR
                    LD  B, (HL)                         ; C=HOR BYTE POS

                    SPR_LINE_EVEN () ; 0
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 2
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 4
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 6
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 8
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 10
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 12
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 14
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 16
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 18
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 20
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 22
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 24
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 26
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 28
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()
                    INC C
                    SPR_LINE_EVEN () ; 30
                    SPR_AND_OR_FORWARDS_BACKAWRDS_40 ()

STACK_EVEN          LD SP, $0000
                    RET

SPR_ODD:            LD (STACK_ODD+1), SP ; store sp
                    LD SP, HL                     ; SP = SPRITE ADDRESS

                    LD H, HIGH SCN_BUF_TAB_H            ; HOR BYTES
                    LD L, E                        ; HOR
                    LD B,(HL)                     ; C=HOR BYTE POS

                    SPR_LINE_ODD () ; 0
                    SPR_AND_OR_FORWARDS_40 ()
                    INC C

    ld a,4
        add a,b
        ld b,a

                    SPR_LINE_EVEN_ODD_40 () ; 1
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 3
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 5
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 7
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 9
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 11
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 13
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 15
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 17
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 19
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 21
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 23
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 25
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 27
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 29
                    SPR_AND_OR_BACKWARDS_FORWARDS_40 ()
                    INC C

                    SPR_LINE_EVEN_ODD_40 () ; 31
                                        SPR_AND_OR_BACKWARDS_40 ()

STACK_ODD           LD SP, $0000
                    RET
                    ENDP


ALIGN $100
TEST_AND_OR_SPR00 defw TEST_AND_OR_SPR01,TEST_AND_OR_SPR02,TEST_AND_OR_SPR03,TEST_AND_OR_SPR04

ALIGN $100
SCCH2
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))/256
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))/256

ALIGN $100
SCCL2
 DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(0*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(1*2048)+(0*256)+(7*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(0*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(1*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(2*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(3*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(4*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(5*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(6*32))&255
 DEFB (SCREEN+(2*2048)+(0*256)+(7*32))&255

ALIGN $100
SBCH2
 DEFB (BCK_BUF+(0*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(7*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(7*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(0*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(1*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(2*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(3*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(4*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(5*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(6*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(7*32))/256

ALIGN $100
SBCL2
 DEFB (BCK_BUF+(0*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(0*2048)+(0*256)+(7*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(1*2048)+(0*256)+(7*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(0*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(1*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(2*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(3*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(4*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(5*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(6*32))&255
 DEFB (BCK_BUF+(2*2048)+(0*256)+(7*32))&255

ALIGN $100
SCN_BUF_TAB_H
 DEFS 4,0
 DEFS 4,1
 DEFS 4,2
 DEFS 4,3
 DEFS 4,4
 DEFS 4,5
 DEFS 4,6
 DEFS 4,7
 DEFS 4,8
 DEFS 4,9
 DEFS 4,10
 DEFS 4,11
 DEFS 4,12
 DEFS 4,13
 DEFS 4,14
 DEFS 4,15
 DEFS 4,16
 DEFS 4,17
 DEFS 4,18
 DEFS 4,19
 DEFS 4,20
 DEFS 4,21
 DEFS 4,22
 DEFS 4,23
 DEFS 4,24
 DEFS 4,25
 DEFS 4,26
 DEFS 4,27
 DEFS 4,28
 DEFS 4,29
 DEFS 4,30
 DEFS 4,31

ALIGN $100
TMP_BUF_EVEN_TAB_HI
 DEFB (TMP_BUF+(0*2048)+(0*256)+(0*32))/256,(TMP_BUF+(0*2048)+(2*256)+(0*32))/256,(TMP_BUF+(0*2048)+(4*256)+(0*32))/256,(TMP_BUF+(0*2048)+(6*256)+(0*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(1*32))/256,(TMP_BUF+(0*2048)+(2*256)+(1*32))/256,(TMP_BUF+(0*2048)+(4*256)+(1*32))/256,(TMP_BUF+(0*2048)+(6*256)+(1*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(2*32))/256,(TMP_BUF+(0*2048)+(2*256)+(2*32))/256,(TMP_BUF+(0*2048)+(4*256)+(2*32))/256,(TMP_BUF+(0*2048)+(6*256)+(2*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(3*32))/256,(TMP_BUF+(0*2048)+(2*256)+(3*32))/256,(TMP_BUF+(0*2048)+(4*256)+(3*32))/256,(TMP_BUF+(0*2048)+(6*256)+(3*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(4*32))/256,(TMP_BUF+(0*2048)+(2*256)+(4*32))/256,(TMP_BUF+(0*2048)+(4*256)+(4*32))/256,(TMP_BUF+(0*2048)+(6*256)+(4*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(5*32))/256,(TMP_BUF+(0*2048)+(2*256)+(5*32))/256,(TMP_BUF+(0*2048)+(4*256)+(5*32))/256,(TMP_BUF+(0*2048)+(6*256)+(5*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(6*32))/256,(TMP_BUF+(0*2048)+(2*256)+(6*32))/256,(TMP_BUF+(0*2048)+(4*256)+(6*32))/256,(TMP_BUF+(0*2048)+(6*256)+(6*32))/256
 DEFB (TMP_BUF+(0*2048)+(0*256)+(7*32))/256,(TMP_BUF+(0*2048)+(2*256)+(7*32))/256,(TMP_BUF+(0*2048)+(4*256)+(7*32))/256,(TMP_BUF+(0*2048)+(6*256)+(7*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(0*32))/256,(TMP_BUF+(1*2048)+(2*256)+(0*32))/256,(TMP_BUF+(1*2048)+(4*256)+(0*32))/256,(TMP_BUF+(1*2048)+(6*256)+(0*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(1*32))/256,(TMP_BUF+(1*2048)+(2*256)+(1*32))/256,(TMP_BUF+(1*2048)+(4*256)+(1*32))/256,(TMP_BUF+(1*2048)+(6*256)+(1*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(2*32))/256,(TMP_BUF+(1*2048)+(2*256)+(2*32))/256,(TMP_BUF+(1*2048)+(4*256)+(2*32))/256,(TMP_BUF+(1*2048)+(6*256)+(2*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(3*32))/256,(TMP_BUF+(1*2048)+(2*256)+(3*32))/256,(TMP_BUF+(1*2048)+(4*256)+(3*32))/256,(TMP_BUF+(1*2048)+(6*256)+(3*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(4*32))/256,(TMP_BUF+(1*2048)+(2*256)+(4*32))/256,(TMP_BUF+(1*2048)+(4*256)+(4*32))/256,(TMP_BUF+(1*2048)+(6*256)+(4*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(5*32))/256,(TMP_BUF+(1*2048)+(2*256)+(5*32))/256,(TMP_BUF+(1*2048)+(4*256)+(5*32))/256,(TMP_BUF+(1*2048)+(6*256)+(5*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(6*32))/256,(TMP_BUF+(1*2048)+(2*256)+(6*32))/256,(TMP_BUF+(1*2048)+(4*256)+(6*32))/256,(TMP_BUF+(1*2048)+(6*256)+(6*32))/256
 DEFB (TMP_BUF+(1*2048)+(0*256)+(7*32))/256,(TMP_BUF+(1*2048)+(2*256)+(7*32))/256,(TMP_BUF+(1*2048)+(4*256)+(7*32))/256,(TMP_BUF+(1*2048)+(6*256)+(7*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(0*32))/256,(TMP_BUF+(2*2048)+(2*256)+(0*32))/256,(TMP_BUF+(2*2048)+(4*256)+(0*32))/256,(TMP_BUF+(2*2048)+(6*256)+(0*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(1*32))/256,(TMP_BUF+(2*2048)+(2*256)+(1*32))/256,(TMP_BUF+(2*2048)+(4*256)+(1*32))/256,(TMP_BUF+(2*2048)+(6*256)+(1*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(2*32))/256,(TMP_BUF+(2*2048)+(2*256)+(2*32))/256,(TMP_BUF+(2*2048)+(4*256)+(2*32))/256,(TMP_BUF+(2*2048)+(6*256)+(2*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(3*32))/256,(TMP_BUF+(2*2048)+(2*256)+(3*32))/256,(TMP_BUF+(2*2048)+(4*256)+(3*32))/256,(TMP_BUF+(2*2048)+(6*256)+(3*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(4*32))/256,(TMP_BUF+(2*2048)+(2*256)+(4*32))/256,(TMP_BUF+(2*2048)+(4*256)+(4*32))/256,(TMP_BUF+(2*2048)+(6*256)+(4*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(5*32))/256,(TMP_BUF+(2*2048)+(2*256)+(5*32))/256,(TMP_BUF+(2*2048)+(4*256)+(5*32))/256,(TMP_BUF+(2*2048)+(6*256)+(5*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(6*32))/256,(TMP_BUF+(2*2048)+(2*256)+(6*32))/256,(TMP_BUF+(2*2048)+(4*256)+(6*32))/256,(TMP_BUF+(2*2048)+(6*256)+(6*32))/256
 DEFB (TMP_BUF+(2*2048)+(0*256)+(7*32))/256,(TMP_BUF+(2*2048)+(2*256)+(7*32))/256,(TMP_BUF+(2*2048)+(4*256)+(7*32))/256,(TMP_BUF+(2*2048)+(6*256)+(7*32))/256

ALIGN $100
BCK_BUF_EVEN_TAB_LO
 DEFB (TMP_BUF+(0*2048)+(0*256)+(0*32))&255,(TMP_BUF+(0*2048)+(2*256)+(0*32))&255,(TMP_BUF+(0*2048)+(4*256)+(0*32))&255,(TMP_BUF+(0*2048)+(6*256)+(0*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(1*32))&255,(TMP_BUF+(0*2048)+(2*256)+(1*32))&255,(TMP_BUF+(0*2048)+(4*256)+(1*32))&255,(TMP_BUF+(0*2048)+(6*256)+(1*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(2*32))&255,(TMP_BUF+(0*2048)+(2*256)+(2*32))&255,(TMP_BUF+(0*2048)+(4*256)+(2*32))&255,(TMP_BUF+(0*2048)+(6*256)+(2*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(3*32))&255,(TMP_BUF+(0*2048)+(2*256)+(3*32))&255,(TMP_BUF+(0*2048)+(4*256)+(3*32))&255,(TMP_BUF+(0*2048)+(6*256)+(3*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(4*32))&255,(TMP_BUF+(0*2048)+(2*256)+(4*32))&255,(TMP_BUF+(0*2048)+(4*256)+(4*32))&255,(TMP_BUF+(0*2048)+(6*256)+(4*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(5*32))&255,(TMP_BUF+(0*2048)+(2*256)+(5*32))&255,(TMP_BUF+(0*2048)+(4*256)+(5*32))&255,(TMP_BUF+(0*2048)+(6*256)+(5*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(6*32))&255,(TMP_BUF+(0*2048)+(2*256)+(6*32))&255,(TMP_BUF+(0*2048)+(4*256)+(6*32))&255,(TMP_BUF+(0*2048)+(6*256)+(6*32))&255
 DEFB (TMP_BUF+(0*2048)+(0*256)+(7*32))&255,(TMP_BUF+(0*2048)+(2*256)+(7*32))&255,(TMP_BUF+(0*2048)+(4*256)+(7*32))&255,(TMP_BUF+(0*2048)+(6*256)+(7*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(0*32))&255,(TMP_BUF+(1*2048)+(2*256)+(0*32))&255,(TMP_BUF+(1*2048)+(4*256)+(0*32))&255,(TMP_BUF+(1*2048)+(6*256)+(0*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(1*32))&255,(TMP_BUF+(1*2048)+(2*256)+(1*32))&255,(TMP_BUF+(1*2048)+(4*256)+(1*32))&255,(TMP_BUF+(1*2048)+(6*256)+(1*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(2*32))&255,(TMP_BUF+(1*2048)+(2*256)+(2*32))&255,(TMP_BUF+(1*2048)+(4*256)+(2*32))&255,(TMP_BUF+(1*2048)+(6*256)+(2*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(3*32))&255,(TMP_BUF+(1*2048)+(2*256)+(3*32))&255,(TMP_BUF+(1*2048)+(4*256)+(3*32))&255,(TMP_BUF+(1*2048)+(6*256)+(3*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(4*32))&255,(TMP_BUF+(1*2048)+(2*256)+(4*32))&255,(TMP_BUF+(1*2048)+(4*256)+(4*32))&255,(TMP_BUF+(1*2048)+(6*256)+(4*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(5*32))&255,(TMP_BUF+(1*2048)+(2*256)+(5*32))&255,(TMP_BUF+(1*2048)+(4*256)+(5*32))&255,(TMP_BUF+(1*2048)+(6*256)+(5*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(6*32))&255,(TMP_BUF+(1*2048)+(2*256)+(6*32))&255,(TMP_BUF+(1*2048)+(4*256)+(6*32))&255,(TMP_BUF+(1*2048)+(6*256)+(6*32))&255
 DEFB (TMP_BUF+(1*2048)+(0*256)+(7*32))&255,(TMP_BUF+(1*2048)+(2*256)+(7*32))&255,(TMP_BUF+(1*2048)+(4*256)+(7*32))&255,(TMP_BUF+(1*2048)+(6*256)+(7*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(0*32))&255,(TMP_BUF+(2*2048)+(2*256)+(0*32))&255,(TMP_BUF+(2*2048)+(4*256)+(0*32))&255,(TMP_BUF+(2*2048)+(6*256)+(0*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(1*32))&255,(TMP_BUF+(2*2048)+(2*256)+(1*32))&255,(TMP_BUF+(2*2048)+(4*256)+(1*32))&255,(TMP_BUF+(2*2048)+(6*256)+(1*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(2*32))&255,(TMP_BUF+(2*2048)+(2*256)+(2*32))&255,(TMP_BUF+(2*2048)+(4*256)+(2*32))&255,(TMP_BUF+(2*2048)+(6*256)+(2*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(3*32))&255,(TMP_BUF+(2*2048)+(2*256)+(3*32))&255,(TMP_BUF+(2*2048)+(4*256)+(3*32))&255,(TMP_BUF+(2*2048)+(6*256)+(3*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(4*32))&255,(TMP_BUF+(2*2048)+(2*256)+(4*32))&255,(TMP_BUF+(2*2048)+(4*256)+(4*32))&255,(TMP_BUF+(2*2048)+(6*256)+(4*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(5*32))&255,(TMP_BUF+(2*2048)+(2*256)+(5*32))&255,(TMP_BUF+(2*2048)+(4*256)+(5*32))&255,(TMP_BUF+(2*2048)+(6*256)+(5*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(6*32))&255,(TMP_BUF+(2*2048)+(2*256)+(6*32))&255,(TMP_BUF+(2*2048)+(4*256)+(6*32))&255,(TMP_BUF+(2*2048)+(6*256)+(6*32))&255
 DEFB (TMP_BUF+(2*2048)+(0*256)+(7*32))&255,(TMP_BUF+(2*2048)+(2*256)+(7*32))&255,(TMP_BUF+(2*2048)+(4*256)+(7*32))&255,(TMP_BUF+(2*2048)+(6*256)+(7*32))&255

ALIGN $100
BCK_BUF_EVEN_TAB_HI
 DEFB (BCK_BUF+(0*2048)+(0*256)+(0*32))/256,(BCK_BUF+(0*2048)+(2*256)+(0*32))/256,(BCK_BUF+(0*2048)+(4*256)+(0*32))/256,(BCK_BUF+(0*2048)+(6*256)+(0*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(1*32))/256,(BCK_BUF+(0*2048)+(2*256)+(1*32))/256,(BCK_BUF+(0*2048)+(4*256)+(1*32))/256,(BCK_BUF+(0*2048)+(6*256)+(1*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(2*32))/256,(BCK_BUF+(0*2048)+(2*256)+(2*32))/256,(BCK_BUF+(0*2048)+(4*256)+(2*32))/256,(BCK_BUF+(0*2048)+(6*256)+(2*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(3*32))/256,(BCK_BUF+(0*2048)+(2*256)+(3*32))/256,(BCK_BUF+(0*2048)+(4*256)+(3*32))/256,(BCK_BUF+(0*2048)+(6*256)+(3*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(4*32))/256,(BCK_BUF+(0*2048)+(2*256)+(4*32))/256,(BCK_BUF+(0*2048)+(4*256)+(4*32))/256,(BCK_BUF+(0*2048)+(6*256)+(4*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(5*32))/256,(BCK_BUF+(0*2048)+(2*256)+(5*32))/256,(BCK_BUF+(0*2048)+(4*256)+(5*32))/256,(BCK_BUF+(0*2048)+(6*256)+(5*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(6*32))/256,(BCK_BUF+(0*2048)+(2*256)+(6*32))/256,(BCK_BUF+(0*2048)+(4*256)+(6*32))/256,(BCK_BUF+(0*2048)+(6*256)+(6*32))/256
 DEFB (BCK_BUF+(0*2048)+(0*256)+(7*32))/256,(BCK_BUF+(0*2048)+(2*256)+(7*32))/256,(BCK_BUF+(0*2048)+(4*256)+(7*32))/256,(BCK_BUF+(0*2048)+(6*256)+(7*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(0*32))/256,(BCK_BUF+(1*2048)+(2*256)+(0*32))/256,(BCK_BUF+(1*2048)+(4*256)+(0*32))/256,(BCK_BUF+(1*2048)+(6*256)+(0*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(1*32))/256,(BCK_BUF+(1*2048)+(2*256)+(1*32))/256,(BCK_BUF+(1*2048)+(4*256)+(1*32))/256,(BCK_BUF+(1*2048)+(6*256)+(1*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(2*32))/256,(BCK_BUF+(1*2048)+(2*256)+(2*32))/256,(BCK_BUF+(1*2048)+(4*256)+(2*32))/256,(BCK_BUF+(1*2048)+(6*256)+(2*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(3*32))/256,(BCK_BUF+(1*2048)+(2*256)+(3*32))/256,(BCK_BUF+(1*2048)+(4*256)+(3*32))/256,(BCK_BUF+(1*2048)+(6*256)+(3*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(4*32))/256,(BCK_BUF+(1*2048)+(2*256)+(4*32))/256,(BCK_BUF+(1*2048)+(4*256)+(4*32))/256,(BCK_BUF+(1*2048)+(6*256)+(4*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(5*32))/256,(BCK_BUF+(1*2048)+(2*256)+(5*32))/256,(BCK_BUF+(1*2048)+(4*256)+(5*32))/256,(BCK_BUF+(1*2048)+(6*256)+(5*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(6*32))/256,(BCK_BUF+(1*2048)+(2*256)+(6*32))/256,(BCK_BUF+(1*2048)+(4*256)+(6*32))/256,(BCK_BUF+(1*2048)+(6*256)+(6*32))/256
 DEFB (BCK_BUF+(1*2048)+(0*256)+(7*32))/256,(BCK_BUF+(1*2048)+(2*256)+(7*32))/256,(BCK_BUF+(1*2048)+(4*256)+(7*32))/256,(BCK_BUF+(1*2048)+(6*256)+(7*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(0*32))/256,(BCK_BUF+(2*2048)+(2*256)+(0*32))/256,(BCK_BUF+(2*2048)+(4*256)+(0*32))/256,(BCK_BUF+(2*2048)+(6*256)+(0*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(1*32))/256,(BCK_BUF+(2*2048)+(2*256)+(1*32))/256,(BCK_BUF+(2*2048)+(4*256)+(1*32))/256,(BCK_BUF+(2*2048)+(6*256)+(1*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(2*32))/256,(BCK_BUF+(2*2048)+(2*256)+(2*32))/256,(BCK_BUF+(2*2048)+(4*256)+(2*32))/256,(BCK_BUF+(2*2048)+(6*256)+(2*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(3*32))/256,(BCK_BUF+(2*2048)+(2*256)+(3*32))/256,(BCK_BUF+(2*2048)+(4*256)+(3*32))/256,(BCK_BUF+(2*2048)+(6*256)+(3*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(4*32))/256,(BCK_BUF+(2*2048)+(2*256)+(4*32))/256,(BCK_BUF+(2*2048)+(4*256)+(4*32))/256,(BCK_BUF+(2*2048)+(6*256)+(4*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(5*32))/256,(BCK_BUF+(2*2048)+(2*256)+(5*32))/256,(BCK_BUF+(2*2048)+(4*256)+(5*32))/256,(BCK_BUF+(2*2048)+(6*256)+(5*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(6*32))/256,(BCK_BUF+(2*2048)+(2*256)+(6*32))/256,(BCK_BUF+(2*2048)+(4*256)+(6*32))/256,(BCK_BUF+(2*2048)+(6*256)+(6*32))/256
 DEFB (BCK_BUF+(2*2048)+(0*256)+(7*32))/256,(BCK_BUF+(2*2048)+(2*256)+(7*32))/256,(BCK_BUF+(2*2048)+(4*256)+(7*32))/256,(BCK_BUF+(2*2048)+(6*256)+(7*32))/256

ALIGN $100
SCN_BUF_EVEN_TAB_HI
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

; put ScrBufEvenL here and remove ScrBufEvenL + ScrBufEvenH ?

ALIGN $100
ScrBufEvenL ; remove ???? see above
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
TMP_BUF_ODD_TAB_HI
 DEFB (TMP_BUF+(0*2048)+(1*256)+(0*32))/256,(TMP_BUF+(0*2048)+(3*256)+(0*32))/256,(TMP_BUF+(0*2048)+(5*256)+(0*32))/256,(TMP_BUF+(0*2048)+(7*256)+(0*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(1*32))/256,(TMP_BUF+(0*2048)+(3*256)+(1*32))/256,(TMP_BUF+(0*2048)+(5*256)+(1*32))/256,(TMP_BUF+(0*2048)+(7*256)+(1*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(2*32))/256,(TMP_BUF+(0*2048)+(3*256)+(2*32))/256,(TMP_BUF+(0*2048)+(5*256)+(2*32))/256,(TMP_BUF+(0*2048)+(7*256)+(2*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(3*32))/256,(TMP_BUF+(0*2048)+(3*256)+(3*32))/256,(TMP_BUF+(0*2048)+(5*256)+(3*32))/256,(TMP_BUF+(0*2048)+(7*256)+(3*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(4*32))/256,(TMP_BUF+(0*2048)+(3*256)+(4*32))/256,(TMP_BUF+(0*2048)+(5*256)+(4*32))/256,(TMP_BUF+(0*2048)+(7*256)+(4*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(5*32))/256,(TMP_BUF+(0*2048)+(3*256)+(5*32))/256,(TMP_BUF+(0*2048)+(5*256)+(5*32))/256,(TMP_BUF+(0*2048)+(7*256)+(5*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(6*32))/256,(TMP_BUF+(0*2048)+(3*256)+(6*32))/256,(TMP_BUF+(0*2048)+(5*256)+(6*32))/256,(TMP_BUF+(0*2048)+(7*256)+(6*32))/256
 DEFB (TMP_BUF+(0*2048)+(1*256)+(7*32))/256,(TMP_BUF+(0*2048)+(3*256)+(7*32))/256,(TMP_BUF+(0*2048)+(5*256)+(7*32))/256,(TMP_BUF+(0*2048)+(7*256)+(7*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(0*32))/256,(TMP_BUF+(1*2048)+(3*256)+(0*32))/256,(TMP_BUF+(1*2048)+(5*256)+(0*32))/256,(TMP_BUF+(1*2048)+(7*256)+(0*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(1*32))/256,(TMP_BUF+(1*2048)+(3*256)+(1*32))/256,(TMP_BUF+(1*2048)+(5*256)+(1*32))/256,(TMP_BUF+(1*2048)+(7*256)+(1*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(2*32))/256,(TMP_BUF+(1*2048)+(3*256)+(2*32))/256,(TMP_BUF+(1*2048)+(5*256)+(2*32))/256,(TMP_BUF+(1*2048)+(7*256)+(2*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(3*32))/256,(TMP_BUF+(1*2048)+(3*256)+(3*32))/256,(TMP_BUF+(1*2048)+(5*256)+(3*32))/256,(TMP_BUF+(1*2048)+(7*256)+(3*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(4*32))/256,(TMP_BUF+(1*2048)+(3*256)+(4*32))/256,(TMP_BUF+(1*2048)+(5*256)+(4*32))/256,(TMP_BUF+(1*2048)+(7*256)+(4*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(5*32))/256,(TMP_BUF+(1*2048)+(3*256)+(5*32))/256,(TMP_BUF+(1*2048)+(5*256)+(5*32))/256,(TMP_BUF+(1*2048)+(7*256)+(5*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(6*32))/256,(TMP_BUF+(1*2048)+(3*256)+(6*32))/256,(TMP_BUF+(1*2048)+(5*256)+(6*32))/256,(TMP_BUF+(1*2048)+(7*256)+(6*32))/256
 DEFB (TMP_BUF+(1*2048)+(1*256)+(7*32))/256,(TMP_BUF+(1*2048)+(3*256)+(7*32))/256,(TMP_BUF+(1*2048)+(5*256)+(7*32))/256,(TMP_BUF+(1*2048)+(7*256)+(7*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(0*32))/256,(TMP_BUF+(2*2048)+(3*256)+(0*32))/256,(TMP_BUF+(2*2048)+(5*256)+(0*32))/256,(TMP_BUF+(2*2048)+(7*256)+(0*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(1*32))/256,(TMP_BUF+(2*2048)+(3*256)+(1*32))/256,(TMP_BUF+(2*2048)+(5*256)+(1*32))/256,(TMP_BUF+(2*2048)+(7*256)+(1*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(2*32))/256,(TMP_BUF+(2*2048)+(3*256)+(2*32))/256,(TMP_BUF+(2*2048)+(5*256)+(2*32))/256,(TMP_BUF+(2*2048)+(7*256)+(2*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(3*32))/256,(TMP_BUF+(2*2048)+(3*256)+(3*32))/256,(TMP_BUF+(2*2048)+(5*256)+(3*32))/256,(TMP_BUF+(2*2048)+(7*256)+(3*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(4*32))/256,(TMP_BUF+(2*2048)+(3*256)+(4*32))/256,(TMP_BUF+(2*2048)+(5*256)+(4*32))/256,(TMP_BUF+(2*2048)+(7*256)+(4*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(5*32))/256,(TMP_BUF+(2*2048)+(3*256)+(5*32))/256,(TMP_BUF+(2*2048)+(5*256)+(5*32))/256,(TMP_BUF+(2*2048)+(7*256)+(5*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(6*32))/256,(TMP_BUF+(2*2048)+(3*256)+(6*32))/256,(TMP_BUF+(2*2048)+(5*256)+(6*32))/256,(TMP_BUF+(2*2048)+(7*256)+(6*32))/256
 DEFB (TMP_BUF+(2*2048)+(1*256)+(7*32))/256,(TMP_BUF+(2*2048)+(3*256)+(7*32))/256,(TMP_BUF+(2*2048)+(5*256)+(7*32))/256,(TMP_BUF+(2*2048)+(7*256)+(7*32))/256

ALIGN $100
BCK_BUF_ODD_TAB_LO
 DEFB (TMP_BUF+(0*2048)+(1*256)+(0*32))&255,(TMP_BUF+(0*2048)+(3*256)+(0*32))&255,(TMP_BUF+(0*2048)+(5*256)+(0*32))&255,(TMP_BUF+(0*2048)+(7*256)+(0*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(1*32))&255,(TMP_BUF+(0*2048)+(3*256)+(1*32))&255,(TMP_BUF+(0*2048)+(5*256)+(1*32))&255,(TMP_BUF+(0*2048)+(7*256)+(1*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(2*32))&255,(TMP_BUF+(0*2048)+(3*256)+(2*32))&255,(TMP_BUF+(0*2048)+(5*256)+(2*32))&255,(TMP_BUF+(0*2048)+(7*256)+(2*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(3*32))&255,(TMP_BUF+(0*2048)+(3*256)+(3*32))&255,(TMP_BUF+(0*2048)+(5*256)+(3*32))&255,(TMP_BUF+(0*2048)+(7*256)+(3*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(4*32))&255,(TMP_BUF+(0*2048)+(3*256)+(4*32))&255,(TMP_BUF+(0*2048)+(5*256)+(4*32))&255,(TMP_BUF+(0*2048)+(7*256)+(4*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(5*32))&255,(TMP_BUF+(0*2048)+(3*256)+(5*32))&255,(TMP_BUF+(0*2048)+(5*256)+(5*32))&255,(TMP_BUF+(0*2048)+(7*256)+(5*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(6*32))&255,(TMP_BUF+(0*2048)+(3*256)+(6*32))&255,(TMP_BUF+(0*2048)+(5*256)+(6*32))&255,(TMP_BUF+(0*2048)+(7*256)+(6*32))&255
 DEFB (TMP_BUF+(0*2048)+(1*256)+(7*32))&255,(TMP_BUF+(0*2048)+(3*256)+(7*32))&255,(TMP_BUF+(0*2048)+(5*256)+(7*32))&255,(TMP_BUF+(0*2048)+(7*256)+(7*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(0*32))&255,(TMP_BUF+(1*2048)+(3*256)+(0*32))&255,(TMP_BUF+(1*2048)+(5*256)+(0*32))&255,(TMP_BUF+(1*2048)+(7*256)+(0*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(1*32))&255,(TMP_BUF+(1*2048)+(3*256)+(1*32))&255,(TMP_BUF+(1*2048)+(5*256)+(1*32))&255,(TMP_BUF+(1*2048)+(7*256)+(1*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(2*32))&255,(TMP_BUF+(1*2048)+(3*256)+(2*32))&255,(TMP_BUF+(1*2048)+(5*256)+(2*32))&255,(TMP_BUF+(1*2048)+(7*256)+(2*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(3*32))&255,(TMP_BUF+(1*2048)+(3*256)+(3*32))&255,(TMP_BUF+(1*2048)+(5*256)+(3*32))&255,(TMP_BUF+(1*2048)+(7*256)+(3*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(4*32))&255,(TMP_BUF+(1*2048)+(3*256)+(4*32))&255,(TMP_BUF+(1*2048)+(5*256)+(4*32))&255,(TMP_BUF+(1*2048)+(7*256)+(4*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(5*32))&255,(TMP_BUF+(1*2048)+(3*256)+(5*32))&255,(TMP_BUF+(1*2048)+(5*256)+(5*32))&255,(TMP_BUF+(1*2048)+(7*256)+(5*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(6*32))&255,(TMP_BUF+(1*2048)+(3*256)+(6*32))&255,(TMP_BUF+(1*2048)+(5*256)+(6*32))&255,(TMP_BUF+(1*2048)+(7*256)+(6*32))&255
 DEFB (TMP_BUF+(1*2048)+(1*256)+(7*32))&255,(TMP_BUF+(1*2048)+(3*256)+(7*32))&255,(TMP_BUF+(1*2048)+(5*256)+(7*32))&255,(TMP_BUF+(1*2048)+(7*256)+(7*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(0*32))&255,(TMP_BUF+(2*2048)+(3*256)+(0*32))&255,(TMP_BUF+(2*2048)+(5*256)+(0*32))&255,(TMP_BUF+(2*2048)+(7*256)+(0*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(1*32))&255,(TMP_BUF+(2*2048)+(3*256)+(1*32))&255,(TMP_BUF+(2*2048)+(5*256)+(1*32))&255,(TMP_BUF+(2*2048)+(7*256)+(1*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(2*32))&255,(TMP_BUF+(2*2048)+(3*256)+(2*32))&255,(TMP_BUF+(2*2048)+(5*256)+(2*32))&255,(TMP_BUF+(2*2048)+(7*256)+(2*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(3*32))&255,(TMP_BUF+(2*2048)+(3*256)+(3*32))&255,(TMP_BUF+(2*2048)+(5*256)+(3*32))&255,(TMP_BUF+(2*2048)+(7*256)+(3*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(4*32))&255,(TMP_BUF+(2*2048)+(3*256)+(4*32))&255,(TMP_BUF+(2*2048)+(5*256)+(4*32))&255,(TMP_BUF+(2*2048)+(7*256)+(4*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(5*32))&255,(TMP_BUF+(2*2048)+(3*256)+(5*32))&255,(TMP_BUF+(2*2048)+(5*256)+(5*32))&255,(TMP_BUF+(2*2048)+(7*256)+(5*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(6*32))&255,(TMP_BUF+(2*2048)+(3*256)+(6*32))&255,(TMP_BUF+(2*2048)+(5*256)+(6*32))&255,(TMP_BUF+(2*2048)+(7*256)+(6*32))&255
 DEFB (TMP_BUF+(2*2048)+(1*256)+(7*32))&255,(TMP_BUF+(2*2048)+(3*256)+(7*32))&255,(TMP_BUF+(2*2048)+(5*256)+(7*32))&255,(TMP_BUF+(2*2048)+(7*256)+(7*32))&255

ALIGN $100
BCK_BUF_ODD_TAB_HI
 DEFB (BCK_BUF+(0*2048)+(1*256)+(0*32))/256,(BCK_BUF+(0*2048)+(3*256)+(0*32))/256,(BCK_BUF+(0*2048)+(5*256)+(0*32))/256,(BCK_BUF+(0*2048)+(7*256)+(0*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(1*32))/256,(BCK_BUF+(0*2048)+(3*256)+(1*32))/256,(BCK_BUF+(0*2048)+(5*256)+(1*32))/256,(BCK_BUF+(0*2048)+(7*256)+(1*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(2*32))/256,(BCK_BUF+(0*2048)+(3*256)+(2*32))/256,(BCK_BUF+(0*2048)+(5*256)+(2*32))/256,(BCK_BUF+(0*2048)+(7*256)+(2*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(3*32))/256,(BCK_BUF+(0*2048)+(3*256)+(3*32))/256,(BCK_BUF+(0*2048)+(5*256)+(3*32))/256,(BCK_BUF+(0*2048)+(7*256)+(3*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(4*32))/256,(BCK_BUF+(0*2048)+(3*256)+(4*32))/256,(BCK_BUF+(0*2048)+(5*256)+(4*32))/256,(BCK_BUF+(0*2048)+(7*256)+(4*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(5*32))/256,(BCK_BUF+(0*2048)+(3*256)+(5*32))/256,(BCK_BUF+(0*2048)+(5*256)+(5*32))/256,(BCK_BUF+(0*2048)+(7*256)+(5*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(6*32))/256,(BCK_BUF+(0*2048)+(3*256)+(6*32))/256,(BCK_BUF+(0*2048)+(5*256)+(6*32))/256,(BCK_BUF+(0*2048)+(7*256)+(6*32))/256
 DEFB (BCK_BUF+(0*2048)+(1*256)+(7*32))/256,(BCK_BUF+(0*2048)+(3*256)+(7*32))/256,(BCK_BUF+(0*2048)+(5*256)+(7*32))/256,(BCK_BUF+(0*2048)+(7*256)+(7*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(0*32))/256,(BCK_BUF+(1*2048)+(3*256)+(0*32))/256,(BCK_BUF+(1*2048)+(5*256)+(0*32))/256,(BCK_BUF+(1*2048)+(7*256)+(0*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(1*32))/256,(BCK_BUF+(1*2048)+(3*256)+(1*32))/256,(BCK_BUF+(1*2048)+(5*256)+(1*32))/256,(BCK_BUF+(1*2048)+(7*256)+(1*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(2*32))/256,(BCK_BUF+(1*2048)+(3*256)+(2*32))/256,(BCK_BUF+(1*2048)+(5*256)+(2*32))/256,(BCK_BUF+(1*2048)+(7*256)+(2*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(3*32))/256,(BCK_BUF+(1*2048)+(3*256)+(3*32))/256,(BCK_BUF+(1*2048)+(5*256)+(3*32))/256,(BCK_BUF+(1*2048)+(7*256)+(3*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(4*32))/256,(BCK_BUF+(1*2048)+(3*256)+(4*32))/256,(BCK_BUF+(1*2048)+(5*256)+(4*32))/256,(BCK_BUF+(1*2048)+(7*256)+(4*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(5*32))/256,(BCK_BUF+(1*2048)+(3*256)+(5*32))/256,(BCK_BUF+(1*2048)+(5*256)+(5*32))/256,(BCK_BUF+(1*2048)+(7*256)+(5*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(6*32))/256,(BCK_BUF+(1*2048)+(3*256)+(6*32))/256,(BCK_BUF+(1*2048)+(5*256)+(6*32))/256,(BCK_BUF+(1*2048)+(7*256)+(6*32))/256
 DEFB (BCK_BUF+(1*2048)+(1*256)+(7*32))/256,(BCK_BUF+(1*2048)+(3*256)+(7*32))/256,(BCK_BUF+(1*2048)+(5*256)+(7*32))/256,(BCK_BUF+(1*2048)+(7*256)+(7*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(0*32))/256,(BCK_BUF+(2*2048)+(3*256)+(0*32))/256,(BCK_BUF+(2*2048)+(5*256)+(0*32))/256,(BCK_BUF+(2*2048)+(7*256)+(0*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(1*32))/256,(BCK_BUF+(2*2048)+(3*256)+(1*32))/256,(BCK_BUF+(2*2048)+(5*256)+(1*32))/256,(BCK_BUF+(2*2048)+(7*256)+(1*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(2*32))/256,(BCK_BUF+(2*2048)+(3*256)+(2*32))/256,(BCK_BUF+(2*2048)+(5*256)+(2*32))/256,(BCK_BUF+(2*2048)+(7*256)+(2*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(3*32))/256,(BCK_BUF+(2*2048)+(3*256)+(3*32))/256,(BCK_BUF+(2*2048)+(5*256)+(3*32))/256,(BCK_BUF+(2*2048)+(7*256)+(3*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(4*32))/256,(BCK_BUF+(2*2048)+(3*256)+(4*32))/256,(BCK_BUF+(2*2048)+(5*256)+(4*32))/256,(BCK_BUF+(2*2048)+(7*256)+(4*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(5*32))/256,(BCK_BUF+(2*2048)+(3*256)+(5*32))/256,(BCK_BUF+(2*2048)+(5*256)+(5*32))/256,(BCK_BUF+(2*2048)+(7*256)+(5*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(6*32))/256,(BCK_BUF+(2*2048)+(3*256)+(6*32))/256,(BCK_BUF+(2*2048)+(5*256)+(6*32))/256,(BCK_BUF+(2*2048)+(7*256)+(6*32))/256
 DEFB (BCK_BUF+(2*2048)+(1*256)+(7*32))/256,(BCK_BUF+(2*2048)+(3*256)+(7*32))/256,(BCK_BUF+(2*2048)+(5*256)+(7*32))/256,(BCK_BUF+(2*2048)+(7*256)+(7*32))/256

ALIGN $100
SCN_BUF_ODD_TAB_HI
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
ScrBufOddL ; remove ???? see above
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

;ALIGN $100
;ScrBufEvenH ; remove ???? see above
; DEFB (SCREEN+(0*2048)+(0*256)+(0*32))/256,(SCREEN+(0*2048)+(2*256)+(0*32))/256,(SCREEN+(0*2048)+(4*256)+(0*32))/256,(SCREEN+(0*2048)+(6*256)+(0*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(1*32))/256,(SCREEN+(0*2048)+(2*256)+(1*32))/256,(SCREEN+(0*2048)+(4*256)+(1*32))/256,(SCREEN+(0*2048)+(6*256)+(1*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(2*32))/256,(SCREEN+(0*2048)+(2*256)+(2*32))/256,(SCREEN+(0*2048)+(4*256)+(2*32))/256,(SCREEN+(0*2048)+(6*256)+(2*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(3*32))/256,(SCREEN+(0*2048)+(2*256)+(3*32))/256,(SCREEN+(0*2048)+(4*256)+(3*32))/256,(SCREEN+(0*2048)+(6*256)+(3*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(4*32))/256,(SCREEN+(0*2048)+(2*256)+(4*32))/256,(SCREEN+(0*2048)+(4*256)+(4*32))/256,(SCREEN+(0*2048)+(6*256)+(4*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(5*32))/256,(SCREEN+(0*2048)+(2*256)+(5*32))/256,(SCREEN+(0*2048)+(4*256)+(5*32))/256,(SCREEN+(0*2048)+(6*256)+(5*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(6*32))/256,(SCREEN+(0*2048)+(2*256)+(6*32))/256,(SCREEN+(0*2048)+(4*256)+(6*32))/256,(SCREEN+(0*2048)+(6*256)+(6*32))/256
; DEFB (SCREEN+(0*2048)+(0*256)+(7*32))/256,(SCREEN+(0*2048)+(2*256)+(7*32))/256,(SCREEN+(0*2048)+(4*256)+(7*32))/256,(SCREEN+(0*2048)+(6*256)+(7*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(0*32))/256,(SCREEN+(1*2048)+(2*256)+(0*32))/256,(SCREEN+(1*2048)+(4*256)+(0*32))/256,(SCREEN+(1*2048)+(6*256)+(0*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(1*32))/256,(SCREEN+(1*2048)+(2*256)+(1*32))/256,(SCREEN+(1*2048)+(4*256)+(1*32))/256,(SCREEN+(1*2048)+(6*256)+(1*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(2*32))/256,(SCREEN+(1*2048)+(2*256)+(2*32))/256,(SCREEN+(1*2048)+(4*256)+(2*32))/256,(SCREEN+(1*2048)+(6*256)+(2*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(3*32))/256,(SCREEN+(1*2048)+(2*256)+(3*32))/256,(SCREEN+(1*2048)+(4*256)+(3*32))/256,(SCREEN+(1*2048)+(6*256)+(3*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(4*32))/256,(SCREEN+(1*2048)+(2*256)+(4*32))/256,(SCREEN+(1*2048)+(4*256)+(4*32))/256,(SCREEN+(1*2048)+(6*256)+(4*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(5*32))/256,(SCREEN+(1*2048)+(2*256)+(5*32))/256,(SCREEN+(1*2048)+(4*256)+(5*32))/256,(SCREEN+(1*2048)+(6*256)+(5*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(6*32))/256,(SCREEN+(1*2048)+(2*256)+(6*32))/256,(SCREEN+(1*2048)+(4*256)+(6*32))/256,(SCREEN+(1*2048)+(6*256)+(6*32))/256
; DEFB (SCREEN+(1*2048)+(0*256)+(7*32))/256,(SCREEN+(1*2048)+(2*256)+(7*32))/256,(SCREEN+(1*2048)+(4*256)+(7*32))/256,(SCREEN+(1*2048)+(6*256)+(7*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(0*32))/256,(SCREEN+(2*2048)+(2*256)+(0*32))/256,(SCREEN+(2*2048)+(4*256)+(0*32))/256,(SCREEN+(2*2048)+(6*256)+(0*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(1*32))/256,(SCREEN+(2*2048)+(2*256)+(1*32))/256,(SCREEN+(2*2048)+(4*256)+(1*32))/256,(SCREEN+(2*2048)+(6*256)+(1*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(2*32))/256,(SCREEN+(2*2048)+(2*256)+(2*32))/256,(SCREEN+(2*2048)+(4*256)+(2*32))/256,(SCREEN+(2*2048)+(6*256)+(2*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(3*32))/256,(SCREEN+(2*2048)+(2*256)+(3*32))/256,(SCREEN+(2*2048)+(4*256)+(3*32))/256,(SCREEN+(2*2048)+(6*256)+(3*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(4*32))/256,(SCREEN+(2*2048)+(2*256)+(4*32))/256,(SCREEN+(2*2048)+(4*256)+(4*32))/256,(SCREEN+(2*2048)+(6*256)+(4*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(5*32))/256,(SCREEN+(2*2048)+(2*256)+(5*32))/256,(SCREEN+(2*2048)+(4*256)+(5*32))/256,(SCREEN+(2*2048)+(6*256)+(5*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(6*32))/256,(SCREEN+(2*2048)+(2*256)+(6*32))/256,(SCREEN+(2*2048)+(4*256)+(6*32))/256,(SCREEN+(2*2048)+(6*256)+(6*32))/256
; DEFB (SCREEN+(2*2048)+(0*256)+(7*32))/256,(SCREEN+(2*2048)+(2*256)+(7*32))/256,(SCREEN+(2*2048)+(4*256)+(7*32))/256,(SCREEN+(2*2048)+(6*256)+(7*32))/256

;ALIGN $100
;ScrBufEvenL ; remove ???? see above
; DEFB (SCREEN+(0*2048)+(0*256)+(0*32))&255,(SCREEN+(0*2048)+(2*256)+(0*32))&255,(SCREEN+(0*2048)+(4*256)+(0*32))&255,(SCREEN+(0*2048)+(6*256)+(0*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(1*32))&255,(SCREEN+(0*2048)+(2*256)+(1*32))&255,(SCREEN+(0*2048)+(4*256)+(1*32))&255,(SCREEN+(0*2048)+(6*256)+(1*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(2*32))&255,(SCREEN+(0*2048)+(2*256)+(2*32))&255,(SCREEN+(0*2048)+(4*256)+(2*32))&255,(SCREEN+(0*2048)+(6*256)+(2*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(3*32))&255,(SCREEN+(0*2048)+(2*256)+(3*32))&255,(SCREEN+(0*2048)+(4*256)+(3*32))&255,(SCREEN+(0*2048)+(6*256)+(3*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(4*32))&255,(SCREEN+(0*2048)+(2*256)+(4*32))&255,(SCREEN+(0*2048)+(4*256)+(4*32))&255,(SCREEN+(0*2048)+(6*256)+(4*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(5*32))&255,(SCREEN+(0*2048)+(2*256)+(5*32))&255,(SCREEN+(0*2048)+(4*256)+(5*32))&255,(SCREEN+(0*2048)+(6*256)+(5*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(6*32))&255,(SCREEN+(0*2048)+(2*256)+(6*32))&255,(SCREEN+(0*2048)+(4*256)+(6*32))&255,(SCREEN+(0*2048)+(6*256)+(6*32))&255
; DEFB (SCREEN+(0*2048)+(0*256)+(7*32))&255,(SCREEN+(0*2048)+(2*256)+(7*32))&255,(SCREEN+(0*2048)+(4*256)+(7*32))&255,(SCREEN+(0*2048)+(6*256)+(7*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(0*32))&255,(SCREEN+(1*2048)+(2*256)+(0*32))&255,(SCREEN+(1*2048)+(4*256)+(0*32))&255,(SCREEN+(1*2048)+(6*256)+(0*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(1*32))&255,(SCREEN+(1*2048)+(2*256)+(1*32))&255,(SCREEN+(1*2048)+(4*256)+(1*32))&255,(SCREEN+(1*2048)+(6*256)+(1*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(2*32))&255,(SCREEN+(1*2048)+(2*256)+(2*32))&255,(SCREEN+(1*2048)+(4*256)+(2*32))&255,(SCREEN+(1*2048)+(6*256)+(2*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(3*32))&255,(SCREEN+(1*2048)+(2*256)+(3*32))&255,(SCREEN+(1*2048)+(4*256)+(3*32))&255,(SCREEN+(1*2048)+(6*256)+(3*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(4*32))&255,(SCREEN+(1*2048)+(2*256)+(4*32))&255,(SCREEN+(1*2048)+(4*256)+(4*32))&255,(SCREEN+(1*2048)+(6*256)+(4*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(5*32))&255,(SCREEN+(1*2048)+(2*256)+(5*32))&255,(SCREEN+(1*2048)+(4*256)+(5*32))&255,(SCREEN+(1*2048)+(6*256)+(5*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(6*32))&255,(SCREEN+(1*2048)+(2*256)+(6*32))&255,(SCREEN+(1*2048)+(4*256)+(6*32))&255,(SCREEN+(1*2048)+(6*256)+(6*32))&255
; DEFB (SCREEN+(1*2048)+(0*256)+(7*32))&255,(SCREEN+(1*2048)+(2*256)+(7*32))&255,(SCREEN+(1*2048)+(4*256)+(7*32))&255,(SCREEN+(1*2048)+(6*256)+(7*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(0*32))&255,(SCREEN+(2*2048)+(2*256)+(0*32))&255,(SCREEN+(2*2048)+(4*256)+(0*32))&255,(SCREEN+(2*2048)+(6*256)+(0*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(1*32))&255,(SCREEN+(2*2048)+(2*256)+(1*32))&255,(SCREEN+(2*2048)+(4*256)+(1*32))&255,(SCREEN+(2*2048)+(6*256)+(1*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(2*32))&255,(SCREEN+(2*2048)+(2*256)+(2*32))&255,(SCREEN+(2*2048)+(4*256)+(2*32))&255,(SCREEN+(2*2048)+(6*256)+(2*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(3*32))&255,(SCREEN+(2*2048)+(2*256)+(3*32))&255,(SCREEN+(2*2048)+(4*256)+(3*32))&255,(SCREEN+(2*2048)+(6*256)+(3*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(4*32))&255,(SCREEN+(2*2048)+(2*256)+(4*32))&255,(SCREEN+(2*2048)+(4*256)+(4*32))&255,(SCREEN+(2*2048)+(6*256)+(4*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(5*32))&255,(SCREEN+(2*2048)+(2*256)+(5*32))&255,(SCREEN+(2*2048)+(4*256)+(5*32))&255,(SCREEN+(2*2048)+(6*256)+(5*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(6*32))&255,(SCREEN+(2*2048)+(2*256)+(6*32))&255,(SCREEN+(2*2048)+(4*256)+(6*32))&255,(SCREEN+(2*2048)+(6*256)+(6*32))&255
; DEFB (SCREEN+(2*2048)+(0*256)+(7*32))&255,(SCREEN+(2*2048)+(2*256)+(7*32))&255,(SCREEN+(2*2048)+(4*256)+(7*32))&255,(SCREEN+(2*2048)+(6*256)+(7*32))&255

ALIGN $100
ScrBufY
 DEFS 4,0
 DEFS 4,1
 DEFS 4,2
 DEFS 4,3
 DEFS 4,4
 DEFS 4,5
 DEFS 4,6
 DEFS 4,7
 DEFS 4,8
 DEFS 4,9
 DEFS 4,10
 DEFS 4,11
 DEFS 4,12
 DEFS 4,13
 DEFS 4,14
 DEFS 4,15
 DEFS 4,16
 DEFS 4,17
 DEFS 4,18
 DEFS 4,19
 DEFS 4,20
 DEFS 4,21
 DEFS 4,22
 DEFS 4,23
 DEFS 4,24
 DEFS 4,25
 DEFS 4,26
 DEFS 4,27
 DEFS 4,28
 DEFS 4,29
 DEFS 4,30
 DEFS 4,31

ALIGN $100
ScrBufSprISOY
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6
 DEFB 0,2,4,6

; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6
; DEFB 0,2,4,6

TEST_AND_OR_SPR01
 defb 255,0,255,0,255,0,255,0,255,0
 defb 255,0,0,255,0,255,0,255,192,63
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 223,32,255,0,255,0,254,1,255,0
 defb 255,0,254,1,255,0,255,0,223,32
 defb 192,63,0,255,0,255,0,255,255,0
 defb 255,0,255,0,255,0,255,0,255,0

TEST_AND_OR_SPR02
 defb 255,0,255,0,255,0,255,0,255,0
 defb 63,192,0,255,0,255,0,255,240,15
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 247,8,255,0,255,0,255,0,191,64
 defb 191,64,255,0,255,0,255,0,247,8
 defb 240,15,0,255,0,255,0,255,63,192
 defb 255,0,255,0,255,0,255,0,255,0

TEST_AND_OR_SPR03
 defb 255,0,255,0,255,0,255,0,255,0
 defb 15,240,0,255,0,255,0,255,252,3
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 253,2,255,0,255,0,255,0,239,16
 defb 239,16,255,0,255,0,255,0,253,2
 defb 252,3,0,255,0,255,0,255,15,240
 defb 255,0,255,0,255,0,255,0,255,0

TEST_AND_OR_SPR04
 defb 255,0,255,0,255,0,255,0,255,0
 defb 3,252,0,255,0,255,0,255,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,127,128,255,0,255,0,251,4
 defb 251,4,255,0,255,0,127,128,255,0
 defb 255,0,0,255,0,255,0,255,3,252
 defb 255,0,255,0,255,0,255,0,255,0

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

iso ; a = pos.z b = pos.x c = pos.y ; return VH(pos.x + pos.y + pos.z, ISOLEFT + ( (pos.x - pos.y) <<1 )); in a and d
; add a, b               ; pos.z + pos.x                         ; #4
; add a, c               ; pos.z + pos.x + pos.y         ; #4
; ld d, a                ; rc                                            ; #4
; ld a, isoleft  ; isoleft                                       ; #7
; add a, b               ; isoleft + pos.x                       ; #4
; sub a, c               ; isoleft + pos.x - pos.y       ; #4
; ret
