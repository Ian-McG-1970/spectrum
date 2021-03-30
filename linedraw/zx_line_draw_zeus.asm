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

 BORDER (6)

 LD DE,$1140       ;  vblank setup - attr into D, MSB of port addr into E
 LD A,D
 LD ($5ae0), A
 LD ($5ae1), A

MAIN_LOOP
 CALL V_BLANK
 CALL COPY_SCREEN
 CALL CLEAR_SCREEN

;  LD A, (px)
;  LD L,A
;  LD A, (py)
;  LD C, A
;  CALL  PLOT

;  LD L, 10
;  LD C, 10
;  CALL  PLOT

;  LD L, 100
;  LD C, 50
;  CALL  PLOT

  CALL MOVE_POINTS

; BORDER (1)

 ld C,0
 ld e,0
 LD A, (sttx)
 ld b,a
 LD A, (stty)
 ld h,a
 call DRAW_LINE

 ld C,191
 ld e,0
 LD A, (sttx)
 ld b,a
 LD A, (stty)
 ld h,a
 call DRAW_LINE

 ld C,0
 ld e,255
 LD A, (sttx)
 ld b,a
 LD A, (stty)
 ld h,a
 call DRAW_LINE

 ld C,191
 ld e,255
 LD A, (sttx)
 ld b,a
 LD A, (stty)
 ld h,a
 call DRAW_LINE

; BORDER (2)

  CALL KEYBOARD
 JP MAIN_LOOP

sttx defb 172
stty defb 12
endx defb 95
endy defb 147
sdx defb 1
sdy defb 1
edx defb 1
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

KEYBOARD LD BC,$FBFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; q
         JP NZ,Q_KEY_N   ; not pressed
                LD      A, (px)
                DEC     A
                LD      (px), A
Q_KEY_N  LD BC,$DFFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; p
         JP NZ,P_KEY_N   ; not pressed
                LD      A, (px)
                INC     A
                LD      (px), A
P_KEY_N  LD BC,$FEFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; q
         JP NZ,Z_KEY_N   ; not pressed
                LD      A, (py)
                DEC     A
                LD      (py), A
Z_KEY_N  LD BC,$7FFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; p
         JP NZ,M_KEY_N   ; not pressed
                LD      A, (py)
                INC     A
                LD      (py), A
M_KEY_N RET

MOVE_POINTS LD a, (sttx)
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
EDGE LD A,   b
     ADD A,    c
     cp      h
     jp z, REVERSE
     cp l
     jp z, REVERSE
     LD b, a
RET

REVERSE LD b, a
        LD a, c
        NEG
        LD c, a
RET

V_BLANK
 LD DE,$1140       ; attr into D, MSB of port addr into E
FB_LP
  INC HL          ; padding instruction
  LD A,E          ; MSB of port addr into A
  IN A,($ff)      ; read port 0x40FF into A
  CP D            ; is it D (i.e. INK 1, PAPER 1, BRIGHT 0; FLASH 0)?
  JP NZ,FB_LP     ; no? keep trying
RET

PLOT
 LD H, HIGH ScrBufH     ; SCREEN V TABLE
 LD D,(HL)              ; L=VPOS
 INC H    ; ScrBufL
 LD E,(HL)              ; DE = SCREEN POS

 LD L,C                 ; C=HPOS
 INC H    ; ScrBufY

 LD L, (HL)             ; HOR BYTE POS
 LD H,0                 ; CLEAR
 ADD HL,DE              ; SCREEN POS + HOR BYTE POS

 LD B, HIGH ScrBufOR    ; SCREEN OR TABLE
 LD A, (BC)
 OR (HL)
 LD (HL), A
RET

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

DRAW_LINE
 LD A, H ; ey
 CP E    ; lt sy
 JP NC, DRAW_LINE_NOSWAP
  LD A, C ; swap x (C/B)
  LD C, B
  LD B, A
  LD A, E ; swap y (E/H)
  LD E, H
  LD H, A

DRAW_LINE_NOSWAP
 LD A, B ; ex
 CP C    ; lt sx
 JP C, LINE_V4_V2
;;                jp      DRAW_LINE_TLBR

; C=SX
; E=SY
; B=EX/DX
; H=EY/DY

LINE_V4
 LD A, B ; ex
 SUB C   ; sx
 LD B, A ; b=dx=ex-sx

 LD A, H ; ey
 SUB E   ; sy
 LD H, A ; h=dy=ey-sy

 CP B    ; dy GT dx
 JR NC, LINE_DXDY_TLBR

LINE_DYDX_TLBR

 CALL SLOPE
 LD (LINE_DYDX_TLBR_SLOPE+1), A

LINE_DYDX_TLBR_LOOP       ; for count eq 0 to dy
   INC C                  ; inc VPOS
LINE_DYDX_TLBR_SLOPE
   ADD A, 0               ; A=FRAC
   JP NC, LINE_DYDX_TLBR_CNT
    INC E                 ; inc HPOS

LINE_DYDX_TLBR_CNT
   EX AF,AF'              ; 'A=FRAC

   LD D, HIGH ScrBufY     ; SCREEN H TABLE
   LD A,(DE)              ; E=HPOS

   LD H, HIGH ScrBufL     ; SCREEN V TABLE
   LD L,C                 ; retore l
   ADD A,(HL)             ; L=VPOS
   DEC H                  ; ScrBufH
   LD H,(HL)              ; HL = SCREEN POS START LINE
   LD L,A

   INC D                  ; ScrBufOR
   LD A, (DE)
   OR (HL)
   LD (HL), A

   EX AF,AF'              ; restore accum
  DJNZ LINE_DYDX_TLBR_LOOP

RET

LINE_DXDY_TLBR

 LD A, B
 LD B, H
 LD H, A
 CALL SLOPE
 LD (LINE_DXDY_TLBR_SLOPE+1), A

LINE_DXDY_TLBR_LOOP       ; for count eq 0 to dy
   INC E                  ; #4 - inc hpos
LINE_DXDY_TLBR_SLOPE
   ADD A, 127             ; #7 - add slope to accum
   JP NC, LINE_DXDY_TLBR_CNT
    INC C                 ; #4 - inc vpos

LINE_DXDY_TLBR_CNT
   EX AF,AF'              ; 'A=FRAC

   LD D, HIGH ScrBufY     ; SCREEN H TABLE
   LD A,(DE)              ; E=HPOS

   LD H, HIGH ScrBufL     ; SCREEN V TABLE
   LD L,C                 ; retore l
   ADD A,(HL)             ; L=VPOS
   DEC H                  ; ScrBufH
   LD H,(HL)              ; HL = SCREEN POS START LINE
   LD L,A

   INC D                  ; ScrBufOR
   LD A, (DE)
   OR (HL)
   LD (HL), A

   EX AF,AF'              ; restore accum
  DJNZ LINE_DXDY_TLBR_LOOP

RET









; C=SX
; E=SY
; B=EX/DX
; H=EY/DY

LINE_V4_V2

; LD A, B ; ex
; SUB C   ; sx
; LD B, A ; b=dx=ex-sx

 LD A, C ; ex
 SUB B   ; sx
 LD B, A ; b=dx=ex-sx

 LD A, H ; ey
 SUB E   ; sy
 LD H, A ; h=dy=ey-sy

 CP B    ; dy GT dx
 JR NC, LINE_DXDY_TRBL

LINE_DYDX_TRBL

 CALL SLOPE ; ; Integer divide HL by B result A remainder H
 LD (LINE_DYDX_TRBL_SLOPE+1), A

LINE_DYDX_TRBL_LOOP       ; for count eq 0 to dy
   DEC C                  ; inc VPOS
LINE_DYDX_TRBL_SLOPE
   ADD A, 0               ; A=FRAC
   JP NC, LINE_DYDX_TRBL_CNT
    INC E                 ; inc HPOS

LINE_DYDX_TRBL_CNT
   EX AF,AF'              ; 'A=FRAC

   LD D, HIGH ScrBufY     ; SCREEN H TABLE
   LD A,(DE)              ; E=HPOS

   LD H, HIGH ScrBufL     ; SCREEN V TABLE
   LD L,C                 ; retore l
   ADD A,(HL)             ; L=VPOS
   DEC H                  ; ScrBufH
   LD H,(HL)              ; HL = SCREEN POS START LINE
   LD L,A

   INC D                  ; ScrBufOR
   LD A, (DE)
   OR (HL)
   LD (HL), A

   EX AF,AF'              ; restore accum
  DJNZ LINE_DYDX_TRBL_LOOP

RET

LINE_DXDY_TRBL

 LD A, B
 LD B, H
 LD H, A
 CALL SLOPE
 LD (LINE_DXDY_TRBL_SLOPE+1), A

LINE_DXDY_TRBL_LOOP       ; for count eq 0 to dy
   INC E                  ; #4 - inc hpos
LINE_DXDY_TRBL_SLOPE
   ADD A, 127             ; #7 - add slope to accum
   JP NC, LINE_DXDY_TRBL_CNT
    DEC C                 ; #4 - inc vpos

LINE_DXDY_TRBL_CNT
   EX AF,AF'              ; 'A=FRAC

   LD D, HIGH ScrBufY     ; SCREEN H TABLE
   LD A,(DE)              ; E=HPOS

   LD H, HIGH ScrBufL     ; SCREEN V TABLE
   LD L,C                 ; retore l
   ADD A,(HL)             ; L=VPOS
   DEC H                  ; ScrBufH
   LD H,(HL)              ; HL = SCREEN POS START LINE
   LD L,A

   INC D                  ; ScrBufOR
   LD A, (DE)
   OR (HL)
   LD (HL), A

   EX AF,AF'              ; restore accum
  DJNZ LINE_DXDY_TRBL_LOOP

RET














; add A,(HL)

; inc sp
; ld a,(sp)
;; ADD hl,sp



; Integer divide HL by B result A remainder H

SLOPE   ADC     HL,     HL
        LD      A,      H
        JR      C,      LP0
        CP      B
        JR      C,      LPP1
LP0     SUB     B
        LD      H,      A
        OR      A

LPP1    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP1
        CP      B
        JR      C,      LPP2
LP1     SUB     B
        LD      H,      A
        OR      A

LPP2    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP2
        CP      B
        JR      C,      LPP3
LP2     SUB     B
        LD      H,      A
        OR      A

LPP3    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP3
        CP      B
        JR      C,      LPP4
LP3     SUB     B
        LD      H,      A
        OR      A

LPP4    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP4
        CP      B
        JR      C,      LPP5
LP4     SUB     B
        LD      H,      A
        OR      A

LPP5    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP5
        CP      B
        JR      C,      LPP6
LP5     SUB     B
        LD      H,      A
        OR      A

LPP6    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP6
        CP      B
        JR      C,      LPP7
LP6     SUB     B
        LD      H,      A
        OR      A

LPP7    ADC     HL,     HL
        LD      A,      H
        JR      C,      LP7
        CP      B
        JR      C,      LPP8
LP7     SUB     B
        LD      H,      A
        OR      A

LPP8    LD      A,      L
        RLA
        CPL
        RET


MUL_88 ; A+E = A (multipler) * B (multiplicant)
 CP B           ; multipler ge multipicant
 JR NC, M88_NO_SWAP

  LD E, A ; swap multipler and multiplicant
  LD A, B
  LD B, E

M88_NO_SWAP:
 LD C, A        ; backup multiplier
 SUB B          ; subtract multiplicant from multiplier
 RRA            ; difference *2 - multiplier A 9-bit rotation to the right. The carry is copied into bit 7, and the bit leaving on the right is copied into the carry.
 LD D, A        ; D = difference*2
 LD A, C        ; A = multiplier
 ADD A, B
 RRA            ; result *2
 LD L, A            ; l = low element pointer
 LD H, HIGH (sqrlo) ; low table pointer
 LD A, (HL)         ;
 LD E, L
 LD L, D
 JR NC, M88_EVEN

M88_ODD:
 SUB (HL)
 LD L, E
 LD E, A   ; e = result
 INC H          ; sqrhi
 LD A, (HL)
 LD L, D
 SBC A, (HL)
 LD D,A    ; d = result

 LD A,E   ; add b + e
 ADD A,B  ;
 LD E,A   ;
 LD A,D   ; add carry to d
 ADC A,0
RET

M88_EVEN:
 SUB (HL)
 LD L, E    ; l = low pos
 LD E, A
 INC H           ; sqrhi
 LD A, (HL)
 LD L, D    ; l = low pos
 SBC A, (HL)
RET


; LD si, ObjectPoints00  ; points
; LD di, ObjectLines00   ; lines
; LD cl, 8               ; points
; LD ch, 12              ; lines
; CALL DRAW_OBJECT

; Fast 8bit * 8bit Unsigned with only 512 bytes of tables
; Input: A = Multiplier, B = Multiplicant
; Output: A:E = Product
; CPC Cycles: 136-172 (154 on average) = 34-43 (38.5) usec
; Size: 44 bytes of code and 512 bytes for the tables
; Fast 8 bit unsigned multiplication with 16 bit result. It uses formula
; x*y = ((x+y)/2)2 - ((x-y)/2)2, if x+y is even
; = ((x+y-1)/2)2 - ((x-y-1)/2)2 + y, if x+y is odd and x>=y



STX     DEFW    172
ENX     DEFW    0
MDX     DEFW    0
STY     DEFW    172
ENY     DEFW    200
MDY     DEFW    0
STZ     DEFW    16400
ENZ     DEFW    16300
MDZ     DEFW    0
RZ      DEFW    16384

; A
; R
; I
; BC =SX
; DE =SY
; HL =SZ
; SP =EX
; IX =EY
; IY =EZ

MID_POINT:

MP_LOOP:
        LD      HL,     (STX)
        LD      BC,     (ENX)
        ADD     HL,     BC
        SRL     H               ; hl / 2
        RR      L               ; hl / 2
        LD      (MDX),  HL

        LD      HL,     (STY)
        LD      BC,     (ENY)
        ADD     HL,     BC
        SRL     H
        RR      L
        LD      (MDY),  HL

        LD      HL,     (STZ)
        LD      BC,     (ENZ)
        ADD     HL,     BC
        SRL     H
        RR      L
        LD      (MDZ),  HL


        LD      HL,     (MDZ)
        LD      BC,     (RZ)
        XOR     A               ; clc
        SBC     HL,     BC

 RET     Z               ; equal

        JP      C,      MP_GT

MP_LT:
        LD      HL,     (MDX)
        LD      (ENX),  HL
        LD      HL,     (MDY)
        LD      (ENY),  HL
        LD      HL,     (MDZ)
        LD      (ENZ),  HL
        JP      MP_LOOP:

MP_GT:
        LD     HL,      (MDX)
        LD     (STX),   HL
        LD     HL,      (MDY)
        LD     (STY),   HL
        LD     HL,      (MDZ)
        LD     (STZ),   HL
        JP     MP_LOOP:

ALIGN $100

sqrlo ;low(x*x)   should be at the page border
    db 0,1,4,9,$10,$19,$24,$31,$40,$51,$64,$79,$90,$a9,$c4,$e1
    db 0,$21,$44,$69,$90,$b9,$e4,$11,$40,$71,$a4,$d9,$10,$49,$84,$c1
    db 0,$41,$84,$c9,$10,$59,$a4,$f1,$40,$91,$e4,$39,$90,$e9,$44,$a1
    db 0,$61,$c4,$29,$90,$f9,$64,$d1,$40,$b1,$24,$99,$10,$89,4,$81
    db 0,$81,4,$89,$10,$99,$24,$b1,$40,$d1,$64,$f9,$90,$29,$c4,$61
    db 0,$a1,$44,$e9,$90,$39,$e4,$91,$40,$f1,$a4,$59,$10,$c9,$84,$41
    db 0,$c1,$84,$49,$10,$d9,$a4,$71,$40,$11,$e4,$b9,$90,$69,$44,$21
    db 0,$e1,$c4,$a9,$90,$79,$64,$51,$40,$31,$24,$19,$10,9,4,$1
    db 0,1,4,9,$10,$19,$24,$31,$40,$51,$64,$79,$90,$a9,$c4,$e1
    db 0,$21,$44,$69,$90,$b9,$e4,$11,$40,$71,$a4,$d9,$10,$49,$84,$c1
    db 0,$41,$84,$c9,$10,$59,$a4,$f1,$40,$91,$e4,$39,$90,$e9,$44,$a1
    db 0,$61,$c4,$29,$90,$f9,$64,$d1,$40,$b1,$24,$99,$10,$89,4,$81
    db 0,$81,4,$89,$10,$99,$24,$b1,$40,$d1,$64,$f9,$90,$29,$c4,$61
    db 0,$a1,$44,$e9,$90,$39,$e4,$91,$40,$f1,$a4,$59,$10,$c9,$84,$41
    db 0,$c1,$84,$49,$10,$d9,$a4,$71,$40,$11,$e4,$b9,$90,$69,$44,$21
    db 0,$e1,$c4,$a9,$90,$79,$64,$51,$40,$31,$24,$19,$10,9,4,$1
sqrhi ;high(x*x)
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    db 1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3
    db 4,4,4,4,5,5,5,5,6,6,6,7,7,7,8,8
    db 9,9,9,$a,$a,$a,$b,$b,$c,$c,$d,$d,$e,$e,$f,$f
    db $10,$10,$11,$11,$12,$12,$13,$13,$14,$14,$15,$15,$16,$17,$17,$18
    db $19,$19,$1a,$1a,$1b,$1c,$1c,$1d,$1e,$1e,$1f,$20,$21,$21,$22,$23
    db $24,$24,$25,$26,$27,$27,$28,$29,$2a,$2b,$2b,$2c,$2d,$2e,$2f,$30
    db $31,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f
    db $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f
    db $51,$52,$53,$54,$55,$56,$57,$59,$5a,$5b,$5c,$5d,$5f,$60,$61,$62
    db $64,$65,$66,$67,$69,$6a,$6b,$6c,$6e,$6f,$70,$72,$73,$74,$76,$77
    db $79,$7a,$7b,$7d,$7e,$7f,$81,$82,$84,$85,$87,$88,$8a,$8b,$8d,$8e
    db $90,$91,$93,$94,$96,$97,$99,$9a,$9c,$9d,$9f,$a0,$a2,$a4,$a5,$a7
    db $a9,$aa,$ac,$ad,$af,$b1,$b2,$b4,$b6,$b7,$b9,$bb,$bd,$be,$c0,$c2
    db $c4,$c5,$c7,$c9,$cb,$cc,$ce,$d0,$d2,$d4,$d5,$d7,$d9,$db,$dd,$df
    db $e1,$e2,$e4,$e6,$e8,$ea,$ec,$ee,$f0,$f2,$f4,$f6,$f8,$fa,$fc,$fe

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

SCRBUFFER DEFS SCREEN_SIZE+32, 0

MemTop DEFW  0

; Stop planting code after this. (When generating a tape file we save bytes below here).

; AppLast                           EQU *                                    ; The last used byte's address.

; Setup the emulation registers, so Zeus can emulate this code correctly.

Zeus_PC                           EQU Start                             ; Tell the emulator where to start.
Zeus_SP                           EQU MemTop                               ; Tell the emulator where to put the stack.


;Value = 0                       ; Set up a variable
;        repeat                  ; Loop
;          db Value              ; Plant the current value
;          Value = Value + 1     ; Increment it
;        until Value > $100      ; Go back until we've done enough

; h=count
; l=sx
; d=ex or dx
; e=ey or dy
; c=sy
; a=accum


;  4   ld c,a            4   add a,l
;  7   ld b,0            4   ld l,a
;  11  add hl,bc         4   adc a,h
;                        4   sub l
;                        4   ld h,a


