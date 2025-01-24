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

 LD HL, OBJECT_POINTS_00 ; +2 ; +(3*4)-2
 LD DE, OBJECT_LINES_00
 LD B,8
 LD C,12
 CALL DRAW_OBJECT

 LD IX,(x_offset)
 LD A, 1
 CALL HEX16
 LD IX,(y_offset)
 LD A, 2
 CALL HEX16
 LD IX,(z_offset)
 LD A, 3
 CALL HEX16

; LD A,(XO0)
; LD IY,$0005
; CALL HEX8
; LD A,(XO1)
; LD IY,$0006
; CALL HEX8
; LD A,(XO2)
; LD IY,$0007
; CALL HEX8
; LD A,(XO3)
; LD IY,$0008
; CALL HEX8

; LD A,(XN0)
; LD IY,$000A
; CALL HEX8
; LD A,(XN1)
; LD IY,$000B
; CALL HEX8
; LD A,(XN2)
; LD IY,$000C
; CALL HEX8
; LD A,(XN3)
; LD IY,$000D
; CALL HEX8

; LD A,(XO4)
; LD IY,$0010
; CALL HEX8
; LD A,(XO5)
; LD IY,$0011
; CALL HEX8

; LD A,(XN4)
; LD IY,$0013
; CALL HEX8
; LD A,(XN5)
; LD IY,$0014
; CALL HEX8

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
                LD      hl, (x_offset)
                LD      BC,15
                SBC     HL,BC
                LD      (x_offset), hl
Q_KEY_N  LD BC,$FEFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $02         ; z
         JP NZ,Z_KEY_N   ; not pressed
                LD      hl, (x_offset)
                LD      BC,15
                ADD     HL,BC
                LD      (x_offset), hl
Z_KEY_N  LD BC,$FDFE     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $01         ; a
         JP NZ,A_KEY_N   ; not pressed
                LD      hl, (y_offset)
                LD      BC,15
                SBC     HL,BC
                LD      (y_offset), hl
A_KEY_N  LD BC,$fdfe     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $04         ; d
         JP NZ,D_KEY_N   ; not pressed
                LD      hl, (y_offset)
                LD      BC,15
                ADD     HL,BC
                LD      (y_offset), hl
D_KEY_N  LD BC,$fbfe     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $02         ; w
         JP NZ,W_KEY_N   ; not pressed
                LD      hl, (z_offset)
                LD      BC,15
                ADD     HL,BC
                LD      (z_offset), hl
W_KEY_N  LD BC,$fdfe     ; Load BC with the row port address
         IN A,(C)        ; Read the port into the accumulator
         AND $02         ; s
         JP NZ,S_KEY_N   ; not pressed
                LD      hl, (z_offset)
                LD      BC,15
                SBC     HL,BC
                LD      (z_offset), hl
S_KEY_N
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

PLOT2
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
 XOR (HL)
 LD (HL), A
RET

PLOT3
        LD H, HIGH ScrBufH      ; #7 SCREEN V TABLE
        LD D,(HL)               ; #7 14 ; high byte of y
        INC H                   ; #4 18
        LD A,(HL)               ; #7 25 ; low byte of y
        INC H                   ; #4 29
        LD L,C                          ; #4 33
        ADD A,(HL)              ; #7 40 ; x offset from edge
        LD E,A                          ; #4 44
        INC H                   ; #4 48
        LD A,(DE)               ; #7 55
        XOR (HL)                ; #7 62 ; pixel bit within char
        LD (DE),A               ; #7 69
                RET

PLOT4: ; new - c=hor l=ver
;               ld b,high scrbufy       ;               #7
                ld a,(bc)           ; hor   #7 14

;               ld h,high scrbufvlo     ; verlo #7 21
                add A,(hl)              ;       #7 28
                inc h               ; verhi #4 32
                ld h,(hl)           ;       #7 39
                ld l,a              ;       #4 43

                inc b               ; or    #4 47
                ld a,(bc)           ;       #7 54
                or (hl)             ;       #7 61
                ld (hl), a          ;       #7 68
                RET

;B = x, L = y, C = line_table msb
;uses 1KB table, aligned to 256B page
;66 ticks per pixel
;ld h, c                     ;4  table of pixel lines, most significant byte
;ld d, (hl)           ;7  read MSB of line
;inc h                ;4  move to second half of table
;ld a, (hl)           ;7  read LSB of line
;inc h                ;4  move to x offset table
;ld l, b              ;4  x pos as index to table
;or (hl)              ;7  combine pixel line address with byte offset in line
;ld e, a              ;4  complete pointer to pixel in DE
;inc h                ;4  move to bit mask table
;ld a, (de)          ;7  read display byte
;xor (hl)            ;7  xor with bitmask
;ld (de), a          ;7  put pixel

;H = lookup, C =X, L=Y
;ld d,(hl) ;7
;dec h ; 4
;ld a,(hl) ; 7
;dec h ; 4
;ld l,c ; 4
;add a,(hl) ; 7
;ld e,a ; 4
;dec h ; 4
;ld a,(de) ; 7
;xor (hl) ; 7
;ld (hl),a ;7

;I used something similar in Asteroids RX, which I since optimised further. However it also works out to be the same number of Ts. One possible benefit however is that it only uses BC and HL. The 4 x 256 byte lookup tables are in the order of pixel offset within char, x offset from edge, low byte of y and high byte of y.
;l=x ; c =y ; h = lookup

;ld b,(hl) ; pixel value ; 7
;inc h ; 4
;ld a,(hl) ; X offset ; 7
;inc h ; 4
;ld l,c ; Y value ; 4
;or (hl) ; low byte + x offset ; 7
;inc h ; 4
;ld h,(hl) ; high byte ; 7
;ld l,a ; low byte ;4
;ld a,b ; pixel value ;4
;xor (hl) ; mix with screen ; 7
;ld (hl),a ; print ; 7

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

DRAW_LINE_H_SAME LD      A,      B       ; ex
                CP      C               ; lt sx
                JP NZ,  DRAW_LINE_NOT_PLOT
                RET

DRAW_LINE       LD      A,      H               ; ey
                CP      E                       ; lt sy
                JP Z,   DRAW_LINE_H_SAME
                JP NC,  DRAW_LINE_NOSWAP
                        LD      A,      C       ; swap x (C/B)
                        LD      C,      B
                        LD      B,      A
                        LD      A,      E       ; swap y (E/H)
                        LD      E,      H
                        LD      H,      A

DRAW_LINE_NOSWAP        LD      A,      B       ; ex
                        CP      C               ; lt sx
DRAW_LINE_NOT_PLOT      JP C,   LINE_V4_V2
;;                jp      DRAW_LINE_TLBR

; C=SX
; E=SY
; B=EX/DX
; H=EY/DY

LINE_V4                 LD      A,      B       ; ex
                        SUB     C               ; sx
                        LD      B,      A       ; b=dx=ex-sx
                        LD      A,      H       ; ey
                        SUB     E               ; sy
                        LD      H,      A       ; h=dy=ey-sy
                        CP      B               ; dy GT dx
                        JR NC,  LINE_DXDY_TLBR

LINE_DYDX_TLBR          CALL    SLOPE
                        LD      (LINE_DYDX_TLBR_SLOPE+1),       A
                        LD      D, HIGH ScrBufOR

LINE_DYDX_TLBR_LOOP             INC     C                               ; for count eq 0 to dy - inc VPOS
LINE_DYDX_TLBR_SLOPE            ADD     A,      0                       ; A=FRAC
                                JP NC,  LINE_DYDX_TLBR_CNT
                                        INC     E                       ; inc HPOS
LINE_DYDX_TLBR_CNT              EX      AF,     AF'                     ; 'A=FRAC
                                DEC     D                               ; SCREEN H TABLE
                                LD      A,      (DE)                    ; E=HPOS
                                LD      H,      HIGH ScrBufL            ; SCREEN V TABLE
                                LD      L,      C                       ; retore l
                                ADD     A,      (HL)                    ; L=VPOS
                                DEC     H                               ; ScrBufH
                                LD      H,      (HL)                    ; HL = SCREEN POS START LINE
                                LD      L,      A
                                INC     D                               ; ScrBufOR
                                LD      A,      (DE)
                                OR      (HL)
                                LD      (HL),   A
                                EX      AF,     AF'              ; restore accum
                        DJNZ    LINE_DYDX_TLBR_LOOP
                        RET

LINE_DXDY_TLBR          LD      A,      B
                        LD      B,      H
                        LD      H,      A
                        CALL    SLOPE
                        LD      (LINE_DXDY_TLBR_SLOPE+1),       A
                        LD      D, HIGH ScrBufOR

LINE_DXDY_TLBR_LOOP     INC     E                       ; for count eq 0 to dy ; #4 - inc hpos
LINE_DXDY_TLBR_SLOPE            ADD     A,      127             ; #7 - add slope to accum
                                JP      NC, LINE_DXDY_TLBR_CNT
                                        INC     C               ; #4 - inc vpos
LINE_DXDY_TLBR_CNT              EX      AF,     AF'             ; 'A=FRAC
                                DEC     D                       ; SCREEN H TABLE
                                LD      A,      (DE)            ; E=HPOS
                                LD      H,      HIGH ScrBufL    ; SCREEN V TABLE
                                LD      L,      C               ; retore l
                                ADD     A,      (HL)            ; L=VPOS
                                DEC     H                       ; ScrBufH
                                LD      H,      (HL)            ; HL = SCREEN POS START LINE
                                LD      L,      A
                                INC     D                  ; ScrBufOR
                                LD      A,      (DE)
                                OR      (HL)
                                LD      (HL),   A
                                EX      AF,     AF'              ; restore accum
                        DJNZ    LINE_DXDY_TLBR_LOOP
                        RET

 ld sp, ix
 ld sp, iy

; C=SX
; E=SY
; B=EX/DX
; H=EY/DY

LINE_V4_V2              LD      A,      C       ; ex
                        SUB     B               ; sx
                        LD      B,      A       ; b=dx=ex-sx
                        LD      A,      H       ; ey
                        SUB     E               ; sy
                        LD      H,      A       ; h=dy=ey-sy
                        CP      B               ; dy GT dx
                        JR NC,  LINE_DXDY_TRBL

LINE_DYDX_TRBL          CALL    SLOPE                            ; Integer divide HL by B result A remainder H
                        LD      (LINE_DYDX_TRBL_SLOPE+1),       A
                        LD      D, HIGH ScrBufOR

LINE_DYDX_TRBL_LOOP             DEC     C                        ; for count eq 0 to dy ; inc VPOS
LINE_DYDX_TRBL_SLOPE            ADD     A,      0                ; A=FRAC
                                JP NC,  LINE_DYDX_TRBL_CNT
                                        INC E                    ; inc HPOS
LINE_DYDX_TRBL_CNT              EX      AF,     AF'              ; 'A=FRAC
                                DEC     D                        ; SCREEN H TABLE
                                LD      A,      (DE)             ; E=HPOS
                                LD      H,      HIGH ScrBufL     ; SCREEN V TABLE
                                LD      L,      C                ; retore l
                                ADD     A,      (HL)             ; L=VPOS
                                DEC     H                        ; ScrBufH
                                LD      H,      (HL)             ; HL = SCREEN POS START LINE
                                LD      L,      A
                                INC     D                        ; ScrBufOR
                                LD      A,      (DE)
                                OR      (HL)
                                LD      (HL),   A
                                EX      AF,     AF'              ; restore accum
                        DJNZ LINE_DYDX_TRBL_LOOP
                        RET

LINE_DXDY_TRBL          LD      A,      B
                        LD      B,      H
                        LD      H,      A
                        CALL    SLOPE
                        LD      (LINE_DXDY_TRBL_SLOPE+1),       A
                        LD      D, HIGH ScrBufOR

LINE_DXDY_TRBL_LOOP             INC     E                       ; for count eq 0 to dy ; #4 - inc hpos
LINE_DXDY_TRBL_SLOPE            ADD     A,      127             ; #7 - add slope to accum
                                JP NC,  LINE_DXDY_TRBL_CNT
                                        DEC     C               ; #4 - inc vpos
LINE_DXDY_TRBL_CNT              EX      AF,     AF'             ; 'A=FRAC
                                DEC     D                       ; SCREEN H TABLE
                                LD      A,      (DE)            ; E=HPOS
                                LD      H,      HIGH ScrBufL    ; SCREEN V TABLE
                                LD      L,      C               ; retore l
                                ADD     A,      (HL)            ; L=VPOS
                                DEC     H                       ; ScrBufH
                                LD      H,      (HL)            ; HL = SCREEN POS START LINE
                                LD      L,      A
                                INC     D                       ; ScrBufOR
                                LD      A,      (DE)
                                OR      (HL)
                                LD      (HL),   A
                                EX      AF,     AF'             ; restore accum
                        DJNZ LINE_DXDY_TRBL_LOOP
                        RET

x_offset dw -10
y_offset dw -10
z_offset dw 1900

x_rot_pnt dw 0
y_rot_pnt dw 0
z_rot_pnt dw 0

x_pers_pnt dw 0
y_pers_pnt dw 0

; hl = line list address
; ix = point list address
; bc = b = point / c = lines

OBJECT_POINTS_00
 DEFW +350,+450,+350
 DEFW -350,+350,+350
 DEFW +350,-450,+350
 DEFW -350,-350,+350
 DEFW +350,+450,-350
 DEFW -350,+350,-350
 DEFW +350,-450,-350
 DEFW -350,-350,-350

OBJECT_LINES_00
 DEFB 1,2
 DEFB 2,4
 DEFB 4,3
 DEFB 3,1
 DEFB 5,6
 DEFB 6,8
 DEFB 8,7
 DEFB 7,5
 DEFB 1,5
 DEFB 2,6
 DEFB 3,7
 DEFB 4,8

DRAW_OBJECT
 PUSH   DE        ; line address
 PUSH   BC        ; line count

ROTATE_POINT_LOOP
  PUSH  BC                              ; backup point counter
  ld    a,b
  ld    (ROTATE_POINT_COUNTER+1), a     ; backup point counter

  ld    (DRAW_OBJ_SP+1), sp             ; backup sp
  ld    sp, HL                          ; move sp to point pos

  ld    bc, 6                           ; inc point pos
  add   HL,BC

  pop   bc                              ; x point
  pop   de                              ; y point
  pop   af                              ; z point

DRAW_OBJ_SP     ld sp,1234              ; restore sp
  push    HL                            ; store point pos

        CALL ROTATE_POINT                       ; rotate bc de af and put them back into these variables

  push  af                              ; backup z point
  push  de                              ; backup y point

  ld    hl,(x_offset)                   ; get x offset
  add   hl,bc                           ; calc RX
  ex    de, hl                          ; move hl to de

ROTATE_POINT_COUNTER    ld l,0

  ld    h, high RX_LO                   ; point to RX_LO table

  ld    (hl), e                         ; store RX_LO value
  inc   h                               ; point to RX_HI table
  ld    (hl), d                         ; store RX_HI value
  inc   h                               ; point to RY_LO table
  ld (x_rot_pnt),de                     ; self modifiying code instead - todo

  ex    de, hl                          ; backup table in hl to de

  pop   hl                              ; get y rot
  ld    bc,(y_offset)                   ; get y offset
  add   hl,bc                           ; calc RY
  ex    de, hl                          ; hl now has table and de has roty

  ld    (hl), e                         ; store RY_LO value
  inc   h                               ; point to RY_HI table
  ld    (hl), d                         ; store RY_HI value
  inc   h                               ; point to RZ_LO table
  ld (y_rot_pnt),de                     ; self modifiying code instead - todo

  ex    de, hl                          ; backup table in hl to de

  pop   hl                              ; get z rot
  ld    bc,(z_offset)                   ; get z offset
  add   hl,bc                           ; calc RZ
  ex    de, hl                          ; hl now has table and de has roty

  ld    (hl), e                         ; store RZ_LO value
  inc   h                               ; point to RZ_HI table
  ld    (hl), d                         ; store RZ_HI value
  inc   h                               ; point to PX_LO table
  ld (z_rot_pnt),de                     ; self modifiying code instead - todo

  call PERSPECTIVE_POINT

; get x_pers_pnt and add middle and then calc clip code
  ld    de,(x_pers_pnt)         ; get v offset
  ld    a,96                                    ; add middle
  add   a, e
  ld    (hl), a                 ; store PX_LO value
  jp nc, cont1
        inc             d
cont1
  inc   h                       ; point to PX_HI table
  ld    (hl), d                 ; store PX_HI value
  inc   h                       ; point to PY_LO table

; get y_pers_pnt and add middle and then calc clip code
  ld    de,(y_pers_pnt)         ; get h offset
  ld    a,128                                   ; add middle
  add   a,e
  ld    (hl), a                 ; store PY_LO value
 jp nc, cont2
        inc             e
cont2
  inc   h                       ; point to PY_HI table
  ld    (hl), e                 ; store PY_HI value

;       call CLIP_CODE

  inc   h                       ; point to CC table
  ld    (hl), a                 ; store CC value

; or cc with start value of 0
; and cc with start value of 255

  pop   HL
  pop   bc
  djnz  ROTATE_POINT_LOOP

 pop    bc ; line count
; ld     b,c ; line count
 pop    hl ; line address

; check if all point off
; check if all points on

DRAW_UNCLIPPED

 ld     b,c ; line count


DRAW_UNCLIPPED_LOOP
  push  bc

  ld    b, high PX_LO       ; perspect low table
  ld    d, high PY_LO       ; perspect low table

  ld    c, (hl)             ; get start line point
  ld    e, c
  ld    a, (bc)             ; get PX for first point
  ld    ixl, a
  ld    a, (de)             ; get PY for first point
  ld    ixh, a
  inc   hl                 ; next point

  ld    c, (hl)             ; get end line point
  ld    e, c
  ld    a, (bc)             ; get PX for second point
  ld    iyl, a
  ld    a, (de)             ; get PY for second point
  ld    iyh, a
  inc   hl                 ; next point

  push hl
  ld   a,ixl
  ld   c,a
  ld   a,ixh
  ld   e,a
  ld   a,iyl
  ld   b,a
  ld   a,iyh
  ld   h,a
  call DRAW_LINE

  pop  hl
  pop   bc
  djnz  DRAW_UNCLIPPED_LOOP
RET

X_ROTATE_POINT DW 0
Y_ROTATE_POINT DW 0
Z_ROTATE_POINT DW 0

ROTATE_POINT ; rotate bc de af and put them back into these variables
        LD      (X_ROTATE_POINT),       BC
        LD      (Y_ROTATE_POINT),       DE
        PUSH    AF
        EXX
        POP     HL
        LD      (Z_ROTATE_POINT),       HL

; do rotation

        LD      HL,     (Z_ROTATE_POINT)
        PUSH    HL
        POP     AF
        EXX
        LD      DE,     (Y_ROTATE_POINT)
        LD      BC,     (X_ROTATE_POINT)

RET

SLOPE ; h*b
 ld a,h
 ex af, af'
 ld a,b
 exx

 ld l, a

 LD H, HIGH RECIPTAB
 LD A, (HL)
 LD IYL,A
 INC H
 LD A, (HL)
 LD IYH,A

 ex af, af'
 ld d, a

        LD      A,    IYL       ; IXL*D
        LD      L,    D

                LD      H,   HIGH UMUL_TAB      ; point to table            ; #7
                LD      B,   H                  ; point to table            ; #4
                ADD     L                       ; multipler + multiplicand  ; #4
                LD      C,   A                  ; into c                    ; #4
                JP NC,  NO_OVERFLOW             ; wrap around               ; #10
                        INC     B                       ; inc pointer                  ; #4
                        INC     B                       ; inc pointer to neg table ?   ; #4
NO_OVERFLOW     SUB     L                       ; - multiplicand            ; #4
                SUB     L                       ; - multiplicand            ; #4
                JP NC,  NO_NEG                  ; wrap around               ; #10
                        NEG                             ; negate multiplier ; #8
;                        AND     A
NO_NEG          LD      L,      A               ; multiplier to hl          ; #4
 XOR     A ; clear carry
                INC     B                       ;    ; #4
                INC     H                       ;    ; #4
                LD      A,      (BC)            ;    ; #7
                SBC     (HL)                    ;    ; #7
                LD      E,      A               ;    ; #4

        LD      A,    IYH       ; IXH*D
        LD      L,    D

                LD      H,   HIGH UMUL_TAB      ; point to table            ; #7
                LD      B,   H                  ; point to table            ; #4
                ADD     L                       ; multipler + multiplicand  ; #4
                LD      C,   A                  ; into c                    ; #4
                JP NC,  NO_OVERFLOW2             ; wrap around               ; #10
                        INC     B                       ; inc pointer                  ; #4
                        INC     B                       ; inc pointer to neg table ?   ; #4
NO_OVERFLOW2    SUB     L                       ; - multiplicand            ; #4
                SUB     L                       ; - multiplicand            ; #4
                JP NC,  NO_NEG2                  ; wrap around               ; #10
                        NEG                             ; negate multiplier ; #8
NO_NEG2         LD      L,      A               ; multiplier to hl          ; #4
                LD      A,      (BC)            ;    ; #7
                SUB     (HL)                    ;    ; #7

        ADD      A, E

 exx
        RET

; look up reciprocal
; recip lo * a
; recip hi * a

ret

MUL_SHIFT_V2 MACRO () ; 1 shift of a 16 bit multiply
                        rl e
                        rl d
                        jr nc,muluw_cont
                                add hl,bc
                                jr nc,muluw_cont
                                        inc de
muluw_cont
MEND

;16*16 multiplication
;The following routine multiplies bc by de and places the result in dehl.
DIV_UMUL_1616_V7 proc

   ld   hl, 0

   sla  e               ; optimised 1st iteration
   rl   d
   jr   nc, _cont
	ld   h, b
	ld   l, c
_cont

                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2
                                add hl,hl
                                MUL_SHIFT_V2 () ;2

    ld l,h
    ld h,e

   ret
   endp


PERS_SHIFT MACRO () ; shift z/x/y
 SRL     D
 RR      E
 SRL     H
 RR      L
 SRL     B
 RR      C
MEND

NEGATE_16 MACRO ( LO, HI )
 XOR    A
 SUB    LO
 LD     LO,     A
 SBC    A,      A
 SUB    HI
 LD     HI,     A
ENDM


BC_Div_DE proc
;BC/DE ==> BC, remainder in HL
;NOTE: BC/0 returns 0 as the quotient.
;min: 738cc
;max: 898cc
;avg: 818cc
;144 bytes
  xor a
  ld h,a
  ld l,a
  sub e
  ld e,a
  sbc a,a
  sub d
  ld d,a

  ld a,b
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  ld b,a

  ld a,c
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  adc hl,hl
  add hl,de
  jr c,$+4
    sbc hl,de
  rla
  ld c,a

  ret
  endp

ALIGN $100
PERSPECTIVE_POINT:
                exx
				ld      HL,     (z_rot_pnt)

                ld      c,      H
                ld      b, HIGH PPTABLO
                ld      a,      (bc)
                ld      (PP_SFT+1),       A

                ld      DE,     (x_rot_pnt)
                ld      a,      D
                ld      (positive2xtst+1),a
                bit     7,      a
                jp      z,positive2x     ; positive
                        NEGATE_16 (E, D)

positive2x      ld      bc,     (y_rot_pnt)
                ld      a,      b
                ld      (positive2ytst+1),a
                bit     7,      a
                jp      z,positive2y     ; positive
                        NEGATE_16 (C, B)
positive2y

PP_SFT:   JP   PP_SFT_8

PP_SFT_8: PERS_SHIFT ()
PP_SFT_7: PERS_SHIFT ()
PP_SFT_6: PERS_SHIFT ()
PP_SFT_5: PERS_SHIFT ()
PP_SFT_4: PERS_SHIFT ()
PP_SFT_3: PERS_SHIFT ()
PP_SFT_2: PERS_SHIFT ()
PP_SFT_1: PERS_SHIFT ()
PP_SFT_0:

; bc = x
; hl = y
 push bc ; hl
; push de ; bc
; e = reciprical of z
; lookup reciprical of e

 LD H, HIGH RECIPTAB
 LD C, (HL)
 INC H
 LD B, (HL)

 PUSH BC

                CALL    DIV_UMUL_1616_V7 ; IX = reciprical DE = x
positive2xtst   ld      A,      #0                           ;  ??
                bit     7,      A
                jp z,   positive2x1
                        NEGATE_16 (H, L)
positive2x1
;       ld      a,      96                                      ; v middle
;               add     a,      L
;               ld              l,      a
;               jp, nc  pvcont
;                               inc             h
pvcont  ld      (x_pers_pnt),   HL

 pop bc
 pop de
                CALL    DIV_UMUL_1616_V7 ; IX = reciprical DE = y
positive2ytst   ld      A,      #0                           ;  ??
                bit     7,      A
                jp z,   positive2y1
                        NEGATE_16 (H, L)
positive2y1
;       ld      a,      128                                     ; h middle
;               add     a,      L
;               ld              l,      a
;               jp, nc  phcont
;                               inc             h
phcont  ld      (y_pers_pnt),   HL

 exx
 ret

MUL_SHIFT MACRO () ; 1 shift of a 16 bit multiply
                        rl e
                        rl d
                        jr nc, muluw_cont
                                add hl,bc
                                jr nc, muluw_cont
                                        inc de
muluw_cont
MEND

UMUL_88 ; input A*L output DE
        UMUL88 (E)
        LD D, A
        RET

;Very fast 8bit * 8bit Unsigned with only 1K of tables
;Cycles: 104-112 (108 on average) = 26-28 (27) usec
;Size: 24 bytes of code and 1024 bytes for the tables
;Uses the formula ab = ((a + b)2 - (a - b)2) / 4. It's based on a routine for the 6502 by Stephen Judd in a C= Hacking article. Because of differences between the way the 6502 does register indexing it was quite difficult to actually get this working, but it's a great compromise between speed and space since it only uses 1K of tables (as opposed to the 16K or 8K table routines above), and can still manage to do the job in a maximum of 28 microseconds.
;Input: A = Multiplier, L = Multiplicand
;Output: DE = Product

UMUL88  MACRO (RESULT_LO) ; A=result hi
                LD      H,   HIGH UMUL_TAB      ; point to table            ; #7
                LD      B,   H                  ; point to table            ; #4
                ADD     L                       ; multipler + multiplicand  ; #4
                LD      C,   A                  ; into c                    ; #4
                JP NC,  NO_OVERFLOW             ; wrap around               ; #10
                        INC     B                       ; inc pointer                  ; #4
                        INC     B                       ; inc pointer to neg table ?   ; #4
NO_OVERFLOW     SUB     L                       ; - multiplicand            ; #4
                SUB     L                       ; - multiplicand            ; #4
                JP NC,  NO_NEG                  ; wrap around               ; #10
                        NEG                             ; negate multiplier ; #8
NO_NEG          LD      L,      A               ; multiplier to hl          ; #4
                LD      A,      (BC)            ;    ; #7
                SUB     (HL)                    ;    ; #7
                LD      RESULT_LO,      A               ;    ; #4
; XOR A ; clear carry
                INC     B                       ;    ; #4
                INC     H                       ;    ; #4
                LD      A,      (BC)            ;    ; #7
                SBC     (HL)                    ;    ; #7
ENDM

UMUL88H MACRO ()
                LD      H,   HIGH UMUL_TAB      ; point to table            ; #7
                LD      B,   H                  ; point to table            ; #4
                ADD     L                       ; multipler + multiplicand  ; #4
                LD      C,   A                  ; into c                    ; #4
                JP NC,  NO_OVERFLOW             ; wrap around               ; #10
                        INC     B                       ; inc pointer                  ; #4
                        INC     B                       ; inc pointer to neg table ?   ; #4
NO_OVERFLOW     SUB     L                       ; - multiplicand            ; #4
                SUB     L                       ; - multiplicand            ; #4
                JP NC,  NO_NEG                  ; wrap around               ; #10
                        NEG                             ; negate multiplier ; #8
NO_NEG          LD      L,      A               ; multiplier to hl          ; #4
; XOR A ; clear carry
                INC     B                       ;    ; #4
                INC     H                       ;    ; #4
                LD      A,      (BC)            ;    ; #7
                SBC     (HL)                    ;    ; #7
ENDM

UMUL88L MACRO ()
                LD      H,   HIGH UMUL_TAB      ; point to table            ; #7
                LD      B,   H                  ; point to table            ; #4
                ADD     L                       ; multipler + multiplicand  ; #4
                LD      C,   A                  ; into c                    ; #4
                JP NC,  NO_OVERFLOW             ; wrap around               ; #10
                        INC     B                       ; inc pointer                  ; #4
                        INC     B                       ; inc pointer to neg table ?   ; #4
NO_OVERFLOW     SUB     L                       ; - multiplicand            ; #4
                SUB     L                       ; - multiplicand            ; #4
                JP NC,  NO_NEG                  ; wrap around               ; #10
                        NEG                             ; negate multiplier ; #8
NO_NEG          LD      L,      A               ; multiplier to hl          ; #4
                LD      A,      (BC)            ;    ; #7
                SUB     (HL)                    ;    ; #7
ENDM

; Diagram of the additions
;                 y1    y0
;              x  x1    x0
;                 --------
;              x0y0h x0y0l
; +      x0y1h x0y1l
; +      x1y0h x1y0l
; +x1y1h x1y1l
; ------------------------
;     z3    z2    z1    z0

;MUL1616RC DB 0,0,0,0

;     AAaa ; IX = 0?
;   BBbb   ; HL
;   CCcc   ; BC
; DDdd     ; DE = ?0
; R3R2R1R0

DIV_UMUL_1616 ; input IX*DE output IX-IY (MUL1616RC)
        LD      A,    IXL       ; IXL*E
        LD      L,    E
        UMUL88H ()
        EX      AF,   AF'       ; AA

        LD      A,    IXL       ; IXL*D
        LD      L,    D
        UMUL88 (IYL)
        LD      IYH,  A
        PUSH    IY              ;CCcc

        LD      A,    IXH       ; IXH*E
        LD      L,    E
        UMUL88 (E)
        LD      L,    D         ; IXH*D (used below)
        LD      D,  A

        LD      A,    IXH       ; IXH*D
;        LD      L,    D         ; IXH*D (previously setup above)
        UMUL88L ()

        POP     HL      ; CCcc

        EX      AF,   AF'     ; AA
        ADD     A,    L       ; BBbb + 00AA
        LD      L,    A
        JP      NC, DUM1616NC
                INC   H
DUM1616NC
        EX      AF,   AF'     ; dd
        ADD     HL,   DE      ; BBbb + CCcc ;ADD?
        ADD     A, H          ; BB + CC + dd ;ADD?
        LD      H, A

;     AAaa ; IX = 0?
;   BBbb   ; HL
;   CCcc   ; BC
; DDdd     ; DE = ?0
; R3R2R1R0

        RET

;        add ix,iy
;        add     ix,hl
        add     iy,bc
        add     iy,de
;        add     ix,iy
 ret


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
PPTABLO:
 DB  LOW PP_SFT_0
 DB  LOW PP_SFT_1
 DB  LOW PP_SFT_2, LOW PP_SFT_2
 DB  LOW PP_SFT_3, LOW PP_SFT_3, LOW PP_SFT_3, LOW PP_SFT_3

 DB  LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4, LOW PP_SFT_4

 DB  LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5
 DB  LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5, LOW PP_SFT_5

 DB  LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6
 DB  LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6
 DB  LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6
 DB  LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6, LOW PP_SFT_6

 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7
 DB  LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7, LOW PP_SFT_7

 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8
 DB  LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8, LOW PP_SFT_8

ALIGN $100
RECIPTAB
REC16LO
 DB 255,  0, 85,  0, 51,170,146,  0,113,153, 69, 85,177, 73, 17,  0
 DB  15, 56,121,204, 48,162, 33,170, 61,216,123, 36,211,136, 66,  0
 DB 193,135, 80, 28,235,188,144,102, 62, 24,244,209,176,144,114, 85
 DB  57, 30,  5,236,212,189,167,146,125,105, 86, 68, 50, 33, 16,  0
 DB 240,224,210,195,181,168,155,142,129,117,105, 94, 83, 72, 61, 51
 DB  41, 31, 21, 12,003,250,241,232,224,216,208,200,192,185,177,170
 DB 163,156,149,143,136,130,124,118,112,106,100, 94, 89, 83, 78, 73
 DB  67, 62, 57, 52, 48, 43, 38, 34, 29, 25, 20, 16, 12,  8,  4,  0
 DB 252,248,244,240,236,233,229,225,222,218,215,212,208,205,202,199
 DB 195,192,189,186,183,180,178,175,172,169,166,164,161,158,156,153
 DB 151,148,146,143,141,138,136,134,131,129,127,125,122,120,118,116
 DB 114,112,110,108,106,104,102,100, 98, 96, 94, 92, 90, 88, 87, 85
 DB  83, 81, 80, 78, 76, 74, 73, 71, 70, 68, 66, 65, 63, 62, 60, 59
 DB  57, 56, 54, 53, 51, 50, 48, 47, 46, 44, 43, 41, 40, 39, 37, 36
 DB  35, 33, 32, 31, 30, 28, 27, 26, 25, 24, 22, 21, 20, 19, 18, 17
 DB  15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0
REC16HI
 DB 255,128, 85, 64, 51, 42, 36, 32, 28, 25, 23, 21, 19, 18, 17, 16
 DB  15, 14, 13, 12, 12, 11, 11, 10, 10,  9,  9,  9,  8,  8,  8,  8
 DB 007,007,007,007,006,006,006,006,006,006,005,005,005,005,005,005
 DB 005,005,005,004,004,004,004,004,004,004,004,004,004,004,004,004
 DB 003,003,003,003,003,003,003,003,003,003,003,003,003,003,003,003
 DB 003,003,003,003,003,002,002,002,002,002,002,002,002,002,002,002
 DB 002,002,002,002,002,002,002,002,002,002,002,002,002,002,002,002
 DB 002,002,002,002,002,002,002,002,002,002,002,002,002,002,002,002
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
 DB 001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001

ALIGN $100
UMUL_TAB:
 DB $00, $00, $01, $02, $04, $06, $09, $0C, $10, $14, $19, $1E, $24, $2A, $31, $38
 DB $40, $48,    $51,    $5A,    $64,    $6E,    $79,    $84,    $90,    $9C,    $A9,    $B6,    $C4,    $D2,    $E1, $F0
 DB $00, $10,    $21,    $32,    $44,    $56,    $69,    $7C,    $90,    $A4,    $B9,    $CE,    $E4,    $FA,    $11, $28
 DB $40, $58,    $71,    $8A,    $A4,    $BE,    $D9,    $F4,    $10,    $2C,    $49,    $66,    $84,    $A2,    $C1, $E0
 DB $00, $20,    $41,    $62,    $84,    $A6,    $C9,    $EC,    $10,    $34,    $59,    $7E,    $A4,    $CA,    $F1, $18
 DB $40, $68,    $91,    $BA,    $E4,    $E0,    $39,    $64,    $90,    $BC,    $E9,    $16,    $44,    $72,    $A1, $D0
 DB $00, $30,    $61,    $92,    $C4,    $F6,    $29,    $5C,    $90,    $C4,    $F9,    $2E,    $64,    $9A,    $D1, $08
 DB $40, $78,    $B1,    $EA,    $24,    $5E,    $99,    $D4,    $10,    $4C,    $89,    $C6,    $04,    $42,    $81, $C0
 DB $00, $40,    $81,    $C2,    $04,    $46,    $F9,    $CC,    $10,    $54,    $99,    $DE,    $24,    $6A,    $B1, $F8
 DB $40, $88,    $D1,    $1A,    $64,    $AE,    $F9,    $44,    $90,    $DC,    $29,    $76,    $C4,    $12,    $61, $B0
 DB $00, $50,    $A1,    $F2,    $44,    $96,    $E9,    $3C,    $90,    $E4,    $39,    $8E,    $E4,    $3A,    $91, $E8
 DB $40, $98,    $F1,    $4A,    $A4,    $FE,    $F9,    $B4,    $10,    $6C,    $C9,    $26,    $84,    $E2,    $41, $A0
 DB $00, $60,    $C1,    $22,    $84,    $E6,    $49,    $AC,    $10,    $74,    $D9,    $3E,    $A4,    $0A,    $71, $D8
 DB $40, $A8,    $11,    $7A,    $E4,    $4E,    $B9,    $24,    $90,    $FC,    $69,    $D6,    $44,    $B2,    $21, $90
 DB $00, $70,    $E1,    $52,    $C4,    $36,    $A9,    $1C,    $90,    $04,    $79,    $EE,    $64,    $DA,    $51, $C8
 DB $40, $B8,    $31,    $AA,    $24,    $9E,    $19,    $94,    $10,    $8C,    $09,    $86,    $04,    $82,    $01, $80
 DB $00, $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00, $00
 DB $00, $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00,    $00, $00
 DB $01, $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $01,    $02, $02
 DB $02, $02,    $02,    $02,    $02,    $02,    $02,    $02,    $03,    $03,    $03,    $03,    $03,    $03,    $03, $03
 DB $04, $04,    $04,    $04,    $04,    $04,    $04,    $04,    $05,    $05,    $05,    $05,    $05,    $05,    $05, $05
 DB $06, $06,    $06,    $06,    $06,    $07,    $07,    $07,    $07,    $07,    $07,    $08,    $08,    $08,    $08, $08
 DB $09, $09,    $09,    $09,    $09,    $09,    $0A,    $0A,    $0A,    $0A,    $0A,    $0B,    $0B,    $0B,    $0B, $0C
 DB $0C, $0C,    $0C,    $0C,    $0D,    $0D,    $0D,    $0D,    $0E,    $0E,    $0E,    $0E,    $0F,    $0F,    $0F, $0F
 DB $10, $10,    $10,    $10,    $11,    $11,    $11,    $11,    $12,    $12,    $12,    $12,    $13,    $13,    $13, $13
 DB $14, $14,    $14,    $15,    $15,    $15,    $15,    $16,    $16,    $16,    $17,    $17,    $17,    $18,    $18, $18
 DB $19, $19,    $19,    $19,    $1A,    $1A,    $1A,    $1B,    $1B,    $1B,    $1C,    $1C,    $1C,    $1D,    $1D, $1D
 DB $1E, $1E,    $1E,    $1F,    $1F,    $1F,    $20,    $20,    $21,    $21,    $21,    $22,    $22,    $22,    $23, $23
 DB $24, $24,    $24,    $25,    $25,    $25,    $26,    $26,    $27,    $27,    $27,    $28,    $28,    $29,    $29, $29
 DB $2A, $2A,    $2B,    $2B,    $2B,    $2C,    $2C,    $2D,    $2D,    $2D,    $2E,    $2E,    $2F,    $2F,    $30, $30
 DB $31, $31,    $31,    $32,    $32,    $33,    $33,    $34,    $34,    $35,    $35,    $35,    $36,    $36,    $37, $37
 DB $38, $38,    $39,    $39,    $3A,    $3A,    $3B,    $3B,    $3C,    $3C,    $3D,    $3D,    $3E,    $3E,    $3F, $3F
 DB $00, $80,    $01,    $82,    $04,    $86,    $09,    $8C,    $10,    $94,    $19,    $9E,    $24,    $AA,    $31, $B8
 DB $40, $C8,    $51,    $DA,    $64,    $EE,    $79,    $04,    $90,    $1C,    $A9,    $36,    $C4,    $52,    $E1, $70
 DB $00, $90,    $21,    $B2,    $44,    $D6,    $69,    $FC,    $90,    $24,    $B9,    $4E,    $E4,    $7A,    $11, $A8
 DB $40, $D8,    $71,    $0A,    $A4,    $3E,    $D9,    $74,    $10,    $AC,    $49,    $E6,    $84,    $22,    $C1, $60
 DB $00, $A0,    $41,    $E2,    $84,    $26,    $C9,    $6C,    $10,    $B4,    $59,    $FE,    $A4,    $4A,    $F1, $98
 DB $40, $E8,    $91,    $3A,    $E4,    $8E,    $39,    $E4,    $90,    $3C,    $E9,    $96,    $44,    $F2,    $A1, $50
 DB $00, $B0,    $61,    $12,    $C4,    $76,    $29,    $DC,    $90,    $44,    $F9,    $AE,    $64,    $1A,    $D1, $88
 DB $40, $F8,    $B1,    $6A,    $24,    $DE,    $99,    $54,    $10,    $CC,    $89,    $46,    $02,    $C2,    $81, $40
 DB $00, $C0,    $81,    $42,    $04,    $C6,    $89,    $4C,    $10,    $D4,    $99,    $5E,    $24,    $EA,    $B1, $78
 DB $40, $08,    $D1,    $9A,    $64,    $2E,    $F9,    $C4,    $90,    $5C,    $29,    $F6,    $C4,    $92,    $61, $30
 DB $00, $D0,    $A1,    $72,    $44,    $16,    $E9,    $BC,    $90,    $64,    $39,    $E0,    $E4,    $BA,    $91, $68
 DB $40, $18,    $F1,    $CA,    $A4,    $7E,    $59,    $34,    $10,    $EC,    $C9,    $A6,    $84,    $62,    $41, $20
 DB $00, $E0,    $C1,    $A2,    $84,    $66,    $49,    $2C,    $10,    $F4,    $D9,    $BE,    $A4,    $8A,    $71, $58
 DB $40, $28,    $11,    $FA,    $E4,    $CE,    $B9,    $A4,    $90,    $7C,    $69,    $56,    $44,    $32,    $21, $10
 DB $00, $F0,    $E1,    $D2,    $C4,    $B6,    $A9,    $9C,    $90,    $84,    $79,    $6E,    $64,    $5A,    $51, $48
 DB $40, $38,    $31,    $2A,    $24,    $1E,    $19,    $14,    $10,    $0C,    $09,    $06,    $42,    $02,    $01, $00
 DB $40, $40,    $41,    $41,    $42,    $42,    $43,    $43,    $44,    $44,    $45,    $45,    $46,    $46,    $47, $47
 DB $48, $48,    $49,    $49,    $4A,    $4A,    $4B,    $4C,    $4C,    $4D,    $4D,    $4E,    $4E,    $4F,    $4F, $50
 DB $51, $51,    $52,    $52,    $53,    $53,    $54,    $54,    $55,    $56,    $56,    $57,    $57,    $58,    $59, $59
 DB $5A, $5A,    $5B,    $5C,    $5C,    $5D,    $5D,    $5E,    $5F,    $5F,    $60,    $60,    $61,    $62,    $62, $63
 DB $64, $64,    $65,    $65,    $66,    $67,    $67,    $68,    $69,    $69,    $6A,    $6A,    $6B,    $6C,    $6C, $6D
 DB $6E, $6E,    $6F,    $70,    $70,    $71,    $72,    $72,    $73,    $74,    $74,    $75,    $76,    $76,    $77, $78
 DB $79, $79,    $7A,    $7B,    $7B,    $7C,    $7D,    $7D,    $7E,    $7F,    $7F,    $80,    $81,    $82,    $82, $83
 DB $84, $84,    $85,    $86,    $87,    $87,    $88,    $89,    $8A,    $8A,    $8B,    $8C,    $8D,    $8D,    $8E, $8F
 DB $90, $90,    $91,    $92,    $93,    $93,    $94,    $95,    $96,    $96,    $97,    $98,    $99,    $99,    $9A, $9B
 DB $9C, $9D,    $9D,    $9E,    $9F,    $A0,    $A0,    $A1,    $A2,    $A3,    $A4,    $A4,    $A5,    $A6,    $A7, $A8
 DB $A9, $A9,    $AA,    $AB,    $AC,    $AD,    $AD,    $AE,    $AF,    $B0,    $B1,    $B2,    $B2,    $B3,    $B4, $B5
 DB $B6, $B7,    $B7,    $B8,    $B9,    $BA,    $BB,    $BC,    $BD,    $BD,    $BE,    $BF,    $C0,    $C1,    $C2, $C3
 DB $C4, $C4,    $C5,    $C6,    $C7,    $C8,    $C9,    $CA,    $CB,    $CB,    $CC,    $CD,    $CE,    $CF,    $D0, $D1
 DB $D2, $D3,    $D4,    $D4,    $D5,    $D6,    $D7,    $D8,    $D9,    $DA,    $DB,    $DC,    $DD,    $DE,    $DF, $E0
 DB $E1, $E1,    $E2,    $E3,    $E4,    $E5,    $E6,    $E7,    $E8,    $E9,    $EA,    $EB,    $EC,    $ED,    $EE, $EF
 DB $F0, $F1,    $F2,    $F3,    $F4,    $F5,    $F6,    $F7,    $F8,    $F9,    $FA,    $FB,    $FC,    $FD,    $FE, $FF








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

ALIGN $100
RX_LO   DEFS 255, 0

ALIGN $100
RX_HI   DEFS 255, 0

ALIGN $100
RY_LO   DEFS 255, 0

ALIGN $100
RY_HI   DEFS 255, 0

ALIGN $100
RZ_LO   DEFS 255, 0

ALIGN $100
RZ_HI   DEFS 255, 0

ALIGN $100
PX_LO   DEFS 255, 0

ALIGN $100
PX_HI   DEFS 255, 0

ALIGN $100
PY_LO   DEFS 255, 0

ALIGN $100
PY_HI   DEFS 255, 0

ALIGN $100
CLIP_CODE      DEFS 255, 0

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

TEMP    DEFS 1024,0
STACK   DEFS 1024,0 ; stack buffer

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

;The 4 x 256 byte lookup tables are in the order of pixel offset within char, x offset from edge, low byte of y and high byte of y.
;l=x ; c =y ; h = lookup
;ld b,(hl) ; pixel value ; 7
;inc h ; 4
;ld a,(hl) ; X offset ; 7
;inc h ; 4
;ld l,c ; Y value ; 4
;or (hl) ; low byte + x offset ; 7
;inc h ; 4
;ld h,(hl) ; high byte ; 7
;ld l,a ; low byte ;4
;ld a,b ; pixel value ;4
;xor (hl) ; mix with screen ; 7
;ld (hl),a ; print ; 7

; The 4 x 256 byte lookup tables are in the order of pixel offset within char, x offset from edge, low byte of y and high byte of y.
; the look up table order is reversed and the routine can be shaved down to 62 Ts like so:
;H = lookup, C =X, L=Y
;ld d,(hl) ;7 ; high byte of y
;dec h ; 4
;ld a,(hl) ; 7 ; low byte of y
;dec h ; 4
;ld l,c ; 4
;add a,(hl) ; 7 ; x offset from edge
;ld e,a ; 4
;dec h ; 4
;ld a,(de) ; 7
;xor (hl) ; 7 ; pixel bit within char
;ld (de),a ;7



;Very fast 8bit * 8bit Unsigned with only 1K of tables
;Input: A = Multiplier, L = Multiplicand

;Output: DE = Product

;CPC Cycles: 104-112 (108 on average) = 26-28 (27) usec

;Size: 24 bytes of code and 1024 bytes for the tables

;Here's a new routine I've developed which uses the formula ab = ((a + b)2 - (a - b)2) / 4. It's based on a routine for the 6502 by Stephen Judd in a C= Hacking article. Because of differences between the way the 6502 does register indexing it was quite difficult to actually get this working, but it's a great compromise between speed and space since it only uses 1K of tables (as opposed to the 16K or 8K table routines above), and can still manage to do the job in a maximum of 28 microseconds.

;Firstly, once again, we need some code to generate the tables. These tables contain values for x2/4 for 9 bit values of x, with the LSB when bit 8 is zero first followed by the MSB.

;.gen_sq4
;        xor a
;        ld de,umul_tab + #1ff
;        ld (de),a
;        dec d
;        ld (de),a
;        ld h,d
;        ld l,e
;        inc e
;        ld c,e
;        ld b,2
;
;        .sq4_lp
;        ld a,b
;        cp 2
;        ld a,e
;        rra
;        add (hl)
;        ld (de),a
;        inc h
;        inc d
;        ld a,(hl)
;        adc c
;        ld (de),a
;        dec d
;        ld h,d
;        inc l
;        inc e
;        jr nz,sq4_lp
;        inc d
;        inc d
;        djnz sq4_lp
;        ret

;align #100
;.umul_tab ds #400

;Now for the actual multiply routine:
;
;        ld h,umul_tab_lo / #100 ; 2
;        ld b,h                  ; 3
;        add l                   ; 4
;        ld c,a                  ; 5
;        jr nc,@noovf            ; 7
;        inc b                   ; 8
;        inc b                   ; 9
;.@noovf
;        sub l                   ; 10
;        sub l                   ; 11
;        jr nc,@noneg            ; 13
;        neg                     ; 15
;.@noneg
;        ld l,a                  ; 16
;        ld a,(bc)               ; 18
;        sub (hl)                ; 20
;        ld e,a                  ; 21
;        inc b                   ; 22
;        inc h                   ; 23
;        ld a,(bc)               ; 25
;        sbc (hl)                ; 27
;        ld d,a                  ; 28
;



;clip

;;bc = start first
;;de = end first
;;ix = start other
;;iy = end other

;;add 24576 to bc ; add 64 to b?
;;add 24576 to de ; add 64 to d?
;;add 24576 to ix ; add 64 to ixh?
;;add 24576 to iy ; add 64 to iyh?

;loop
;               ld      l,c
;               ld      h,b
;               add     hl,de
;               inc     hl
;;              hl = (( bc + de) +1) /2
;        SRL     H   ;1
;        RRC     L         ;l

;               ld              a,h
;               cp              #middle_high
;               jlt             lessthan
;               jgt             greaterthan

;               ld              a,l
;               cp              #middle_low
;               jlt             lessthan
;               jgt             greaterthan

;equal

;;              hl = hl - 16384
                ret

;lessthan
;                       mov     bc,hl
;                       jp      loop
;greaterthan
;                       mov     de,hl
;                       jp      loop

;err               col?
;rxlo b400 98 54 98 54 98 54 98 54
;rxhi b500 fe 01 fe 01 fe 01 fe 01
;rylo b600 97 33 53 b7? 97 33 53 b7
;ryhi b700 ff ff 02 02? ff ff 02 02
;rzlo b800 85 85 85 85 41 41 41 41
;rzhi b900 07 07 07 07 0a 0a 0a 0a
;pxlo ba00 31 8c 31 8c 3e 80 3e 80
;pxhi bb00 00 01 00 01 00 01 00 01
;pylo bc00 73 67 cd 08? 76 6e b9 c3
;pyhi bd00 01 01 01 00? 01 01 01 01
;cc   be00 01 01 01 00 01 01 01 01
;ok                col?
;rxlo b400 98 54 98 54 98 54 98 54
;rxhi b500 fe 01 fe 01 fe 01 fe 01
;rylo b600 a6 42 62 c6? a6 42 62 c6
;ryhi b700 ff ff 02 02? ff ff 02 02
;rzlo b800 85 85 85 85 41 41 41 41
;rzhi b900 07 07 07 07 0a 0a 0a 0a
;pxlo ba00 31 8c 31 8c 3e 80 3e 80
;pxhi bb00 00 01 00 01 00 01 00 01
;pylo bc00 75 79 d0 dc? 79 6f bb c4
;pyhi bd00 01 01 01 01? 01 01 01 01
;cc   be00 01 01 01 01 01 01 01 01

; Diagram of the additions
;                 y1    y0
;              x  x1    x0
;                 --------
;              x0y0h x0y0l
; +      x0y1h x0y1l
; +      x1y0h x1y0l
; +x1y1h x1y1l
; ------------------------

