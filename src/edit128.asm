
;========================================================
; MS-DOS Editor For The Commodore 128
; Scott Hutter
;
; History:
; 1/1/08 - Initial Development
;========================================================

; Basic loader

	* = $1c01                   ; C128 Basic
    .word upstartEnd            ; link address
    .word 10                    ; line num
    .byte $9e                   ; sys
    .text format("%d", $1c0e);
    .byte 0
upstartEnd:
    .word 0                     ; empty link signals the end of the program

; Code start
	
	*= $1c0e

	JMP START

;========================================================
; Data Tables
;========================================================

MENBAR	.TEXT "   File  Edit  Search  View  Options  Help                                      "
	.BYTE 00

STATBAR	.TEXT " F1=Help                                                 Line:001    Col:01     "
	.BYTE 00

TITLE	.TEXT "Untitled1"
	.BYTE 00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00

XLOC	.BYTE 00
YLOC	.BYTE 00

DATAPOS .BYTE 00

LASTCH	.BYTE 00

TXTCOL  .TEXT "01"
	.BYTE 00
TXTLIN  .TEXT "001"
	.BYTE 00

	; Windowing menu coordinates

WINTOPX .BYTE 00
WINTOPY .BYTE 00
WINROWS .BYTE 00
WINCOLS .BYTE 00
WINSTYL .BYTE 00	; 1 = reverse
WINHDLR .WORD $0000	; Address of input handler
			; if $0000, no handler, no waiting


;========================================================
; CONSTANTS
;========================================================

	CLRHM = $93
	CURHM = $13

	RVSON = $12
	RVSOF = $92

	WHITE = $05
	GREY  = $9B
	BLUE  = $1F
	LBLUE = $1F
	BROWN = $95

	HZBAR = $C0
	VTBAR = $DD
	TLCNR = $B0
	TRCNR = $AE
	BLCNR = $AD
	BRCNR = $BD
	SPACE = $20

	UCASE = $8E
	LCASE = $0E

	RIGHT = $1D
	DOWN  = $11
	LEFT  = $9D

	;Zero Page Locations
	SCBOT	= $E4
	SCTOP	= $E5
	SCLF	= $E6
	SCRT	= $E7
	QUOTEMD	= $F4

	;KERNAL ROUTINES
	PLOT   = $FFF0
	CHROUT = $FFD2
	GETIN  = $FFE4

	;BASIC ROUTINES
	CRSRON = $CD6F
	CRSROFF= $CD9F
	SAVEPOS= $CC1E
	RSTRPOS= $C932
	CURMOD = $0A2B
	CLKRATE= $D030
	SCROLY = $D011

	;VDC Registers
	VDCREG1= $D600
	VDCREG2= $D601

;========================================================
; MAIN STARTUP ROUTINE
;========================================================
START	
	JSR INIT
	JSR DRAWMENUBAR
	;JSR DRAWTOPBDR
	;JSR DRAWWIN
	JSR DRAWSCREEN
	JSR DRAWSTATBAR
	JSR DRWTITLE

	;Set location to window
	LDA #$01
	STA XLOC
	LDA #$02
	STA YLOC
	JSR GOTOXY

	;Set the cursor to underline
	LDA #$06
	STA CURMOD

	JSR CRSRON 	; VDC Cursor on
	JSR GOEDITSCR	;Set the window boundaries

	JSR GETTEXT

	RTS

;========================================================
; INIT
;========================================================
INIT
	LDA #$01	; fast mode
	STA CLKRATE

	LDA #$00 ;F2	; black background
	LDX #$1A
	JSR $CDCC
	
	LDA #$30	; Reset TXTLIN to 001
	LDY #$00
	STA TXTLIN, Y
	INY
	STA TXTLIN, Y
	LDA #$31
	INY
	STA TXTLIN, Y

	LDA #$30	; Reset TXTCOL to 01
	LDY #$00
	STA TXTCOL, Y
	LDA #$31
	INY
	STA TXTCOL, Y

	LDA SCROLY	; Blank the 40col screen
	AND #$6F
	STA SCROLY
	
	RTS	

;========================================================
; Draw the menu bar
;========================================================
DRAWMENUBAR
	LDA #CLRHM
	JSR CHROUT

	LDA #LCASE
	JSR CHROUT
	
	LDA #RVSON
	JSR CHROUT

	LDA #GREY
	JSR CHROUT
	
	LDY #$00
LOOP1	LDA MENBAR,Y
	JSR CHROUT
	INY
	CPY #$50	; 80 characters max for the menu bar
	BNE LOOP1

	LDA #RVSOF
	JSR CHROUT

	RTS

;========================================================
; Draw the screen
;========================================================
DRAWSCREEN
	LDA #$00
	STA WINTOPX
	LDA #$01
	STA WINTOPY
	LDA #$4E
	STA WINCOLS
	LDA #$15
	STA WINROWS
	JSR DRAWWINDOW

	RTS

;========================================================
; Draw the top border
;========================================================
DRAWTOPBDR

	LDA #TLCNR	; Top left corner
	JSR CHROUT

	LDY #$00	; Draw horizontal bar
LOOP2	LDA #HZBAR
	JSR CHROUT
	INY
	CPY #$4E
	BNE LOOP2

	LDA #TRCNR	; Draw top right corner
	JSR CHROUT

	RTS


;========================================================
; Draw the status bar
;========================================================
DRAWSTATBAR

	LDA #$00
	STA XLOC
	LDA #$17
	STA YLOC
	JSR GOTOXY

	LDA #RVSON
	JSR $FFD2

	LDY #$00
LP10	LDA STATBAR,Y
	CMP #$00
	BEQ LP9
	JSR CHROUT
	INY
	JMP LP10

LP9	LDA #RVSOF
	JSR CHROUT
	RTS

;========================================================
; Draw the main window background
;========================================================
DRAWWIN			; MAIN BACKGROUND WINDOW DRAWING ROUTINE
	
	LDX #$00

DRWBACK

	LDA #VTBAR	; Draw vertical bar
	JSR CHROUT

	LDA #BROWN	; Blue background
	JSR CHROUT

	LDY #$00	; Draw empty background
LOOP3	LDA #SPACE
	JSR CHROUT
	INY
	CPY #$4E
	BNE LOOP3

	LDA #GREY	; Grey
	JSR CHROUT

	LDA #VTBAR	; Draw vertical bar
	JSR CHROUT

	INX
	CPX #$16
	BNE DRWBACK
	
	RTS

;========================================================
; Draw the file title
;========================================================
DRWTITLE
	LDA #$23
	STA XLOC
	LDA #$01
	STA YLOC
	JSR GOTOXY

	LDA #RVSON
	JSR $FFD2

	LDY #$00
LP7	LDA TITLE,Y
	CMP #$00
	BEQ LP8
	JSR CHROUT
	INY
	JMP LP7

LP8	LDA #RVSOF
	JSR CHROUT
	RTS

;========================================================
; GETTEXT
; MAIN TEXT ENTRY LOOP
;========================================================
GETTEXT	

	JSR GETIN	; Wait for a character
	CMP #$00
	BEQ GETTEXT

	STA LASTCH	; Store it temporarily
	
	LDX #$00
	STX QUOTEMD	; Turn off any quote mode

	; Test for special characters
	
	CMP #$0D	; is CR?
	BEQ GTXT_1
	
	CMP #$14	; is backspace?
	BEQ GETTEXT_BKSP
	
	CMP #$11	; is down arrow?
	BEQ GTXT_1
	
	CMP #$91	; is up arrow?
	BNE GETTEXT_FMENU_CHK
	JSR DECLINCTR
	JMP GTXT_2

GETTEXT_FMENU_CHK

	CMP #$BB	; is C= F character (File menu)
	BNE GETTEXT_EMENU_CHK
	JSR MNU_FILE
	JMP GETTEXT

GETTEXT_EMENU_CHK

	CMP #$B1	; is C= E character (Edit menu)
	BNE GETTEXT_COLCHK
	JSR MNU_EDIT
	JMP GETTEXT

GETTEXT_COLCHK

	SEC		;Get the new XY position
	JSR PLOT
	CPY #$4D	;Have we moved to a new line?
	BEQ GTXT_1
	JSR INCCOLCTR	;No...increase the col counter
	JMP GTXT_2	

GTXT_1	JSR INCLINCTR	;Yes..increase the line counter

	LDA LASTCH
	CMP #$11	;if downarrow, dont print a newline
	BEQ GTXT_2

	JSR NEWLINE

GTXT_2	JSR UPDATELINECOL

	LDA LASTCH
	JSR CHROUT	;Print the char

	JMP GETTEXT	;Do it again

GETTEXT_BKSP
	
	SEC
	JSR PLOT
	TYA
	CMP #$01	; Check if at beginning of a line
	BEQ GETTEXT
	
	LDA LASTCH
	JSR CHROUT

	LDA #$94
	JSR CHROUT

	JMP GETTEXT



;========================================================
;UPDATELINECOL
;========================================================
UPDATELINECOL
	
	JSR SAVEPOS

	JSR GOFULLSCR
	
	LDA #$49	; Move the cursor to 73,22
	STA XLOC
	LDA #$17
	STA YLOC
	JSR GOTOXY

	LDA #RVSON	; Turn rvs on
	JSR $FFD2

	LDA #GREY	; Change background color
	JSR CHROUT

	LDX #$00	; Print the text of col number
UPD_T1	LDA TXTCOL, X
	BEQ UPD_T2
	JSR CHROUT
	INX
	JMP UPD_T1

UPD_T2
	LDA #$3E	; Move the cursor to 62,22
	STA XLOC
	LDA #$17
	STA YLOC
	JSR GOTOXY

	LDX #$00	; Print the text of the line number
UPD_T3	LDA TXTLIN, X
	BEQ UPD_T4
	JSR CHROUT
	INX
	JMP UPD_T3

UPD_T4
	JSR GOEDITSCR

	JSR RSTRPOS

	LDA #RVSOF	; Turn rvs off
	JSR $FFD2
	RTS
	


;========================================================
; GOTOXY - Places cursor at X,Y position
;   SET XLOC and YLOC before calling
;========================================================
GOTOXY
	LDA #CURHM
	JSR CHROUT

	CLC
	LDY XLOC
	LDX YLOC
	JSR $FFF0
	RTS


;=========================================================
;NEWLINE
;=========================================================
NEWLINE
	PHA
	TXA
	PHA

	LDX #$00
	LDA #$30
	STA TXTCOL, X
	INX
	LDA #$31
	STA TXTCOL, X
	
	PLA
	TAX
	PLA

	RTS
	

;=========================================================
;INCCOLCTR
;=========================================================
INCCOLCTR
	CLC
	LDY #$01		; Start at the ones digit (string + 1)
	LDA TXTCOL, Y		; Get the ascii value
	ADC #$01		; add one  to it
	CMP #$3A		; if its a colon... (we have passed '9')
	BEQ INC_COLTENSDIGIT	; jump ahead and handle the tens digit
	STA TXTCOL,Y		; otherwise, store the new value in the ones digit
	RTS			; and return

INC_COLTENSDIGIT
	CLC
	LDA #$30		; Load an ascii character zero '0'
	STA TXTCOL, Y		; store it in the ones digit
	DEY			; move to the tens digit
	LDA TXTCOL, Y		; get the character
	ADC #$01		; add one to it
	STA TXTCOL, Y		; store the new character in the ones digit
	RTS			; exit

	
;=========================================================
;INCLINCTR
;=========================================================
INCLINCTR
	CLC
	LDY #$02		; Start at the ones digit (string + 2)
	LDA TXTLIN, Y		; Get the ascii value
	ADC #$01		; add one  to it
	CMP #$3A		; if its a colon... (we have passed '9')
	BEQ INC_LINTENSDIGIT	; jump ahead and handle the tens digit
	STA TXTLIN,Y		; otherwise, store the new value in the ones digit
	RTS			; and return

INC_LINTENSDIGIT
	CLC
	LDA #$30		; Load an ascii character zero '0'
	STA TXTLIN, Y		; store it in the ones digit
	DEY			; move to the tens digit
	LDA TXTLIN, Y		; get the character
	ADC #$01		; add one to it
	CMP #$3A		; if its a colon
	BEQ INC_LINHUNDDIGIT	; jump ahead and handle the hundreds digit
	STA TXTLIN, Y		; store the new character in the ones digit
	RTS			; exit

INC_LINHUNDDIGIT
	CLC
	LDA #$30		; Load an ascii character zero '0'
	STA TXTLIN, Y		; store it in the tens digit
	DEY			; move to the hundreds digit
	LDA TXTLIN, Y		; get the character
	ADC #$00		; add one to it
	STA TXTLIN, Y		; store the new character in the ones digit
	RTS			; exit

;=========================================================
;DECLINCTR
;=========================================================
DECLINCTR
	SEC
	LDY #$02		; Start at the ones digit (string + 2)
	LDA TXTLIN, Y		; Get the ascii value
	SBC #$01		; subtract one  from it
	CMP #$2F		; if its a slash... (we have passed '0')
	BEQ DEC_LINTENSDIGIT	; jump ahead and handle the tens digit
	STA TXTLIN,Y		; otherwise, store the new value in the ones digit
	RTS			; and return

DEC_LINTENSDIGIT
	SEC
	LDA #$39		; Load an ascii character nine '9'
	STA TXTLIN, Y		; store it in the ones digit
	DEY			; move to the tens digit
	LDA TXTLIN, Y		; get the character
	SBC #$01		; subtract one  from it
	CMP #$2F		; if its a slash... (we have passed '0')
	BEQ DEC_LINHUNDDIGIT	; jump ahead and handle the hundreds digit
	STA TXTLIN, Y		; store the new character in the ones digit
	RTS			; exit

DEC_LINHUNDDIGIT
	SEC
	LDA #$39		; Load an ascii character nine '9'
	STA TXTLIN, Y		; store it in the tens digit
	DEY			; move to the hundreds digit
	LDA TXTLIN, Y		; get the character
	SBC #$01		; add one to it
	STA TXTLIN, Y		; store the new character in the ones digit
	RTS			; exit


RAMTEST1
	LDA #$41
	LDX #$01
	LDY #$00
	JSR VDCPOKE

	
	LDA #$01
	LDX #$00
	JSR VDCPEEK

	LDX #$02
	LDY #$00
	JSR VDCPOKE

	RTS

VDCSCRL
	LDA #$F1
	LDX #$00
	JSR VDCPEEK

	LDX #$A1
	LDY #$00
	JSR VDCPOKE

	LDA #$41
	LDX #$01
	JSR VDCPEEK

	LDX #$F1
	LDY #$00
	JSR VDCPOKE

	RTS


;=========================================================
;VDCPOKE
; Write (POKE) to the VDC RAM directly via VDC registers
; IN: A = Byte to poke
;     X = LO Byte of address to poke to
;     Y = HI Byte of address to poke to
;=========================================================
VDCPOKE PHA
        LDA #$13        ; Decimal 19, low byte register
        STA VDCREG1
VPOK_1  BIT VDCREG1
        BPL VPOK_1
        STX VDCREG2	; Set low byte
        LDA #$12        ; Decimal 18, high byte register
        STA VDCREG1
VPOK_2  BIT VDCREG1
        BPL VPOK_2
        STY VDCREG2	; Set high byte
        LDA #$1F        ; Decimal 31, VDC RAM data register
        STA VDCREG1
VPOK_3  BIT VDCREG1
        BPL VPOK_3
        PLA             ; Recall stashed .A
        STA VDCREG2	; And tell VDC to write it
        RTS 


;=========================================================
;VDCPEEK
; Reads (PEEK) value from VDC RAM directly via VDC registers
; IN: A = LO Byte of address to peek from
;     X = HI Byte of address to peek from
; OUT: A = Byte read
;=========================================================
VDCPEEK
	LDY #$13        ; Decimal 19, low byte register
        STY VDCREG1
VPEK_1  BIT VDCREG1
        BPL VPEK_1
        STA VDCREG2	; Set low byte
        LDA #$12        ; Decimal 18, high byte register
        STA VDCREG1
VPEK_2  BIT VDCREG1
        BPL VPEK_2
        STX VDCREG2	; Set high byte
        LDA #$1F        ; Decimal 31, VDC RAM data register
        STA VDCREG1
VPEK_3  BIT VDCREG1
        BPL VPEK_3
        LDA VDCREG2	; And tell VDC to read the RAM
        RTS 

;=========================================================
;GOFULLSCR
;=========================================================
GOFULLSCR

	LDA #$00	; Top
	STA SCTOP

	LDA #$4F	; Right
	STA SCRT	

	LDA #$00	; Left
	STA SCLF

	LDA #$18	; bOTTOM
	STA SCBOT

	RTS

;=========================================================
;GOEDITSCR
;=========================================================
GOEDITSCR

	LDA #$02	; Top
	STA SCTOP

	LDA #$4E	; Right
	STA SCRT	

	LDA #$01	; Left
	STA SCLF

	LDA #$16	; bOTTOM
	STA SCBOT

	RTS

;=========================================================
;MNU_FILE
;=========================================================
MNU_FILE
	LDA #$02
	STA WINTOPX
	LDA #$01
	STA WINTOPY
	LDA #$0F
	STA WINCOLS
	LDA #$0A
	STA WINROWS
	LDA #$01	; 1 = reverse mode
	STA WINSTYL
	JSR DRAWWINDOW

	RTS

;=========================================================
;MNU_EDIT
;=========================================================
MNU_EDIT
	LDA #$09
	STA WINTOPX
	LDA #$01
	STA WINTOPY
	LDA #$0F
	STA WINCOLS
	LDA #$07
	STA WINROWS
	LDA #$01	; 1 = reverse mode
	STA WINSTYL
	JSR DRAWWINDOW

	RTS

;=========================================================
;DRAWWINDOW
;=========================================================

DRAWWINDOW

	JSR SAVEPOS	; Save our current position
	JSR GOFULLSCR	; Clear any defined windows
	JSR CRSROFF

	; If a window handler routine is not defined
	; Do not store the current screen

	LDA WINHDLR
	CMP #$00
	BNE DRWSAV
	LDA WINHDLR+1
	CMP #$00
	BNE DRWSAV
	JMP DRWHOME

DRWSAV	LDA #$00
	LDX #$00
	JSR KEEP	; Save the screen
	
DRWHOME	LDA WINTOPX	; Position cursor at top x/y of window
	STA XLOC
	LDA WINTOPY
	STA YLOC
	JSR GOTOXY

	LDA WINSTYL
	CMP #$01
	BNE DRWL0

	LDA #RVSON
	JSR CHROUT

	; Draw top window border

DRWL0	LDA #TLCNR	; Top left corner
	JSR CHROUT

	LDY #$00	; Draw horizontal bar
DRWL1	LDA #HZBAR
	JSR CHROUT
	INY
	CPY WINCOLS
	BNE DRWL1

	LDA #TRCNR	; Draw top right corner
	JSR CHROUT

DRWWINDOW_NEXTLINE

	; Next line
	INC WINTOPY
	
	; Is at window bottom?
	LDA WINTOPY
	CMP WINROWS
	BEQ DRWL3

	LDA WINTOPX	; Position cursor at top x/y of window
	STA XLOC
	LDA WINTOPY
	STA YLOC
	JSR GOTOXY

	; Draw rows
	
	LDA #VTBAR	; Vertical bar
	JSR CHROUT

	LDY #$00	; Fill with spaces
DRWL2	LDA #SPACE
	JSR CHROUT
	INY
	CPY WINCOLS
	BNE DRWL2
	
	LDA #VTBAR	; Vertical bar
	JSR CHROUT

	JMP DRWWINDOW_NEXTLINE

DRWL3	; Draw bottom border

	LDA WINTOPX	; Position cursor at top x/y of window
	STA XLOC
	LDA WINTOPY
	STA YLOC
	JSR GOTOXY

	LDA #BLCNR
	JSR CHROUT

	LDY #$00	; Draw horizontal bar
DRWL4	LDA #HZBAR
	JSR CHROUT
	INY
	CPY WINCOLS
	BNE DRWL4

	LDA #BRCNR
	JSR CHROUT

	LDA WINSTYL
	CMP #$01
	BNE DRAWWINDOW_WAITCHAR

	LDA #RVSOF
	JSR CHROUT

DRAWWINDOW_WAITCHAR
rts
	
	; If a window handler routine is defined, we call it now

	LDA WINHDLR
	CMP #$00
	BNE DRWRSTR
	LDA WINHDLR+1
	CMP #$00
	BNE DRWRSTR

	; Store the return address in the handler pointers
	;...we need to store the return address...
	JMP (WINHDLR)

	;JSR GETIN	; Wait for a character
	;CMP #$00
	;BEQ DRAWWINDOW_WAITCHAR

DRWRSTR	LDA #$00
	LDX #$01
	JSR KEEP	; Restore screen

	JSR GOEDITSCR
	JSR RSTRPOS
	

	JSR CRSRON

	RTS
	


;=========================================================
; Keep-80 - Transactor Magazine
; Feb 1989, Volume 9, Issue 3
;
; To save the current 80 col screen: sys 4864,0,0
; To restore the 80 col screen     : sys 4864,0,1
;=========================================================


	; rom routines
	wrvdc = $cdca
	rdvdc = $cdd8
	vcopy = $c53c

	; ram locations
	svars = $0030	; start of screen variables
	smaps = $0354	; start of tab and link maps
	pnt80 = $0a3c	; end pointer for vcopy
	ztemp = $c3	; safe temporary location

keep	bit $d7		; test 80 columns
	bmi ok80
err	sec
	rts

spage	.byte $10	; start page of unused area
epage	.byte $1f	; end page

ok80	cmp #$03
	bcs err

	stx ztemp	; direction
	tay
	bne edda
	txa
	bne rscrn

	; save whole screen

	jsr rend	; write editor values to $07d0
	
	lda #$d0	; destination end+1
	ldx epage
	sta pnt80
	stx pnt80+1
	lda spage	; dest. start
	ldy #$00
	jsr addwr
	lda #$00	; source start = $0000
	tay
setsrce	ldx #$20
	jsr addwr+2
setcopy	ldx #$18
	jsr rdvdc+2
	ora #$80	; 7=1=copy
	jsr wrvdc+2
	jsr vcopy	; call rom routine
	clc		; no errors
	rts

	; recall whole screen

rscrn	lda #$d0	; copy everything
	ldx #$0f	; back to $0000-$0fcf
	sta pnt80
	stx pnt80+1
	lda #$00
	tay
	jsr addwr
	lda spage	; source is unused area
	ldy #$00
	jsr setsrce

rend	lda #$07	; hi-byte of editor storage
	bne edsa

edda	lda #$0f	; store / recall editor values
	clc		; at $0fd0 or $1fd0
	dey
	beq edsa
	adc #$10
edsa	ldy #$d0	; lo-byte
	

	; store / recall screen editor values

editsr	jsr addwr
	ldy #$1a
	lda ztemp	; 0=store
	beq kploop3

kploop1	jsr rdvdc
	sta svars, y
	dey
	bpl kploop1
	ldy #$0d
kploop2	jsr rdvdc
	sta smaps,y
	dey
	bpl kploop2
	rts

kploop3	lda svars,y
	jsr wrvdc
	dey
	bpl kploop3
	ldy #$0d
kploop4	lda smaps,y
	jsr wrvdc
	dey
	bpl kploop4
	rts

; routine to write vdc to address registers ($12/$13)
; or any other pair of registers
; a=first byte, y=next byte, x=first register

addwr	ldx #$12
	jsr wrvdc+2	; here for other pairs
	tya
	inx
	jmp wrvdc+2

