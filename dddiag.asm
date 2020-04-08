;
; Disassembily of Dragon Data Quick diagnostic cart.
;
; Original Copyright (C) 1982 Dragon Data Ltd.
; 

		use 	ascii.asm
		use	cpudefs.asm
		use	dgndefs.asm
		use	romdefs.asm
		use	basictokens.asm
		use 	basicdefs.asm
		use	samdefs.asm
		use	wddefs.asm

		org     CartBase


Start		LDS     #$8000		; Setup stack pointer at top of RAM
		LDX     HWVecReset	; get reset vector
		PSHS    X		; save on stack?
		LBSR    CLearScr	; clear the screen.

		LDX     #$0422
		LEAY    MessMainMenu,PCR ; point at menu
		LBSR    PrintStrY	; display it on screen

LC016	        JSR     BasicKbdIn	; read keyboard	

		TSTA			; key pressed?
		BEQ     LC016		; nope, keep waiting

;
; The menyu table consists of 3 byte entries, the first of which is the character
; of the menu entry, the second two are the word address of the handler routine.
;

		LEAX    MenuTable,PCR	; point at menu table
LC020	        CMPA    ,X+		; same as typed character?
		BNE     LC026		; nope, do next entry

		JMP     [,X]		; jump to handler

LC026	        TST     ,X++		; Last entry ?
		BNE     LC020		; nope check next
		BEQ     LC016		; yes invalid keystroke, read keyboard again

; Cassette check

CheckCas   	LBSR    CLearScr	; clear the screen
		JSR     BasicCassOn	; turn on the tape

		LDX     #$0484		; location on screen
		LEAY    MessRecord,PCR	; prompt user to turn on tape
		LBSR    PrintStrY	; print it

LC03C		JSR     BasicKbdIn	; read keyboard
		CMPA    #$0D		; enter pressed?
		BNE     LC03C		; no, keep waiting
		
		JSR     BasicWriteLead	; write a leader to the tape

		LDY     #$0400		; init counter (on top of screen!)
LC04A        	STY     TextScreenBase	

		LDA     #$3C		; write a $3C byte to tape
		JSR     BasicCassByOut

		LDA     #$E7		; write a $E7 byte to tape
		JSR     BasicCassByOut

		LDY     TextScreenBase	; recover counter
		LEAY    -1,Y		; decrement	
		BNE     LC04A		; keep going if not zero

		LDX     #$04E4		; screen location
		LEAY    MessPlay,PCR	; prompt user to rewind and press play
		LBSR    PrintStrY	; print it

		LDX     #$0544		; screen pos
		LEAY    MessSearch,PCR	; tell user we are searching
		LBSR    PrintStrY	; print it

		JSR     BasicCassOnRd	; turn tape on for reading

LC077		JSR     BasicCassBitIn	; read a bit from tape into carry
		ROLA			; rotate it into A
		CMPA    #$3C		; synced up yet?
		BNE     LC077		; nope loop again

		LEAY    MessFound,PCR	; display found message
		LDX     #$0550		; screen pos
		LBSR    PrintStrY	; print it

		LDY     #$0030		; count of bits to read before giving up
		JSR     BasicCassOnRd	; tape on for read

LC090		JSR     BasicCassBitIn	; read a bit from tape into carry
		ROLA			; rotate it into A
		CMPA    #$3C		; synced up yet?
		BEQ     LC0A2		; yep 

		LEAY    -1,Y		; decrement bit count
		BNE     LC090		; loop again if not zero

		LEAY    MessIOErr,PCR	; IO error message
		BRA     LC0A6		; skip ahead

LC0A2		LEAY    MessOK,PCR	; Tape OK message
LC0A6        	LDX     #$0556		; screen position
		LBSR    PrintStrY	; print it

DelayReset	LDX     #$0009		; small delay loop
DelayXReset	LEAY    -1,Y		; decrement y
		BNE     DelayXReset	; loop if not zero

		LEAX    -1,X		; decrement X
		BNE     DelayXReset	; loop if not zero

		JMP     [HWVecReset]	; reset the machine

; calculate CRC for ROMS.

CheckROM	LBSR    CLearScr	; Clear screen

		LDX     #$8000		; point at first ROM
		BSR     CalcSum		; checksum it

		LDX     #$0543		; screen pos
		LEAY    MessCRC,PCR	; point at message
		LBSR    PrintStrY	; print it	

		LDD     #$787A		; display '8 : '
		STA     ,X++		; store 8 and skip a char
		STB     ,X++		; display ':' and skip a char
		BSR     HexWord		; output the hex sum

		LDX     #$A000		; point at second ROM
		BSR     CalcSum		; calculate it's checksum

		LDX     #$04E3		; screen pos
		LEAY    MessCRC,PCR	; point at message
		LBSR    PrintStrY	; print it

		LDD     #$777A		; display '7 : '
		STA     ,X++		; store 8 and skip a char
		STB     ,X++		; display ':' and skip a char
		BSR     HexWord		; output the hex sum

		BRA     DelayReset	; delay for a while, then reset

CalcSum		LDY     #$2000		; Size of ROM, 8K
		LDD     #$0000		; init sum
		ANDCC   #~FlagCarry	; clear carry
LC0F9		ADCB    ,X+		; add a byte to the total
		ADCA    #$00		; propagate carry
		LEAY    -1,Y		; decrement count
		BNE     LC0F9		; loop till all done

		TFR     D,U		; copy D to U
		RTS

HexWord		PSHS    U		; push checksum onto stack
		PULS    A		; pull MSB
		LBSR    HexByte

		PULS    A		; pull LSB
		LBSR    HexByte

		RTS


CheckVid	CLRA			; clear a
		LDX     #TextScreenBase	; point at screen

LC115        	CLR     256,X		; clear byte on bottom half of screen
		STA     ,X+		; store character on top half
		INCA			; increment A
		BNE     LC115		; loop for all 256 chars

; draw the 'dragon' outline

		LEAX    DragonTable,PCR	; point at table
LC122		LDY     ,X++		; get an address from table
		BEQ     LC12B		; if end of table exit

		COM     ,Y		; invert the char at address
		BRA     LC122		; loop for next

; A is 0 at this point.....

LC12B		LDX     #$0001		; short delay (before reset)
LC12E	        STA     PIA1DB		; set VDG mode bits
		INCA			; increment A
		LBEQ    DelayXReset	; if we've done all 256, then reset

		LDY     #$3000		; init inter mode delay
LC13A		LEAY    -1,Y		; decrement count
		BNE     LC13A		; loop until zero

		BEQ     LC12E		; do next mode

; check the sound
; first enable output from MUX by setting bit CB2 high
; first character on screen is used as a 'frequency' count byte :)

CheckSnd	LDB     PIA1CRB		; get control register
		ORB     #$08		; set bit CB2 high
		STB     PIA1CRB		; update
		
		LDY     #$08E0		; init counter
LC14C		CLRB			; clear b
LC14D		TFR     B,A		; A=B
		ANDA    #$FC		; mask off bottom bits
		STA     PIA1DA		; send A to DAC
		INCB			; increment b
		CMPB    TextScreenBase	; reached top
		BLO     LC14D		; nope loop again

		INC     TextScreenBase	; increment frequency byte, lowers tone
		LEAY    -1,Y		; decrement counter	
		BNE     LC14C		; loop if more

		RTS

; check joysticks

CheckJoy	STA     SAMSV1		; set SAM mode, SG8
		LDA     #$90		; clear screen
		LDX     #TextScreenBase	; point at screen base
LC16A	        STA     ,X+		; store a byte
		CMPX    #$0C00		; end of screen?
		BNE     LC16A		; no loop again

; draw screen edges

		LDX     #$003E		; screen width
LC174		LDY     #$0000		; at top line
		LBSR    PlotXY		; plot a pixel

		LDY     #$003F		; at bottom line
		LBSR    PlotXY		; plot a pixel

		LEAX    -1,X		; decrement x co-ordinate
		BNE     LC174		; loop if not rached LHS of screen

		LDY     #$003E		; bottom of screen
LC18A		LDX     #$0000		; LHS of screen
		LBSR    PlotXY		; plot a pixel

		LDX     #$003F		; RHS of screen
		LBSR    PlotXY		; plot a pixel

		LEAY    -1,Y		; decrement y co-ordinate
		BNE     LC18A		; loop until it reaches top of screen

LC19A		JSR     BasicJoyIn	; read joysticks

		CLRA			; clear MSB
		LDB     BasJoyVal0	; get joy val 0 into LSB R-X
		TFR     D,X		; copy 16 bit value to X
		LDB     BasJoyVal1	; get joy val 1 into LSB R-Y
		TFR     D,Y		; copy 16 bit value to Y
		LBSR    PlotXY

		CLRA			; clear MSB
		LDB     BasJoyVal2	; get joy val 2 into LSB L-X
		TFR     D,X		; copy 16 bit value to X
		LDB     BasJoyVal3	; get joy val 3 into LSB L-Y
		TFR     D,Y		; copy 16 bit value to Y 
		LBSR    PlotXY

		LDA     #$FF		; all pullups on for kboard / JS buttons
		STA     PIA0DB		; output them
		LDB     PIA0DA		; read keyboard 
		TFR     B,A		; save in A
		ANDA    #$03		; and out buttons
		CMPA    #$03		; check buttons not pressed
		BEQ     LC19A		; if so loop and keep reading sticks.

		LDY     #$03E8		; short delay
LC1CD		LEAY    -1,Y
		BNE     LC1CD

		CMPB    PIA0DA		; debounce.....
		BNE     LC19A		; not same, keep reading sticks

		JMP     [HWVecReset]	; reset machine

; check the keyboard

		ifdef Tandy
BreakKey	equ	$72		; Break key, CoCo
MaxBlock	equ	$5A		; max value of big block
KeyOffset	equ	'@'		; offset to add to keys 
		else
BreakKey	equ	$62		; Break key, Dragon
MaxBlock	equ	$5B		; max value of big block
KeyOffset	equ	'0'		; offset to add to keys 
		endc

CheckKbd	CLR     PIA0DB		; set all keyboard lines low
LC1DD		LDB     PIA0DA		; read keyboard inputs
		ORB     #$80		; ignore bit 7 as not keyboard
		COMB			; complement it
		BEQ     LC1DD		; no keys down loop again

; scan keyrows to see which is down...

		CLR     TextPrnSelFlag	; uses this as a counter I think !
		LDB     #$FE		; column 0
		STB     PIA0DB		; strobe column low

LC1ED		LDB     PIA0DA		; read rows
		ORB     #$80		; mask out bit 7
		CMPB    #$FF		; any keys pressed?
		BNE     LC201		; yep, deal with them

		INC     TextPrnSelFlag	; increment col count
		COMB			; zero b
		ROL     PIA0DB		; move to next column
		BLO     LC1ED		; loop to read next column

		BRA     CheckKbd	; start scan again	

LC201		LDY     #$0200		; short delay for debounce
LC205		LEAY    -1,Y
		BNE     LC205

		TFR     B,A		; save previous input value
		SUBA    PIA0DA		; subtract current value
		ANDA    #$7F		; mask out comparitor input
		BNE     CheckKbd	; not same loop	

; At this point B contains the keyrow, TextPrnSelFlag to column
; loop adding 8 for each row of 8 keys and shifting the row counter
; until the 0 for the key which is down is shifted into the carry flag
; then add constant and adjust for ascii

		LDA     TextPrnSelFlag	; get column counter
LC215		ADDA    #$08		; add a row of keys	
		LSRB			; b=b/2, b0->carry
		BLO     LC215		; keep looping until key down -> carry

		ADDA    #KeyOffset-8	; convert to ascii
		CMPA    #MaxBlock	; bigger than '['

		BLO     LC232		; no, it's correct display it

		CMPA    #BreakKey	; break ?
		LBEQ    Start		; yep exit, and redisplay menu

; For tandy adjust for numeric rows
		ifdef Tandy
		cmpa	#$60		; this is '0'
		blo	LC232		; if lower, print it
		cmpa	#$6F		; this is '9'
		bhi     LC232		; if higher, print it
		suba	#$60-'0'	; make it ascii
		bra	LC232		; print it
		else
		BHI     LC232		; if higher, print it	
		endc


; if not already ASCII, and between 'Z' and 'break', look up in table

		TFR     A,B		; get keycode into B	
		SUBB    #$5B		; make it zero based
		LEAX    KeyTableD,PCR	; look it up reom table
		LDA     B,X

LC232		LBSR    CLearScrToA	; display character
		BRA     CheckKbd	; loop and check again

	

CheckRAM	LDX     #$8000		; start at top of RAM
		LDA     #$1D		; select video mode SG6
		STA     PIA1DB

LC23F		CLR     ,-X		; clear a byte
		LEAX    ,X		; set flags
		BNE     LC23F		; keep going until zero

		CLRB			; clear regs
		CLRA

; loop over RAM, checking and setting bytes. 
; in this code A contains the next value to write, B contains
; the last written, the value to check.		
		
LC247        	INCA			; increment value to set
		BEQ     LC25B		; branch if end

		LDX     #$0000		; start at beginning of RAM
LC24D		CMPB    ,X		; check previously written value
		BNE     LC26B		; branch if not same

		STA     ,X+		; store new value, increment pointer
		CMPX    #$4000		; reached end of first 16K?
		BNE     LC24D		; no loop again

		INCB			; increment value to check
		BRA     LC247		; loop for next

LC25B		LDX     #$04E3		; screen co-ordinates
		LDS     #$03FE		; move stack to lower 16K
		LEAY    MessLower,PCR	; print lower message
		LBSR    PrintStrY	

		BRA     LC274		; skip over error print

; bad compare, byte has failed!
LC26B		EORB    ,X		; find out which bist are different	
		TFR     X,U		; save pointer to bad byte
		LDX     #$04E3		; screen co-ordinates
		BSR     LC2BA		; print bad location

LC274		LDA     #$05		; back to text mode
		STA     PIA1DB
		LDU     #$0000		; zero U, A,B
		TFR     U,D

LC27E		INCA			; init ram check loop as above
		PSHS    B,A		; save values
		LDX     #TextScreenBase	; top of screen
		LBSR    HexWord		; print D as hex

		LEAU    1,U		; u=u+1
		LDX     #$4000		; base of second 16K
		PULS    B,A		; restore test values

LC28E		CMPB    ,X		; check previous value wrote ok
		BNE     LC2AE		; nope, RAM error

		STA     ,X+		; save new value, increment pointer
		CMPX    #$8000		; top of RAM?
		BNE     LC28E		; nope keep going

		INCB			; increment old value
		BNE     LC27E		; not done all, loop again

		LDX     #$0543		; screen co-ordinates
		LEAY    MessUpper,PCR	; point to message
		BSR     PrintStrY	; print it

		LEAY    Mess16K,PCR	; point to message
		BSR     PrintStrY	; print it

		LBRA    DelayReset	; delay and reset

LC2AE		EORB    ,X		; test which bits failed
		TFR     X,U		; save fail address
		LDX     #$0543		; screen positon
		BSR     LC2BA		; display error
		
		LBRA    DelayReset	; delay and reset

LC2BA		LEAY    MessRAMError,PCR ; point to message
		BSR     PrintStrY	; print it

		LBSR    HexWord		; print address of error
		BSR     PrintStrY	; print 'at'

; work out bit number (to report IC no).
		LDA     #$FF		; start at -1, as we pre-inc

LC2C7		INCA			; increment bit no
		LSLB			; shift difference
		BHS     LC2C7		; keep going until we hit bad bit

		CMPA    #$01		; get chip number
		BEQ     LC2D3
		BMI     LC2D7
		BRA     HexByte		; print it

LC2D3		LDA     #$09
		BRA     HexByte		; print it

LC2D7		LDA     #$10		; print it

;
; Print the Hex byte in A, X=screen location to output to.
;

HexByte		STA     ,-S		; save byte
		LSRA			; shift MS 4 bits into bottom
		LSRA
		LSRA
		LSRA
		BSR     LC2EC		; convert to ASCII

		STA     ,X+		; save on screen
		LDA     ,S+		; recover byte from stack
		ANDA    #$0F		; mask out top bits
		BSR     LC2EC		; convert to ASCII

		STA     ,X+		; save on screen
		RTS
		
LC2EC		ADDA    #'0'		; Add code of '0'
		CMPA    #'9'		; greater than 9?
		BLS     LC2F4		; no skip on

		ADDA    #$07		; ajust
LC2F4		ORA     #$40		; convert to VDG
		RTS

;
; Clear text screen
;		
		
CLearScr	LDA     #$60		; VDG space
CLearScrToA	LDU     #$0600		; start at end of screen
		ORA     #$40		; convert char to VDG
LC2FE		STA     ,-U		; save it on screen
		CMPU    #TextScreenBase	; reached beginning of screen?
		BNE     LC2FE		; nope, loop again

		RTS

;
; Display a zero terminated string pointed to by Y
;

PrintStrY	LDA     ,Y+		; get a byte from string
		CMPA    #$00		; end of string ?
		BNE     LC30E		; no process character

		RTS			; yes return
		
LC30E		CMPA    #$0D		; Is it a return
		BNE     LC320		; no put it on the screen

		TFR     X,D		; get screen address in D
		ADDD    #$0020		; add a line (32 characters)
		ANDB    #$E0		; mask out character pos
		ADDD    #$0002		; add2 ????
		TFR     D,X		; move back to D
		BRA     PrintStrY	; continue

LC320		ORA     #$40		; Convert to correct char for VDG	
		STA     ,X+		; store it
		BRA     PrintStrY	; loop again for next char

;
; Check printer
;
CheckPrn	JSR     LC329		; do it twice, onse as jsr then again as
					; we fall through.......

LC329		LDA     #$20		; start at space
LC32B		JSR     BasicPrintOut	; sent it to printer

		INCA			; increment character
		BPL     LC32B		; loop if <128, and send to printer

		LDA     #$0D		; print a newline
		JSR     BasicPrintOut	; twice!
		JMP     BasicPrintOut


; entered with :
; 	X = Joystick X value
;	Y = Joystick Y value

PlotXY		PSHS    X		; save X value
		TFR     Y,D		; put Y value in D
		LDX     #$0006		; 6 shifts, multiply by 64 
LC340		LSLB			; mult by 2
		ROLA
		LEAX    -1,X		; dec counter
		BNE     LC340		; keep going until counter zero

		ADDD    ,S		; add X value
		LSRA			; divide by 2 as 2 pixels / byte
		RORB
		TFR     D,U		; get offset into U
		LDA     #$05		; SG8 (SG$) 2 left hand pixels on 
		BLO     LC351		; skip if LH set

		LSLA			; rotate them into right hand pixels
LC351		ORA     TextScreenBase,U	; com bine with screen
		STA     TextScreenBase,U
		PULS    PC,X         	; Pull of PC, effective RTS
		
MessMainMenu
        FCC    "  QUICK DIAGNOSTIC ROM"
        FCB    $0D
        FCC    "  (C) DRAGON DATA 1982"
        FCB    $0D,$0D
        FCC    "B - BASIC ROM BYTE CHECK"
        FCB    $0D
        FCC    "C - CASSETTE RECORDER CHECK"
        FCB    $0D
        FCC    "J - JOYSTICK INPUT CHECK"
        FCB    $0D
        FCC    "K - KEYBOARD FUNCTIN CHECK"
        FCB    $0D
        FCC    "M - 16K+16K RAM READ/WRITE"
        FCB    $0D
        FCC    "P - PRINTER OUTPUT CHECK"
        FCB    $0D
        FCC    "S - SOUND OUTPUT CHECK"
        FCB    $0D
        FCC    "V - VIDEO DISPLAY CHECK"
        FCB    $00

MessPlay	
	FCN    "REWIND AND SET FOR PLAY"
MessRecord	
	FCC    "REWIND AND SET FOR RECORD"
        FCB    $0D
        FCC    "PRESS ENTER AFTER TAPE LEADER"
        FCB    $0D,$0D,$00

MessSearch
	FCN    "SEARCHING  "

MessIOErr   
	FCN    "IO ERROR"

MessFound   
	FCN    "FOUND"

MessCRC	FCN    "CRC CODE FOR IC1"

MessRAMError	
	FCN    "RAM ERROR AT "
        FCN    " IN IC"

MessUpper	
	FCN    " UPPER"

MessLower   
	FCC    " LOWER"

Mess16K FCC    " 16K RAM"

MessOK  FCN    " O.K. "

MenuTable
        FCB    'B'			; basic rom check
        FDB    CheckROM

        FCB    'K'			; keyboard check
        FDB    CheckKbd

        FCB    'J'			; joystick check
        FDB    CheckJoy

        FCB    'C'			; cassette check
        FDB    CheckCas

        FCB    'M'			; RAM check
        FDB    CheckRAM

        FCB    'S'			; sound check
        FDB    CheckSnd

	FCB    'V'			; check video
        FDB    CheckVid

        FCB    'P'
        FDB    CheckPrn

        FCB    $00
        FDB    $0000

        FCB    $00

; Table for keys, to display in key test when key pressed.

KeyTableD
        FCB    	$5E			; up arrow
	FCB	$5C			; down arrow
	FCB	$5F			; left arrow
	FCB	$2F			; right arrow
	FCB	$20			; space
	FCB	$23			; enter
	FCB	$21			; clear

DragonTable
        FDB    $0545,$0546,$0548,$0549
        FDB    $054A,$054B,$0566,$0568
        FDB    $0586,$0587,$0588,$0589
        FDB    $058A,$058B,$05A8,$05AB

        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00

	zmb	($d000-*)
