		processor 6502
;
; 		HARDWARE ADDRESS EQUATES
;
INPTCTRL        equ     $01     	;Input control
AUDC0           equ     $15     	;Audio Control Channel 0
AUDC1           equ     $16     	;Audio Control Channel 1
AUDF0           equ     $17     	;Audio Frequency Channel 0
AUDF1           equ     $18     	;Audio Frequency Channel 1
AUDV0           equ     $19     	;Audio Volume Channel 0
AUDV1           equ     $1A     	;Audio Volume Channel 1
INPT0           equ     $08			;Paddle Control Input 0
INPT1           equ     $09     	;Paddle Control Input 1
INPT2           equ     $0A     	;Paddle Control Input 2
INPT3           equ     $0B     	;Paddle Control Input 3
INPT4           equ     $0C     	;Player 0 Fire Button Input
INPT5           equ     $0D     	;Player 1 Fire Button Input
;
;		MARIA EQUATES
;
BACKGRND        equ     $20     	;Background Color
P0C1            equ     $21     	;Palette 0 - Color 1
P0C2            equ     $22     	;Palette 0 - Color 2
P0C3            equ     $23     	;Palette 0 - Color 3
WSYNC           equ     $20     	;Wait For Sync
P1C1            equ     $21     	;Palette 1 - Color 1
P1C2            equ     $22     	;Palette 1 - Color 2
P1C3            equ     $23     	;Palette 1 - Color 3
MSTAT           equ     $28     	;Maria Status
P2C1            equ     $29     	;Palette 2 - Color 1
P2C2            equ     $2A     	;Palette 2 - Color 2
P2C3            equ     $2B     	;Palette 2 - Color 3
DPPH            equ     $2C     	;Display List List Pointer High
P3C1            equ     $2D     	;Palette 3 - Color 1
P3C2            equ     $2E     	;Palette 3 - Color 2
P3C3            equ     $2F     	;Palette 3 - Color 3
DPPL            equ     $30     	;Display List List Pointer Low
P4C1            equ     $31     	;Palette 4 - Color 1
P4C2            equ     $32     	;Palette 4 - Color 2
P4C3            equ     $33     	;Palette 4 - Color 3
CHARBASE        equ     $34     	;Character Base Address
P5C1            equ     $35     	;Palette 5 - Color 1
P5C2            equ     $36     	;Palette 5 - Color 2
P5C3            equ     $37     	;Palette 5 - Color 3
OFFSET          equ     $38     	;Unused - Store zero here
P6C1            equ     $39     	;Palette 6 - Color 1
P6C2            equ     $3A     	;Palette 6 - Color 2
P6C3            equ     $3B     	;Palette 6 - Color 3
CTRL            equ     $3C     	;Maria Control Register
P7C1            equ     $3D     	;Palette 7 - Color 1
P7C2            equ     $3E     	;Palette 7 - Color 2
P7C3            equ     $3F     	;Palette 7 - Color 3
;
;		CONSOLE EQUATES
;
SWCHA           equ     $280    	;P0, P1 Joystick Directional Input
SWCHB           equ     $282    	;Console Switches
CTLSWA          equ     $281    	;I/O Control for SCHWA
CTLSWB          equ     $283    	;I/O Control for SCHWB
;
;		PROGRAM EQUATES
;
NTSC		equ	243		;number of active scanlines for NTSC			(DO NOT CHANGE)
PAL			equ	293		;number of active scanlines for PAL			(DO NOT CHANGE)
;
REGION		equ	NTSC		;change this to 'PAL' for PAL regions			(MODIFY IF NEEDED)
;
YRES		equ	208		;screen resolution (must be a factor of DLHIGH)		(MODIFY IF NEEDED)
DLHIGH		equ	16		;display list height (only 8 or 16 allowed)		(MODIFY IF NEEDED)
NUMBLANK	equ	REGION-YRES	;number of blank lines needed in total			(DO NOT CHANGE)
NUMBTOP		equ	NUMBLANK/2	;number of blank lines at top of screen			(DO NOT CHANGE)
NUMBBOT		equ	(NUMBLANK/2)+1	;number of blank lines at bottom of screen		(DO NOT CHANGE)
NUMDL		equ	YRES/DLHIGH	;number of display lists				(DO NOT CHANGE)
DLLEN		equ	5		;display list entry length				(SHOULD NOT HAVE TO CHANGE)
NUMSPR		equ	4		;number of sprites					(MODIFY IF NEEDED)
;
NUMCMODE	equ	1		;number of character-mode display lists (usually 1)	(MODIFY IF NEEDED)
DLLENTRY	equ	(DLLEN*NUMSPR)+(DLLEN*NUMCMODE)+3;number of bytes for each DL entry	(DO NOT CHANGE)
;
DLLRAM		equ	$1800		;Start of Display List List in RAM			(SHOULD NOT HAVE TO CHANGE)
DLRAM		equ	DLLRAM+$200	;Start of Display List in RAM				(DO NOT CHANGE)
NULLDL		equ	DLRAM-3		;Blank Display List (3 bytes)				(DO NOT CHANGE)
TEMPADR		equ	$40		;2 Bytes in zero-page used for address calculations	(DO NOT CHANGE)
TEMPDL		equ	$42		;2 Bytes in zero-page used for DL address		(DO NOT CHANGE)
TEMPVAL		equ	$44		;Used for a temporary value when building DL		(DO NOT CHANGE)
CURRDL		equ	$45		;Current Display List being processed			(DO NOT CHANGE)
CURRSPR		equ	$46		;Current Sprite being processed				(DO NOT CHANGE)
;
;		SCREEN MODE (THE VALUE TO BE BUT INTO 'CTRL')
;
;			 COLOR KILL (0=NORMAL COLOR, 1=NO COLOR BURST)
;			 |
;			 |DMA CONTROL (0=DO NOT USE, 1=DO NOT USE, 2=NORMAL DMA, 3=NO DMA)
;			 |||
;			 |||CHARACTER WIDTH (0=ONE BYTE, 1=TWO BYTES)
;			 ||||
;			 ||||BORDER CONTROL (0=BLACK, 1=BACKGROUND COLOR)
;			 |||||
;			 |||||KANGAROO MODE (0=TRANSPARENCY, 1=NO TRANSPARENCY)
;			 ||||||
;			 ||||||READ MODE (0=160X2 OR 160X4, 1=N/A, 2=320B OR 320D, 3=320A OR 320C)
;			 ||||||||
CTRLVAL		equ	%01000000
;
;		ZERO-PAGE VARIABLES - STARTS AT $47 DUE TO ABOVE SETTINGS
;
rtlocal		=	$47		;2 bytes (system timer)
;
;		SPRITE VARIABLES
;
SPRITEVAR	equ	$1F00		;BE CAREFUL - you don't want to go past $203F
XPOS		equ	SPRITEVAR	;X Position for NUMSPR sprites
YPOS		equ	XPOS+NUMSPR	;Y Position for NUMSPR sprites
XPOSCH		equ	YPOS+NUMSPR	;X Position for NUMDL Character Mode Lines (IF #NUMCMODE = 0) THIS IS FREE SPACE
NEXTVAR		equ	XPOSCH+NUMCMODE	;...and so on
;
;		SCREEN RAM, USED FOR CHARACTER MODE GRAPHICS.
;
SCREEN01	equ	$2200
SCREEN02	equ	$2220
SCREEN03	equ	$2240
SCREEN04	equ	$2260
SCREEN05	equ	$2280
SCREEN06	equ	$22A0
SCREEN07	equ	$22C0
SCREEN08	equ	$22E0
SCREEN09	equ	$2300
SCREEN10	equ	$2320
SCREEN11	equ	$2340
SCREEN12	equ	$2360
SCREEN13	equ	$2380
SCREEN14	equ	$23A0
SCREEN15	equ	$23C0
SCREEN16	equ	$23E0
;		END	$23FF
;		FREE	$2400-$27FF
;
;******************************************************************
;
;		PROGRAM CODE STARTS HERE
;
	org	$8000           	;Start of code
;
  ; A78 Header v4.2
  ; 
  ; Use this file to add an a78 header via the source code of your ROM.
  ;
  ; _Implementation Notes_
  ;
  ; * Include this header near the beginning of your DASM source, but after
  ;   your initial ROM ORG statement.
  ; * Change the fields withn the file to describe your game's hardware
  ;   requirements to emulators and flash carts.
  ; * All unused/reserved bits and bytes must be set to zero.
  ;
.ROMSIZE = $20000                  ; Update with your total ROM size.
  ;
  ; Auto-header ROM allocation follows. If the current address is page aligned,
  ; we backup 128 bytes. This may cause issues if you use a different ORG+RORG
  ; at the start of your ROM - in that case, account for the 128 bytes of
  ; header within your game ROM start ORG+RORG statements.
  ;
    if ( . & $FF ) = 0             ; Check if we're at an even page.
        ORG  (. - 128),0           ; If so, go -128 bytes, for header space.
    else
        ORG .,0                    ; In case zero-fill wasn't specified
    endif                          ; orginally.
  ;
    SEG     ROM
  ;
.HEADER = .

  ; Format detection - do not modify.
    DC.B    4                  ; 0          header major version
    DC.B    "ATARI7800"        ; 1..16      header magic string - zero pad
  ;
  ;
    ORG .HEADER+$11,0
    DC.B    "Planet Defender"   ; 17..48     cartridge title string - zero pad
  ;
  ;
    ORG .HEADER+$31,0
    DC.B    (.ROMSIZE>>24)     ; 49..52     cartridge ROM size
    DC.B    (.ROMSIZE>>16&$FF)
    DC.B    (.ROMSIZE>>8&$FF)
    DC.B    (.ROMSIZE&$FF)
  ;
  ;
    ; The following 2 cartridge type bytes are deprecated as of header v4.0.
    ; It's recommended that you still populate these bytes for support with
    ; platforms that don't yet support v4.
  ;
    DC.B    %00000000          ; 53         cartridge type A
    DC.B    %00000000          ; 54         cartridge type B
    ; _Cartridge Type A_
    ;    bit 7 ; POKEY @ $0800 - $080F
    ;    bit 6 ; EXRAM/M2                   (halt banked RAM)
    ;    bit 5 ; BANKSET
    ;    bit 4 ; SOUPER
    ;    bit 3 ; YM2151 @ $0460 - $0461 
    ;    bit 2 ; POKEY @ $0440 - $044F 
    ;    bit 1 ; ABSOLUTE
    ;    bit 0 ; ACTIVISION
    ; _Cartridge Type B_
    ;    bit 7 ; EXRAM/A8                   (mirror RAM)
    ;    bit 6 ; POKEY @ $0450 - $045F 
    ;    bit 5 ; EXRAM/X2                   (hotspot banked RAM)
    ;    bit 4 ; EXFIX                      (2nd last bank @ $4000)
    ;    bit 3 ; EXROM                      (ROM @ $4000)
    ;    bit 2 ; EXRAM                      (RAM @ $4000)
    ;    bit 1 ; SUPERGAME
    ;    bit 0 ; POKEY @ $4000 - $7FFF
  ;
  ;
    DC.B    1                  ; 55         controller 1 device type
    DC.B    1                  ; 56         controller 2 device type
    ;    0 = none
    ;    1 = 7800 joystick
    ;    2 = lightgun
    ;    3 = paddle
    ;    4 = trakball
    ;    5 = 2600 joystick
    ;    6 = 2600 driving
    ;    7 = 2600 keypad
    ;    8 = ST mouse
    ;    9 = Amiga mouse
    ;   10 = AtariVox
    ;   11 = SNES2Atari
    ;   12 = Mega7800
  ;
  ;
    DC.B    %00000000          ; 57         tv type
    ;    bits 7..3 ; reserved
    ;    bit  2    ; 0:single-region,1:multi-region
    ;    bit  1    ; 0:component,1:composite
    ;    bit  0    ; 0:NTSC,1:PAL
  ;
  ;
    DC.B    %00000000          ; 58         save peripheral
    ;    bits 7..2 ; reserved
    ;    bit  1    ; SaveKey/AtariVox
    ;    bit  0    ; High Score Cart (HSC)
  ;
  ;
    ; The following irq source byte is deprecated as of header v4.0.
    ; It's recommended that you still populate this byte for support with
    ; platforms that don't yet support v4.
  ;
    ORG     .HEADER+62,0
    DC.B    %00000000          ; 62         external irq source
    ;    bits 7..5 ; reserved
    ;    bit  4    ; POKEY  @ $0800 - $080F
    ;    bit  3    ; YM2151 @ $0460 - $0461
    ;    bit  2    ; POKEY  @ $0440 - $044F
    ;    bit  1    ; POKEY  @ $0450 - $045F
    ;    bit  0    ; POKEY  @ $4000 - $7FFF
  ;
    DC.B    %00000000          ; 63         slot passthrough device
    ;    bits 7..1 ; reserved
    ;    bit  0    ; XM module


    ; The following 6 bytes are v4 header specific. You should populate
    ; them with valid info if you're not using V3ONLY, because they will
    ; take precedence over v3 headers.

    DC.B    0                  ; 64         mapper
    ;    0 = linear
    ;    1 = supergame
    ;    2 = activision
    ;    3 = absolute
    ;    4 = souper


    DC.B    0                  ; 65         mapper options
    ; linear_
    ;    bit  7      ; bankset rom
    ;    bits 0-1    ; option @4000...
    ;       0 = none
    ;       1 = 16K EXRAM
    ;       2 = 8K  EXRAM/A8
    ;       3 = 32K EXRAM/M2
    ; supergame_
    ;    bit  7      ; bankset rom
    ;    bits 0-2    ; option @4000...
    ;       0 = none
    ;       1 = 16K EXRAM
    ;       2 = 8K  EXRAM/A8
    ;       3 = 32K EXRAM/M2
    ;       4 = 16K EXROM
    ;       5 = EXFIX
    ;       6 = 32K EXRAM/X2

    DC.B    %00000000          ; 66         audio hi
    DC.B    %00000000          ; 67         audio lo
    ;    bit  5      ; adpcm@420
    ;    bit  4      ; covox@430
    ;    bit  3      ; ym2151@460
    ;    bits 0-2    ; pokey...
    ;       0 = none
    ;       1 = pokey@440
    ;       2 = pokey@450
    ;       3 = dual pokey @440+@450
    ;       4 = pokey@800
    ;       5 = pokey@4000

    DC.B    %00000000          ; 68         interrupt hi
    DC.B    %00000000          ; 69         interrupt lo
    ;    bit  2    ; YM2151
    ;    bit  1    ; pokey 2 (@440)
    ;    bit  0    ; pokey 1 (@4000, @450, or @800)


    ORG     .HEADER+100,0       ; 100..127  footer magic string
    DC.B    "ACTUAL CART DATA STARTS HERE"
;
START
	sei                     	;Disable interrupts
	cld                     	;Clear decimal mode
					;Atari recommended startup procedure
	lda     #$17
	sta     INPTCTRL		;Lock into 7800 mode
	lda     #$7F
	sta     CTRL            	;Disable DMA
	lda     #$00            
	sta     OFFSET
	sta     INPTCTRL
	ldx     #$FF            	;Reset stack pointer
	txs
;
;		CLEAR RAM
;
ClearRam
	ldx     #$40
	lda     #$00
ClearRam_Loop1    
	sta     $00,x           	;Clear Zero-Page Ram
	sta	$100,x			;Clear page 1
	inx
	bne     ClearRam_Loop1

        ldy     #$00            	;Clear Non Zero-Page Ram
        lda     #$18            	;Start at $1800
        sta     $81             
        lda     #$00
        sta     $80
ClearRam_Loop2
        lda     #$00
        sta     ($80),y         	;Store data
        iny                     	;Next byte
        bne     ClearRam_Loop2         	;Branch if not done page
        inc     $81             	;Next page
        lda     $81
        cmp     #$20            	;End at $1FFF
        bne     ClearRam_Loop2        	;Branch if not

        ldy     #$00            	;Clear Ram
        lda     #$22            	;Start at $2200
        sta     $81             
        lda     #$00
        sta     $80
ClearRam_Loop3
        lda     #$00
        sta     ($80),y         	;Store data
        iny                     	;Next byte
        bne     ClearRam_Loop3         	;Branch if not done page
        inc     $81             	;Next page
        lda     $81
        cmp     #$27            	;End at $27FF
        bne     ClearRam_Loop3		;Branch if not

        ldx     #$00
        lda     #$00
ClearRam_Loop4                         	;Clear 2100-213F
        sta     $2100,x
	sta	$2000,x			;Clear 2000-203F
        inx
        cpx     #$40
        bne     ClearRam_Loop4

	ldx	#$00
	lda	#$00
ClearRam_Loop5				;Clear Screen RAM
	sta	SCREEN01,X
	sta	SCREEN01+$100,X
	inx
	cpx	#$FF
	bne	ClearRam_Loop5

	jsr	BuildDisplayListList
;    	
;		SET UP MARIA REGISTERS
;	
	lda	#>CMODEGRAPHIC
	sta	CHARBASE
        lda     >#DLLRAM		;DLL at DLLRAM
	sta	DPPH
	lda	<#DLLRAM
	sta	DPPL
	lda	#$00			;Setup ports to read mode
	sta	CTLSWA
	sta	CTLSWB
;
;		SET UP COLOR REGISTERS
;
	lda	#$32
	sta	P0C1
	sta	P1C1
	sta	P2C1
	sta	P3C1
	lda	#$2A
	sta	P4C1
	sta	P5C1
	sta	P6C1
	sta	P7C1
	lda	#$0C
	sta	P0C2
	sta	P1C2
	sta	P2C2
	sta	P3C2
	sta	P4C2
	sta	P5C2
	sta	P6C2
	sta	P7C2
	lda	#$84
	sta	P0C3
	sta	P1C3
	sta	P2C3
	sta	P3C3
	sta	P4C3
	sta	P5C3
	sta	P6C3
	sta	P7C3
	lda	#$00
        sta     BACKGRND 
;
;		TURN ON SCREEN
;
	lda	#CTRLVAL
	sta	CTRL
TEMP					;This temporary loop just fills zone 6 with character graphics
	LDA	#$10
	STA	XPOSCH+5
	LDX	#$00
TEMPLOOP
	TXA
	TAY
	INY
	INY
	TYA
	STA	SCREEN06,X
	INX
	CPX	#$20
	BMI	TEMPLOOP
;
;		HERE IS THE MAIN LOOP.  THIS IS WHERE MOST OF THE PROGRAM WILL BE EXECUTING FROM
;		USE 'JSR'S TO PERFORM THE VARIOUS TASKS (DEPENDING ON THE APPLICATION)
;		LEAVE 'JSR WAITVBLANK' AS THE FIRST SUBROUTINE CALLED, AND LEAVE 'JSR BUILDDISPLAYLIST'
;		AS THE LAST SUBROUTINE CALLED
;
MainLoop
	jsr	WaitVBlank		;Wait for VBLANK
	
	lda	SWCHA			;Read stick
	and	#$80			;Pushed Right?
	bne	skip1
	ldx	XPOS			;Move sprite to right
        cpx     #152
        beq     skip1
	inx
	stx	XPOS
skip1
	lda	SWCHA			;Read stick
	and 	#$40			;Pushed Left?
	bne 	skip2
	ldx 	XPOS			;Move sprite to left
        cpx     #0
        beq     skip2
	dex
	stx 	XPOS
skip2
        lda     SWCHA			;Read stick
        and     #$20			;Pushed Down?
        bne     skip3		
        ldx     YPOS			;Move sprite down
        cpx	#YRES-#DLHIGH	
        beq	skip3			;Don't move if we are at the bottom
        inx
        stx     YPOS
skip3
        lda     SWCHA			;Read stick
        and     #$10			;Pushed Up?
        bne     skip4		
        ldx     YPOS			;Move sprite up
        beq	skip4			;Don't move if we are at the top
        dex			
        stx     YPOS
skip4
	inc	XPOS+1			;Increase the second sprite

	jsr	BuildDisplayList
	jmp	MainLoop
;	
;		BUILD DISPLAY LIST ENTRIES
;
BuildDisplayList
       	lda	<#DLRAM
	sta	TEMPDL
	lda	>#DLRAM
	sta	TEMPDL+1
	lda	#$00
	sta	CURRDL			;Current Display List
BuildDisplayList_MainLoop
	ldy	#$00
	lda	#NUMCMODE
	beq	BuildDisplayList_Sprite
	sta	TEMPVAL
BuildDisplayList_Char
	ldx	CURRDL
	lda	SCREENPL,x
	sta	(TEMPDL),y
	iny
	lda	CHARWM,x		;Get Write Mode for Current Display List's Character Mode
	ora	#$60
	sta	(TEMPDL),y
	iny
	lda	SCREENPH,x
	sta	(TEMPDL),y
	iny
	lda	CHARPALETTE,x		;Get the palette for the current Dispaly List's Character Mode
	ora	CHARWIDTH,x		;'ora' the value with the Character Mode width
	sta	(TEMPDL),y
	iny
	lda	XPOSCH,x		;X Position of each Character Mode line
	sta	(TEMPDL),y
	iny
	dec	TEMPVAL
	lda	TEMPVAL
	bne	BuildDisplayList_Char
BuildDisplayList_Sprite
	lda	#$00
	sta	CURRSPR			;Current Sprite
BuildDisplayList_SpriteLoop
	ldx	CURRSPR
	lda	YPOS,x
	lsr
	lsr
	lsr
BuildDisplayList_16
	lsr
BuildDisplayList_8
	cmp	CURRDL			;See if we need to process the top of the sprite
	bne	BuildDisplayList_NotTop
	jsr	DoSpriteTop
BuildDisplayList_NotTop
	clc
	adc	#$01			;See if we need to process the bottom of the sprite
	cmp	CURRDL
	bne	BuildDisplayList_NotBot
	jsr	DoSpriteBot
BuildDisplayList_NotBot
	inc	CURRSPR
	lda	CURRSPR
	cmp	#NUMSPR
	bmi	BuildDisplayList_SpriteLoop
BuildDisplayList_End
	lda	#$00
	sta	(TEMPDL),y
	iny
	sta	(TEMPDL),y
	iny
	sta	(TEMPDL),y
	iny
	jsr	IncrementDLAdr
	inc	CURRDL
	lda	CURRDL
	cmp	#NUMDL
	bpl	BuildDisplayList_Rts
	jmp	BuildDisplayList_MainLoop
BuildDisplayList_Rts
	rts
	
DoSpriteTop
	pha
	ldx	CURRSPR
   	lda	SPRITEL,x		;Low byte of data address			
	sta     (TEMPDL),y		;Low byte of data address
	iny
	lda	SPRITEWM,x		;Get the Write Mode for the current sprite
	ora	#$40
	sta     (TEMPDL),y
	iny 
	lda	YPOS,x		
	and	#$0F		
	ora	SPRITEH,x		;High byte of data address
	sta     (TEMPDL),y		;High byte of data address
	iny
	lda	SPRITEPALETTE,x		;Get the palette for the current sprite
	ora	SPRITEWIDTH,x		;'ora' the value with the sprite width
	sta     (TEMPDL),y
	iny
	lda	XPOS,x			;Horizontal position
        sta     (TEMPDL),y
	iny
DoSpriteTop_Rts
	pla
	rts

DoSpriteBot
	pha
	ldx	CURRSPR
	lda	YPOS,x
	and	#$0F			;See if sprite is entirely within this region
	beq	DoSpriteBot_Rts		;branch if it is

	lda	SPRITEL,x		;Low byte of data address			
	sta     (TEMPDL),y
	iny
	lda	SPRITEWM,x		;Get the Write Mode for the current sprite
	ora	#$40
	sta     (TEMPDL),y
	iny 
	lda	YPOS,x
	and	#$0F
	eor	#$0F
	sta	TEMPVAL
	lda	SPRITEH,x		;High byte of data address
	clc
	sbc 	TEMPVAL
	sta     (TEMPDL),y
	iny
	lda	SPRITEPALETTE,x		;Get the palette for the current sprite
	ora	SPRITEWIDTH,x		;'ora' the value with the sprite width
	sta     (TEMPDL),y
	iny
	lda	XPOS,x			;Horizontal position
	sta     (TEMPDL),y
	iny
DoSpriteBot_Rts
	pla
	rts
;
;		BUILD DISPLAY LIST LIST
;
BuildDisplayListList
       	lda	<#DLLRAM
	sta	TEMPADR
	lda	>#DLLRAM
	sta	TEMPADR+1
	ldy	#$00
	ldx	#NUMBTOP
BuildDLL_TopLoop
	cpx	#$10
	bmi	BuildDLL_TopLT16
BuildDLL_TopGT16
	txa
	sec
	sbc	#$10			;subtract 16 lines
	tax
	lda	#$10
	jmp	BuildDLL_PutTopEntry
BuildDLL_TopLT16
	txa
	ldx	#$00			;last entry - remainder lines
BuildDLL_PutTopEntry
	sec
	sbc	#$01			;entry is always "Number of Lines - 1"
	;ora	(#DLHIGH<<2)		;take DLHIGH value, shift it left 2 bits to be
        sta     (TEMPADR),y		; in line with 'Holey DMA' value
        jsr	IncrementTempAdr
        lda     >#NULLDL		;NULLDL = blank DL
        sta	(TEMPADR),y
        jsr	IncrementTempAdr
        lda     <#NULLDL
    	sta	(TEMPADR),y
    	jsr	IncrementTempAdr
	cpx	#$00
	bne	BuildDLL_TopLoop                   
;
;		#YRES MODE LINES DIVIDED INTO #NUMDL REGIONS
;
       	lda	<#DLRAM
	sta	TEMPDL
	lda	>#DLRAM
	sta	TEMPDL+1
        ldx	#NUMDL			;#NUMDL DLL entries
BuildDLL_MainLoop
	lda	#DLHIGH-1		;DLHIGH lines
        ora	(#DLHIGH<<2)		;take DLHIGH value, shift it left 2 bits to be
        sta	(TEMPADR),y		; in line with 'Holey DMA' value
        jsr	IncrementTempAdr
	lda	TEMPDL+1
        sta	(TEMPADR),y
        jsr	IncrementTempAdr
	lda	TEMPDL
	sta	(TEMPADR),y
	jsr	IncrementTempAdr
	jsr	IncrementDLAdr
        dex
        cpx	#$00
        bpl	BuildDLL_MainLoop
;
;	Bottom blank lines
;
	ldx	#NUMBBOT
BuildDLL_BotLoop
	cpx	#$10
	bmi	BuildDLL_BotLT16
BuildDLL_BotGT16
	txa
	sec
	sbc	#$10			;subtract 16 lines
	tax
	lda	#$10
	jmp	BuildDLL_PutBotEntry
BuildDLL_BotLT16
	txa
	ldx	#$00			;last entry - remainder lines
BuildDLL_PutBotEntry
	sec
	sbc	#$01			;entry is always "Number of Lines - 1"
	;ora	(#DLHIGH<<2)		;take DLHIGH value, shift it left 2 bits to be
        sta     (TEMPADR),y		; in line with 'Holey DMA' value
        jsr	IncrementTempAdr
        lda     >#NULLDL		;NULLDL = blank DL
        sta	(TEMPADR),y
        jsr	IncrementTempAdr
        lda     <#NULLDL
    	sta	(TEMPADR),y
    	jsr	IncrementTempAdr
	cpx	#$00
	bne	BuildDLL_BotLoop
BuildDLL_FinalEntry
	lda	#$00
	sta	NULLDL
	sta	NULLDL+1
	sta	NULLDL+2
BuildDLL_Rts
	rts

IncrementTempAdr
	iny
	cpy	#$00
	bne	IncrementTempAdr_Rts
	inc	TEMPADR+1
IncrementTempAdr_Rts
	rts

IncrementDLAdr
	lda	TEMPDL
	clc
	adc	#DLLENTRY
	sta	TEMPDL
	bcc	IncrementDLAdr_Rts
	inc	TEMPDL+1
IncrementDLAdr_Rts
	rts

;***************
;		WAIT FOR VERTICAL BLANK TO END
;
WaitVBlank:
WaitVBOff:
	bit	MSTAT  
	bmi	WaitVBOff
WaitVBOn:
	bit	MSTAT
	bpl	WaitVBOn
	inc	rtlocal+1
	bne	WaitVBRts
	inc	rtlocal
WaitVBRts
	rts	
;***************
;		NMI CALL
;      
NMI
	RTI
;***************
;		INTERRUPT CALL
;	
IRQ
	RTI
;***************
;		THIS SHOULD BE WHERE THE CHARACTER MODE GRAPHICS ARE HELD
;
	org	$C000
CMODEGRAPHIC
	incbin	charmode.dat
;***************
;		THIS SHOULD BE WHERE THE SPRITE GRAPHIC DATA IS HELD
;
	org	$E000
SPRITEGRAPHIC
	incbin	spritegr.dat
;
;		THE FOLLOWING LINES ARE THE THE POINTERS TO TABLES FOR ATTRIBUTES TO THE GRAPHICS
;
	org	$F000
;
;***************
;		POINTERS TO THE CHARACTER MODE SCREEN RAM
;
SCREENPL
	.byte	<#SCREEN01,<#SCREEN02,<#SCREEN03,<#SCREEN04,<#SCREEN05,<#SCREEN06,<#SCREEN07,<#SCREEN08
	.byte	<#SCREEN09,<#SCREEN10,<#SCREEN11,<#SCREEN12,<#SCREEN13,<#SCREEN14,<#SCREEN15,<#SCREEN16
SCREENPH
	.byte	>#SCREEN01,>#SCREEN02,>#SCREEN03,>#SCREEN04,>#SCREEN05,>#SCREEN06,>#SCREEN07,>#SCREEN08
	.byte	>#SCREEN09,>#SCREEN10,>#SCREEN11,>#SCREEN12,>#SCREEN13,>#SCREEN14,>#SCREEN15,>#SCREEN16
;***************
;		WRITE MODE FOR EACH CHARACTER MODE DL ENTRY IN HIGHEST BIT - THERE SHOULD BE #NUMDL ENTRIES
;		EXAMPLE: $00=160X2 OR 320X1, $80=160X4 OR 320X2
;
CHARWM
	.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;***************
;		PALETTES FOR EACH CHARACTER MODE DL ENTRY IN 3 HIGHEST BITS - THERE SHOULD BE #NUMDL ENTRIES
;		EXAMPLE: PALETTE 1 WOULD BE ENTERED AS $20, PALETTE 2 WOULD BE ENTERED AS $40, ETC.
;
CHARPALETTE
	.byte	$00,$20,$40,$60,$80,$A0,$c0,$E0,$00,$20,$40,$60,$80,$A0,$C0,$E0
;***************
;		2'S COMPLIMENT WIDTH OF EACH CHARACTER MODE LINE (IN BYTES) - THERE SHOULD BE #NUMDL ENTRIES
;		EXAMPLE: A CHARACTER MODE OF WIDTH OF 1 BYTE WOULD BE ENTERED AS $1F, 2 BYTES WOULD BE $1E, ETC. UP TO 32
;		WHICH WOULD BE ENTERED AS $00
;
CHARWIDTH
	.byte	$1C,$1C,$1C,$1C,$1C,$00,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C
;***************
;		POINTERS TO THE SPRITE GRAPHICS - THERE SHOULD BE AS MANY ENTRIES PER 'LOW' AND 'HIGH' VALUES
;		AS THERE ARE SPRITES.
;
SPRITEL
	.byte	<#(SPRITEGRAPHIC),<#(SPRITEGRAPHIC+$02),<#(SPRITEGRAPHIC+$04),<#(SPRITEGRAPHIC+$06)
SPRITEH
	.byte	>#(SPRITEGRAPHIC),>#(SPRITEGRAPHIC+$02),>#(SPRITEGRAPHIC+$04),>#(SPRITEGRAPHIC+$06)
;***************
;		WRITE MODE FOR EACH SPRITE IN HIGHEST BIT - THERE SHOULD BE AS MANY ENTRIES AS THERE ARE SPRITES
;		EXAMPLE: $00=160X2 OR 320X1, $80=160X4 OR 320X2
;
SPRITEWM
	.byte	$00,$00,$00,$00,$00,$00,$00,$00
;***************
;		PALETTES FOR EACH SPRITE IN 3 HIGHEST BITS - THERE SHOULD BE AS MANY ENTRIES AS THERE ARE SPRITES
;		EXAMPLE: PALETTE 1 WOULD BE ENTERED AS $20, PALETTE 2 WOULD BE ENTERED AS $40, ETC.
;
SPRITEPALETTE
	.byte	$00,$80,$40,$60,$80,$A0,$c0,$E0
;***************
;		2'S COMPLIMENT WIDTH OF EACH SPRITE (IN BYTES) - THERE SHOULD BE AS MANY ENTRIES AS THERE ARE SPRITES
;		EXAMPLE: A SPRITE OF WIDTH OF 1 BYTE WOULD BE ENTERED AS $1F, 2 BYTES WOULD BE $1E, ETC. UP TO 32
;		WHICH WOULD BE ENTERED AS $00
;
SPRITEWIDTH
	.byte	$1E,$1E,$1E,$1E,$1E,$1E,$1E,$1E
;***************
;		CARTRIDGE ID
;
	.byte	"COPYRIGHT 2024 Jason Rowe"
;***************
;		CART RESET VECTOR
;
	 org	$FFF8
	.byte   $FF		;Region verification
	.byte   $87		;ROM start $8000
	.word   #NMI
	.word   #START
	.word   #IRQ
