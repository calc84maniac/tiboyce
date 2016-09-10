#ifdef DEBUG
#ifndef CEMU
#define DBGNOSCALE
#endif
#endif

; Some configuration options
#ifndef SCANDELAY
#define SCANDELAY 12
#endif

#define FRAME_LENGTH (SCANDELAY*256*154)
#define TIMA_LENGTH (SCANDELAY*256*16/456)

MAGENTA = 10
OLIVE = 11
WHITE = 12
BLACK = 13
BLUE = 14

WHITE_BYTE = WHITE*$11
BLACK_BYTE = BLACK*$11
BLUE_BYTE = BLUE*$11

; Some standalone equates
_sprintf = $0000BC
_GetCSC = $02014C
_Mov9ToOP1 = $020320
_CmpPrgNamLen = $020504
_chkFindSym = $02050C
_InsertMem = $020514
_EnoughMem = $02051C
_CmpMemNeed = $020520
_CreatePVar4 = $020524
_CreatePVar3 = $020528
_DelVar = $020588
_DelMem = $020590
_ClrLCDFull = $020808
_HomeUp = $020828
_VPutSN = $020838
_RunIndicOff = $020848
_createAppVar = $021330
_DelVarArc = $021434
_Arc_Unarc = $021448
_DrawStatusBar = $021A3C
_ChkInRAM = $021F98
penCol = $D008D2
penRow = $D008D5
pTemp = $D0259A
progPtr = $D0259D
pixelShadow = $D031F6
userMem = $D1A881
vRam = $D40000
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

; 84+CE IO definitions
mpFlashWaitStates = $E00005

mpLcdTiming0 = $E30000
mpLcdTiming1 = $E30004
mpLcdTiming2 = $E30008
mpLcdBase = $E30010
mpLcdCtrl = $E30018
mpLcdImsc = $E3001C
mpLcdRis = $E30020
pLcdMis = $4024
mpLcdMis = $E30024
mpLcdIcr = $E30028
mpLcdPalette = $E30200
mpLcdCursorImg = $E30800

mpIntRawStatus = $F00000
mpIntEnable = $F00004
mpIntAcknowledge = $F00008
mpIntLatch = $F0000C
pIntMaskedStatus = $5014
mpIntMaskedStatus = $F00014

TMR_ENABLE = %00101001
mpTimer1Count = $F20000
mpTimer1Reset = $F20004
mpTimer1Match1 = $F20008
mpTimer1Match2 = $F2000C
mpTimer2Count = $F20010
mpTimer2Reset = $F20014
mpTimer2Match1 = $F20018
mpTimer2Match2 = $F2001C
mpTimer3Count = $F20020
mpTimer3Reset = $F20024
mpTimer3Match1 = $F20028
mpTimer3Match2 = $F2002C
mpTimerCtrl = $F20030
mpTimerIntStatus = $F20034

mpRtcSecondCount = $F30000

mpKeypadScanMode = $F50000
mpKeypadGrp0 = $F50010
mpKeypadGrp1 = $F50012
mpKeypadGrp2 = $F50014
mpKeypadGrp3 = $F50016
mpKeypadGrp4 = $F50018
mpKeypadGrp5 = $F5001A
mpKeypadGrp6 = $F5001C
mpKeypadGrp7 = $F5001E

#ifdef CEMU
mpCEmuDbg = $FB0000
#endif

mpZeroPage = $FF0000

;GB IO equates
ioregs = $ff00
P1 = $ff00
SB = $ff01
SC = $ff02
DIV = $ff04
TIMA = $ff05
TMA = $ff06
TAC = $ff07
IF = $ff0f

NR10 = $ff10
NR11 = $ff11
NR12 = $ff12
NR13 = $ff13
NR14 = $ff14

NR21 = $ff16
NR22 = $ff17
NR23 = $ff18
NR24 = $ff19

NR30 = $ff1a
NR31 = $ff1b
NR32 = $ff1c
NR33 = $ff1d
NR34 = $ff1e

NR41 = $ff20
NR42 = $ff21
NR43 = $ff22
NR44 = $ff23

NR50 = $ff24
NR51 = $ff25
NR52 = $ff26

WavePatternRAM = $ff30

LCDC = $ff40
STAT = $ff41
SCY = $ff42
SCX = $ff43
LY = $ff44
LYC = $ff45
DMA = $ff46
BGP = $ff47
OBP0 = $ff48
OBP1 = $ff49
WY = $ff4a
WX = $ff4b
IE = $ffff

; Memory areas used by the emulator
palettemem = mpLcdPalette
cursormem = mpLcdCursorImg

z80codebase = vRam
myz80stack = $FE00
vram_tiles_start = (pixelShadow | 31) + 1
vram_pixels_start = vram_tiles_start + $4000
vram_start = vram_pixels_start + $6000
digits = vram_start + $2000
wram_start = vram_start + $4000
memroutineLUT = vram_start + $6000
scanlineLUT = memroutineLUT + $0200
rombankLUT = scanlineLUT + (174*3)
rombankLUT_end = rombankLUT + (128*3)
hram_start = z80codebase + $FE00

vram_base = vram_start - $8000
wram_base = wram_start - $C000
hram_base = z80codebase

gb_frame_buffer_1 = vRam + (320*240)
text_frame_1 = gb_frame_buffer_1 + (160*150)
gb_frame_buffer_2 = gb_frame_buffer_1 + (160*240)
text_frame_2 = gb_frame_buffer_2 + (160*150)

recompile_struct = z80codebase + $010000
recompile_cache_end = gb_frame_buffer_1
	
	.db tExtTok, tAsm84CeCmp
	.org userMem
	
	call _RunIndicOff
	or a
	sbc hl,hl
	ld (menuFrame),hl
	
RepopulateMenu:
	ld ix,rombankLUT
	push ix
	 call ROMSearch
	 ld (lastROM),ix
	pop bc
	lea hl,ix
	or a
	sbc hl,bc
	ex de,hl
	
	ld hl,(menuFrame)
	sbc hl,bc
	jr c,RepopulateMenuTop
	sbc hl,de
	jr nc,RepopulateMenuTop
	
	ld hl,(menuSelection)
	or a
	sbc hl,bc
	jr c,RepopulateMenuTop
	sbc hl,de
	jr c,RedrawMenuClear
	
RepopulateMenuTop:
	ld (menuFrame),bc
	ld (menuSelection),bc
	
RedrawMenuClear:
	ld hl,vRam
	push hl
	pop de
	inc de
	ld bc,320*240*2-1
	ld (hl),c
	ldir
RedrawMenu:
	ld ix,(menuFrame)
	xor a
	ld (penRow),a
RedrawMenuLoop:
	sbc hl,hl
	ld (penCol),hl
	ld hl,(lastROM)
	lea de,ix
	sbc hl,de
	jr z,SelectionLoop
	ld hl,(menuSelection)
	sbc hl,de
	res 3,(iy+5)
	jr nz,_
	set 3,(iy+5)
_
	ld hl,(ix)
	call DrawMenuItem
	lea ix,ix+3
	ld a,(penRow)
	add a,12
	ld (penRow),a
	xor 240
	jr nz,RedrawMenuLoop
	
SelectionLoop:
	halt
	call _GetCSC
	ld hl,(menuSelection)
	cp 1
	jr z,MenuDown
	cp 4
	jr z,MenuUp
	cp 9
	jr z,MenuEnter
	cp 15
	jr nz,SelectionLoop
	jr RestoreHomeScreen
	
MenuDown:
	inc hl
	inc hl
	inc hl
	ld de,(lastROM)
	sbc hl,de
	jr z,SelectionLoop
	add hl,de
	ld (menuSelection),hl
	ld a,(menuFrame)
	sub l
	add a,20*3
	jp nz,RedrawMenu
	ld (menuFrame),hl
	jp RedrawMenuClear
	
MenuUp:
	ld de,rombankLUT
	sbc hl,de
	jr z,SelectionLoop
	add hl,de
	ld a,(menuFrame)
	cp l
	dec hl
	dec hl
	dec hl
	ld (menuSelection),hl
	jp nz,RedrawMenu
	ld de,-19*3
	add hl,de
	ld (menuFrame),hl
	jp RedrawMenuClear
	
MenuEnter:
	ld hl,(menuSelection)
	ld hl,(hl)
	ld de,-6
	add hl,de
	ld b,(hl)
	ld de,ROMName+1
_
	dec hl
	ld a,(hl)
	ld (de),a
	inc de
	djnz -_
	xor a
	ld (de),a
	
	call StartROM
	jp nc,RepopulateMenu
	
RestoreHomeScreen:
	ld hl,pixelShadow
	ld de,pixelShadow+1
	ld bc,(8400*3) - 1
	ld (hl),0
	ldir
	set 0,(iy+3)
	call _DrawStatusBar
	call _HomeUp
	jp _ClrLCDFull
	
StartROM:
	call LoadROM
	ret c
	
	ld hl,(rombankLUT)
	ld de,$4000
	add hl,de
	ld (rom_start),hl
	ld bc,$0147
	add hl,bc
	ld a,(hl)
	ld b,0
	or a
	jr z,mbc_valid_no_carry
	inc b	;MBC1
	dec a
	cp $04-$01
	jr c,mbc_valid
	inc b	;MBC2
	sub $05-$01
	cp $07-$05
	jr c,mbc_valid
	inc b	;MBC3
	sub $0F-$05
	cp $14-$0F
mbc_valid:
	ccf
mbc_valid_no_carry:
	ret c
	ld a,b
	ld (mbc),a
	
	; Mirror ROM across all banks
	lea de,ix
	ld hl,rombankLUT_end
	sbc hl,de
	jr z,_
	ret c
	push hl
	pop bc
	ld hl,rombankLUT
	ldir
_
	
	; Handle MBC-specific mirrors
	ld hl,(rombankLUT+3)
	ld (rombankLUT),hl
	dec a	;MBC1
	jr nz,_
	ld hl,(rombankLUT+($21*3))
	ld (rombankLUT+($20*3)),hl
	ld hl,(rombankLUT+($41*3))
	ld (rombankLUT+($40*3)),hl
	ld hl,(rombankLUT+($61*3))
	ld (rombankLUT+($60*3)),hl
_
	
	call LoadRAM
	ret c
	
	di
RestartFromHere:
	push iy
	ld hl,mpFlashWaitStates
	push hl
	ld a,(hl)
	push af
	ld (hl),2
	ld hl,(mpIntEnable)
	push hl
	ld hl,(mpIntLatch)
	push hl
	ld hl,$000803
	ld (mpIntEnable),hl
	ld hl,$000017
	ld (mpIntLatch),hl
	
	ld hl,(mpLcdBase)
	push hl
	ld hl,(mpLcdCtrl)
	push hl
	ld hl,$463B01
	call GetAndSetLCDTiming
	push de
	
	ld hl,palettecode
	ld de,palettemem
	ld bc,palettecodesize
	ldir
	
	ld hl,$0D25
	ld (mpLcdCtrl),hl
	ld hl,gb_frame_buffer_1
	ld (mpLcdBase),hl
	push hl
	pop de
	inc de
#ifdef DBGNOSCALE
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96
	ld (hl),BLUE_BYTE
	ldir
	ld bc,160*144
	ld (hl),WHITE_BYTE
	ldir
	ld bc,160*96-1
	ld (hl),BLUE_BYTE
	ldir
#else
	ld bc,320*240-1
	ld (hl),WHITE_BYTE
	ldir
#endif
	
#ifdef DEBUG
	ld hl,StartText
	push hl
	 call debug_printf
	pop hl
#endif
	
	ld a,3
	ld (mpKeypadScanMode),a
	inc a
	ld (mpLcdImsc),a
	
	ld hl,(mpTimerCtrl)
	push hl
	xor a
	sbc hl,hl
	ld (mpTimerCtrl),hl
	ld (mpTimer1Count),hl
	ld (mpTimer1Count+3),a
	ld (mpTimer2Count),hl
	ld (mpTimer2Count+3),a
	ld (mpTimer2Match1),hl
	ld (mpTimer2Match1+3),a
	ld (mpTimer2Match2),hl
	ld (mpTimer2Match2+3),a
	ld (mpTimer2Reset),hl
	ld (mpTimer2Reset+3),a
	dec hl
	ld (mpTimer1Match1),hl
	ld (mpTimer1Match1+3),a
	ld hl,FRAME_LENGTH - 1
	ld (mpTimer1Reset),hl
	ld (mpTimer1Reset+3),a
	ld hl,FRAME_LENGTH - (SCANDELAY*256*144) - 1
	ld (mpTimer1Match2),hl
	ld (mpTimer1Match2+3),a
	
	ld (saveSP),sp
	
	ld sp,palettemem + $0200
	
	ld hl,z80codebase
	push hl
	pop de
	inc de
	ld bc,$00FEFF
	ld (hl),l
	ldir
	ld hl,hmem_init
	inc b
	ldir
	
	ld hl,cursorcode
	ld de,cursormem
	ld bc,cursorcodesize
	ldir
	
	ld hl,z80code
	ld de,z80codebase
	ld bc,z80codesize
	ldir
	
	ld hl,scanlineLUT
	push hl
	pop de
	inc de
	ld (hl),$FF
	ld bc,174*3-1
	ldir
	
	ld hl,vram_tiles_start
	push hl
	pop de
	inc de
	ld bc,$4000 + $6000 - 1
	ld (hl),c
	ldir
	
	call generate_digits
	
	ld a,z80codebase >> 16
	ld mb,a
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET
	push.s hl
	
	ld hl,(rombankLUT+3)
	ld (rom_bank_base),hl
	
	ld a,(mbc)
	ld (z80codebase+mbc_z80),a
	
	ld hl,(cram_start)
	ld bc,-$A000
	add hl,bc
	ld (z80codebase+cram_base_0),hl
	ld (cram_bank_base),hl
	
	call prepare_next_frame
	call update_palettes
	
	call flush_code_reset_padding
	
	call ApplyConfiguration
	
	ld de,$0100
	call lookup_code
	
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	
_
	ld a,(mpIntRawStatus)
	bit 4,a
	jr z,-_
	
	ld a,1
	ld (z80codebase+curr_rom_bank),a
	ld bc,$0013
	ld de,$00D8
	ld hl,$014D
	ld iy,$FFFE
	jp set_gb_stack
	
CmdExit:
saveSP = $+1
	ld sp,0
	ld hl,vRam
	push hl
	pop de
	inc de
	ld bc,320*240*2-1
	ld (hl),$FF
	ldir
	ld b,a
	ld a,$D0
	ld mb,a
	xor a
	ld (mpLcdImsc),a
	pop hl
	ld (mpTimerCtrl),hl
	pop hl
	call SetLCDTiming
	pop hl
	ld (mpLcdCtrl),hl
	pop hl
	ld (mpLcdBase),hl
	pop hl
	ld (mpIntLatch),hl
	pop hl
	ld (mpIntEnable),hl
	pop af
	pop hl
	ld (hl),a
	pop iy
	srl b
	jp nz,RestartFromHere
	ei
	push af
	call SaveRAM
	pop af
	ret
	
LoadROM:
	ld hl,ROMName
	call LookUpAppvar
	ret c
	ld a,c
	sub 9
	ld c,a
	jr nc,_
	ld a,b
	or a
	ret z
	dec b
_
	push bc
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	pop bc
	ret nz
	ld d,(hl)
	inc hl
	ld (current_description),hl
	
	ld hl,ROMName-1
_
	inc hl
	ld a,(hl)
	or a
	jr nz,-_
	ld (hl),'R'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),a
	
	ld e,a
	ld ix,rombankLUT
LoadROMLoop:
	push de
	 ld hl,ROMName
	 call LookUpAppvarForceARC
	pop de
	ret c
	ld a,(hl)
	cp e
	scf
	ret nz
	inc hl
	dec bc
LoadROMPageLoop:
	push de
	 push bc
	  ld c,(hl)
	  inc hl
	  ld b,(hl)
	  inc hl
	  ld de,-$4000
	  add hl,de
	  ld (ix),hl
	  lea ix,ix+3
	  add hl,bc
	  sbc hl,de
	  ex (sp),hl
	  inc bc
	  sbc hl,bc
	  ex (sp),hl
	 pop bc
	pop de
	ret c
	jr z,_
	dec d
	ret z
	inc e
	jr LoadROMPageLoop
_
	dec d
	ret z
	inc e
	ld hl,ROMName+1
	xor a
	ld bc,9
	cpir
	dec hl
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr c,LoadROMLoop
	jr z,_
	cp 'F'+1
	jr nz,LoadROMLoop
	ld (hl),'0'
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr nz,LoadROMLoop
_
	ld (hl),'A'
	jr LoadROMLoop
	
LoadRAM:
	ld hl,ROMName+1
	ld bc,9
	xor a
	cpir
	dec hl
	dec hl
	ld (hl),'V'
	dec hl
	ld (hl),'A'
	dec hl
	ld (hl),'S'
	
	ld de,8*1024
	ld a,(mbc)
	cp 2
	jr z,++_
	ld hl,(rom_start)
	ld bc,$0149
	add hl,bc
	ld a,(hl)
	or a
	jr nz,_
	ld d,e
_
	cp 3
	jr c,_
	ld d,(32*1024) >> 8
_
	
	push de
	 ld a,d
	 or e
	 jr z,LoadRAMNoVar
_
	 ld hl,ROMName
	 call LookUpAppvar
	 jr nc,_
	 or a
	 sbc hl,hl
	 call _createAppVar
	 jr -_
_
	 push bc
	  push hl
	   ld a,b
	   or c
	   jr z,_
	   ld de,vram_tiles_start
	   ldir
_
	  pop de
	 pop hl
	 call _ChkInRAM
	 jr nz,LoadRAMNoVar
	 ex de,hl
	 dec hl
	 ld (hl),b
	 dec hl
	 ld (hl),c
	 inc hl
	 inc hl
	 call _DelMem
LoadRAMNoVar:
	pop hl
	push hl
	 inc hl
	 inc hl
	 call _EnoughMem
	 ret c
	 ex de,hl
	 ld de,program_end
	 call _InsertMem
	pop bc
	ld a,c
	ld (de),a
	inc de
	ld a,b
	ld (de),a
	inc de
	ld (cram_start),de
	or c
	jr z,_
	ld hl,vram_tiles_start
	ldir
	ret
_
	ld hl,mpZeroPage
	ld (cram_start),hl
	ret
	
	

SaveRAM:
	ld hl,(ram_size)
	ld a,h
	or l
	jr z,SaveRAMDeleteMem
	
	ld hl,ROMName
	call LookUpAppvar
	jr c,SaveRAMRecreate
	
	push de
	 dec hl
	 dec hl
	 inc bc
	 inc bc
	 ld de,program_end
	 call memcmp
	pop hl
	jr z,SaveRAMDeleteMem
	
	; Delete the existing variable
	push hl
	pop ix
	ld de,(ix-7)
	ld d,(ix-4)
	ld e,(ix-3)
	call _DelVarArc
	
SaveRAMRecreate:
	ld hl,ROMName
	call _Mov9ToOP1
	call _CmpPrgNamLen
	ld bc,0
	call _CreatePVar4
	push hl
	pop ix
	ld hl,(program_end << 16) | (program_end & $00FF00) | (program_end >> 16)
	ld (ix-5),hl
	
	ld a,(AutoArchive)
	or a
	ret z
	ld hl,ROMName
	call _Mov9ToOP1
	jp _Arc_Unarc
	
SaveRAMDeleteMem:
	ld hl,program_end
	ld de,(hl)
	inc de
	inc.s de
	jp _DelMem
	
	
LookUpAppvar:
	call _Mov9ToOP1
	call _chkFindSym
	ret c
GetDataSection:
	call _ChkInRAM
	ex de,hl
	ld bc,9
	jr z,_
	add hl,bc
	ld c,(hl)
	add hl,bc
	inc hl
_
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	or a
	ret
	
	
LookUpAppvarForceARC:
	call _Mov9ToOP1
_
	call _chkFindSym
	ret c
	call _ChkInRAM
	jr nz,_
	call _Arc_Unarc
	jr -_
_
	ex de,hl
	ld de,9
	add hl,de
	ld e,(hl)
	add hl,de
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ret
	
LookUpAppvarForceRAM:
	call _Mov9ToOP1
_
	call _chkFindSym
	ret c
	call _ChkInRAM
	jr z,_
	call _Arc_Unarc
	jr -_
_
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	or a
	ret
	
memcmp:
	ld a,(de)
	inc de
	cpi
	ret nz
	ret po
	jr memcmp
	
ROMSearch:
	xor a
	sbc hl,hl
	ld (penCol),hl
	ld (penRow),a
	ld hl,(progPtr)
	or a
ROMSearchLoop:
	ld (ix),hl
	ex de,hl
	ld hl,(pTemp)
	sbc hl,de
	ret z
	ld a,(de)
	ld hl,-7
	add hl,de
	ld de,(hl)
	xor appVarObj
	jr nz,NoMatch
	push hl
	 inc hl
	 inc hl
	 inc hl
	 ld d,(hl)
	 inc hl
	 ld e,(hl)
	 call GetDataSection
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	 jr nz,_
	 lea ix,ix+3
_
	pop hl
	ld de,(hl)
NoMatch:
	ld e,1
	mlt de
	sbc hl,de
	jr ROMSearchLoop
	
	
DrawMenuItem:
	ld de,-7
	add hl,de
	ld de,(hl)
	inc hl
	inc hl
	inc hl
	ld d,(hl)
	inc hl
	ld e,(hl)
	call GetDataSection
	ld de,9
	add hl,de
	ld b,(hl)
	inc hl
	jp _VPutSN
	
	; In: HLU = HFP, H = VFP, L = LcdTiming2
	; Out: DEU = old HFP, D = old VFP, E = old LcdTiming2 
GetAndSetLCDTiming:
	ld ix,mpLcdTiming0
	ld de,(ix)
	ld d,(ix+6)
	ld e,(ix+8)
	
	; In: HLU = HFP, H = VFP, L = LcdTiming2
SetLCDTiming:
	ld ix,mpLcdTiming0
	ld (ix+8),l
	ld (ix+6),h
	ld l,(ix)
	ld h,(ix+1)
	ld (ix),hl
	ret
	
menuFrame:
	.dl 0
menuSelection:
	.dl 0
lastROM:
	.dl 0
	
StartText:
	.db "Starting!\n",0
	
ROMName:
	.db appVarObj
	.block 9
	
MetaHeader:
	.db "TIBOYCE",0
	
rom_start:
	.dl 0
rom_bank_base:
	.dl 0
cram_start:
	.dl 0
cram_bank_base:
	.dl 0
mbc:
	.db 0
	
hmem_init:
	.db 0,0,0,0,0,$00,$00,$00,0,0,0,0,0,0,0,0
	.db $80,$BF,$F3,0,$BF,0,$3F,$00,0,$BF,$7F,$FF,$9F,0,$BF,0
	.db $FF,$00,$00,$BF,$77,$F3,$F1,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,0,$FC,$FF,$FF,$00,$00,0,0,0,0
	.block $b0
	
	#include "opgen.asm"
	#include "ophandler.asm"
	#include "z80mode.asm"
	#include "render.asm"
	#include "text.asm"
	#include "menu.asm"
	
program_end = $+2
ram_size = program_end
