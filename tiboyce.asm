#ifdef DEBUG
#ifndef CEMU
#define DBGNOSCALE
#endif
#endif

; Some configuration options
#ifndef ROMNAME
#define ROMNAME "PokeRd00"
#endif
#ifndef SCANDELAY
#define SCANDELAY 10
#endif
#ifndef FRAMESKIP
#define FRAMESKIP 0
#endif

; Some standalone equates
_sprintf = $0000BC
_GetCSC = $02014C
_Mov9ToOP1 = $020320
_chkFindSym = $02050C
_RunIndicOff = $020848
_createAppVar = $021330
_Delvar = $021438
_Arc_Unarc = $021448
_ChkInRAM = $021F98
pixelShadow = $D031F6
userMem = $D1A881
vRam = $D40000
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

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

cursormem = $E30800

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
	jr z,mbc_valid
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
	ret nc
mbc_valid:
	ld a,b
	ld (mbc),a
	
	call LoadRAM
	ret c
	
	di
	push iy
	ld hl,($F00004)
	push hl
	ld hl,($F0000C)
	push hl
	ld hl,$000003
	ld ($F00004),hl
	ld ($F0000C),hl
	
	ld hl,($E30010)
	push hl
	ld hl,($E30018)
	push hl
	
	ld hl,$E30200
	push hl
	pop de
	inc de
	ld bc,16*2-1
	ld (hl),l
	ldir
	ld hl,$FFFF
	ld ($E3021C),hl
	
	ld hl,$0D25
	ld ($E30018),hl
	ld hl,gb_frame_buffer_1
	ld ($E30010),hl
	push hl
	pop de
	inc de
#ifdef DBGNOSCALE
	ld bc,160*144
	ld (hl),0
	ldir
	ld bc,160*96
	ld (hl),$FF
	ldir
	ld bc,160*144
	ld (hl),0
	ldir
	ld bc,160*96-1
	ld (hl),$FF
	ldir
#else
	ld bc,320*240-1
	ldir
#endif
	
#ifdef DEBUG
	ld hl,StartText
	push hl
	 call printf
	pop hl
#endif
	
	ld a,3
	ld ($F50000),a
	
	ld hl,($F20030)
	push hl
	xor a
	sbc hl,hl
	ld ($F20030),hl
	ld ($F20008),hl
	ld ($F2000B),a
	ld ($F2000C),hl
	ld ($F2000F),a
	ld hl,SCANDELAY*256
	ld ($F20000),hl
	ld ($F20003),a
	ld ($F20004),hl
	ld ($F20007),a
	
	ld (saveSP),sp
	
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
	pop ix
	lea de,ix+1
	ld (hl),$FF
	ld bc,174*3-1
	ldir
	
	push bc
	pop hl
	ld c,160
	ld a,144/3
_
	ld (ix+(15*3)),hl
	add hl,bc
#ifndef DBGNOSCALE
	add hl,bc
#endif
	ld (ix+(16*3)),hl
	add hl,bc
	ld (ix+(17*3)),hl
	add hl,bc
#ifndef DBGNOSCALE
	add hl,bc
#endif
	lea ix,ix+9
	dec a
	jr nz,-_
	
	call generate_digits
	
	ld a,z80codebase >> 16
	ld mb,a
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET
	push.s hl
	
	ld hl,(rombankLUT+3)
	ld (rombankLUT),hl
	ld (rom_bank_base),hl
	
	ld a,(mbc)
	ld (z80codebase+mbc_z80),a
	
	ld hl,(cram_start)
	ld bc,-$A000
	add hl,bc
	ld (z80codebase+cram_base_0),hl
	ld (cram_bank_base),hl
	
	call update_palettes
	
	call flush_code
	
	ld de,$0100
	call lookup_code
	
	ld a,5
	ld ($F20030),a
	
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	
	
	ld a,1
	ld (z80codebase+curr_rom_bank),a
	ld bc,$0013
	ld de,$00D8
	ld hl,$014D
	ld iy,$FFFE
	jp set_gb_stack
	
exit:
saveSP = $+1
	ld sp,0
	ld a,$D0
	ld mb,a
	pop hl
	ld ($F20030),hl
	pop hl
	ld ($E30018),hl
	pop hl
	ld ($E30010),hl
	pop hl
	ld ($F0000C),hl
	pop hl
	ld ($F00004),hl
	pop iy
	ei
	ret
	
LoadROM:
	ld ix,rombankLUT
LoadROMLoop:
	ld hl,ROMName
	call LookUpAppvarForceARC
	ret c
	ld de,$4000
	sbc hl,de
	dec bc
	ld a,b
	inc bc
_
	ld (ix),hl
	lea ix,ix+3
	add hl,de
	sub d
	jr nc,-_
	ld a,b
	sub $C0
	or c
	ret nz
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
	ld (hl),'0'
	dec hl
	inc (hl)
	jr LoadROMLoop
	
LoadRAM:
	ld hl,$FF0000
	ld (cram_start),hl
	ld hl,ROMName
	ld bc,9
	xor a
	cpir
	dec hl
_
	dec hl
	ld a,(hl)
	sub '0'
	cp 10
	jr c,-_
	inc hl
	ld (hl),0
	
	ld de,8*1024
	ld a,(mbc)
	cp 2
	jr z,_
	ld hl,(rom_start)
	ld bc,$0149
	add hl,bc
	ld a,(hl)
	or a
	ret z
	cp 3
	jr c,_
	ld de,32*1024
_
	
	ld (ram_size),de
	
_
	push de	
	 ld hl,ROMName
	 call LookUpAppvarForceRAM
	pop de
	ex de,hl
	jr nc,_
	push hl
	 call _createAppVar
	pop de
	jr -_
_
	or a
	sbc hl,bc
	scf
	ret nz
	
	ex de,hl
	ld (cram_start),hl
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
	

	
StartText:
	.db "Starting!\n",0
	
ROMName:
	.db appVarObj,ROMNAME,0
	
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
ram_size:
	.dl 0
	
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