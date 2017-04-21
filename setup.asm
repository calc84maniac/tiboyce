MENU_ITEM_COUNT = 16
	
	.org 0
Startup:
	ld (ArcBase),hl
	call _RunIndicOff
	or a
	sbc hl,hl
	ld (menuFrame),hl
	
RepopulateMenu:
	ld ix,rombankLUT
	push ix
	 ACALL(ROMSearch)
	pop bc
	lea hl,ix
	ld (ROMListEnd),hl
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
	call _ClrLCDFull
	ld a,32
	ld (penRow),a
	ld hl,91
	ld (penCol),hl
	ld hl,$0031
	ld.sis (drawFGColor-ramStart),hl
	APTR(EmulatorTitle)
	call _VPutS
	or a
	sbc hl,hl
	ld.sis (drawFGColor-ramStart),hl
	ld c,l
RedrawMenu:
	ld de,(menuFrame)
	ld a,36
	ld b,MENU_ITEM_COUNT
RedrawMenuLoop:
	add a,12
	ld (penRow),a
	ld hl,(ROMListEnd)
	sbc hl,de
	jr z,RedrawMenuLoopEnd
	ld hl,(menuSelection)
	sbc hl,de
	jr z,RedrawMenuSelectedItem
	xor a
	ld hl,(menuLastSelection)
	sbc hl,de
	jr z,RedrawMenuItem
	or c
	jr z,RedrawMenuItem
	jr RedrawMenuSkipItem
RedrawMenuSelectedItem:
	set textInverse,(iy+textFlags)
RedrawMenuItem:
	; Carry is clear
	sbc hl,hl
	ld (penCol),hl
	push bc
	 push de
	  ex de,hl
	  ld hl,(hl)
	  ACALL(DrawMenuItem)
	 pop de
	pop bc
	res textInverse,(iy+textFlags)
RedrawMenuSkipItem:
	inc de
	inc de
	inc de
	ld a,(penRow)
	djnz RedrawMenuLoop
	
RedrawMenuLoopEnd:
	ld hl,(menuSelection)
	ld (menuLastSelection),hl
	
SelectionLoop:
	halt
	call _GetCSC
	ld hl,(menuSelection)
	ld de,(ROMListEnd)
	ld bc,MENU_ITEM_COUNT*3
	sub 1
	jr z,MenuDown
	dec a
	jr z,MenuLeft
	dec a
	jr z,MenuRight
	dec a
	jr z,MenuUp
	cp 9-4
	jr z,MenuEnterTrampoline
	cp 54-4
	jr z,MenuEnterTrampoline
	cp 15-4
	jr nz,SelectionLoop
	jr RestoreHomeScreen
	
MenuEnterTrampoline:
	AJUMP(MenuEnter)
	
MenuDown:
	inc hl
	inc hl
	inc hl
	sbc hl,de
	jr z,SelectionLoop
	add hl,de
	ld (menuSelection),hl
	ld a,(menuFrame)
	sub l
	add a,c
	jr z,RedrawMenuClearTrampoline
RedrawMenuTrampoline:
	AJUMP(RedrawMenu)
	
MenuLeft:
	ld de,rombankLUT
	sbc hl,de
	sbc hl,bc
	jr nc,_
	sbc hl,hl
_
	adc hl,de
	ld (menuSelection),hl
	jr c,RedrawMenuTrampoline
	ld hl,(menuFrame)
	sbc hl,bc
	jr RedrawMenuClearTrampoline
	
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
	ld de,-(MENU_ITEM_COUNT-1)*3
	add hl,de
	jr nz,RedrawMenuTrampoline
RedrawMenuClearTrampoline:
	ld (menuFrame),hl
	AJUMP(RedrawMenuClear)
	
MenuRight:
	add hl,bc
	sbc hl,de
	jr c,_
	ld hl,-3
_
	add hl,de
	ld (menuSelection),hl
	ld hl,(menuFrame)
	add hl,bc
	sbc hl,de
	jr nc,RedrawMenuTrampoline
	add hl,de
	jr RedrawMenuClearTrampoline
	
RestoreHomeScreen:
	ld hl,pixelShadow
	ld de,pixelShadow+1
	ld bc,(8400*3) - 1
	ld (hl),0
	ldir
	set graphDraw,(iy+graphFlags)
	call _DrawStatusBar
	call _HomeUp
	ei
	jp _ClrLCDFull
	
MenuEnter:
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
	
	ACALL_SAFERET(StartROM)
	jr c,RestoreHomeScreen
	AJUMP(RepopulateMenu)
	
StartROM:
	ACALL_SAFERET(LoadROM)
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
	jr c,mbc_valid
	;MBC5
	sub $19-$0F
	cp $1F-$19
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
	
	ACALL(LoadRAM)
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
	ld hl,$000801
	ld (mpIntEnable),hl
	ld hl,$000011
	ld (mpIntLatch),hl
	
	ld hl,(mpLcdBase)
	push hl
	ld hl,(mpLcdCtrl)
	push hl
	ld hl,$463B01
	ACALL(GetAndSetLCDTiming)
	push de
	
	APTR(palettecode)
	ld de,mpLcdPalette
	ld bc,palettecodesize
	ldir
	
	ld hl,$0C25
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
	APRINTF(StartText)
#endif
	
	ld a,3
	ld (mpKeypadScanMode),a
	inc a
	ld (mpLcdImsc),a
	
	ld (saveSP),sp
	
	ld sp,myADLstack
	
	ld hl,z80codebase
	push hl
	pop de
	inc de
	ld bc,$00FEFF
	ld (hl),l
	ldir
	push de
	 APTR(hmem_init)
	pop de
	inc b
	ldir
	
	APTR(cursorcode)
	ld de,mpLcdCursorImg
	ld bc,cursorcodesize
	ldir
	
	APTR(z80code)
	ld de,z80codebase
	ld bc,z80codesize
	ldir
	
	APTR(flags_lut_init)
	ld de,z80codebase + flags_lut
	inc b
	ldir
	
	APTR(CmdExit)
	ld (z80codebase+CmdExitSMC),hl
	
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
	
	ld hl,fake_tile
	push hl
	pop de
	inc de
	ld (hl),$FF
	ld c,64
	ldir
	ld (hl),b
	ld a,(fake_tile - vram_pixels_start) >> 8
	ld (de),a
	inc de
	ld c,40
	ldir
	
	ACALL(generate_digits)
	
	ld a,z80codebase >> 16
	ld mb,a
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET
	push.s hl
	
	ld hl,(rombankLUT+3)
	ld (rom_bank_base),hl
	
	ld a,(mbc)
	ld (z80codebase+mbc_z80),a
	
	ld a,(ram_size+1)
	and $80
	add a,$B7	; or a / scf
	ld (z80codebase+ram_size_smc),a
	
	ld hl,(cram_start)
	ld bc,-$A000
	add hl,bc
	ld (z80codebase+cram_base_0),hl
	ld (cram_bank_base),hl
	
	call prepare_next_frame
	call update_palettes
	
	call flush_code_reset_padding
	
	ACALL(IdentifyDefaultPalette)
	ACALL(ApplyConfiguration)
	
	ld de,$0100
	call lookup_code
	
_
	ld a,(mpIntRawStatus)
	bit 4,a
	jr z,-_
	
	ld a,1
	ld (z80codebase+curr_rom_bank),a
	ld iy,0
	ld bc,$0013
	ld de,$00D8
	ld hl,$014D
	exx
	ld hl,$FFFE
	ld bc,(CALL_STACK_DEPTH+1)*256
	jp set_gb_stack
	
CmdExit:
	ld sp,(saveSP)
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
	ld a,2
	ld (mpKeypadScanMode),a
	xor a
	ld (mpLcdImsc),a
	pop hl
	ACALL(SetLCDTiming)
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
	jr z,_
	AJUMP(RestartFromHere)
_
	push af
	 ACALL(RestoreHomeScreen)
	 ACALL_SAFERET(SaveRAM)
	pop af
	ret
	
LoadROM:
	ld hl,ROMName
	ACALL(LookUpAppvar)
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
	 ACALL_SAFERET(LookUpAppvarForceARC)
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
	 ACALL(LookUpAppvar)
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
	ACALL(LookUpAppvar)
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
	call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	ret
	
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
	call Arc_Unarc_Safe
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
	call Arc_Unarc_Safe
	jr -_
_
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	or a
	ret
	
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
	 ACALL(GetDataSection)
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
	ACALL(GetDataSection)
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
	
IdentifyDefaultPalette:
	ld ix,(rom_start)
	ld bc,$0130
	add ix,bc
	dec b	;B=0
	ld a,(ix-$30+$4B)	; Old Licensee Code
	cp $33
	jr nz,old_licensee
	ld a,(ix-$30+$44)	; New Licensee Code byte 1
	cp c
	jr nz,no_special_palette
	ld a,(ix-$30+$45)	; New Licensee Code byte 2
	sub c
old_licensee:
	dec a
	jr nz,no_special_palette
	lea hl,ix-$30+$34	; Game title
	ld b,$10
title_checksum_loop:
	add a,(hl)
	inc hl
	djnz title_checksum_loop
	APTR(DefaultPaletteChecksumTable)
	ld c,$4F
	cpir
	jr nz,no_special_palette
	ld a,$4F-1
	sub c
	cp $41
	jr c,default_palette_found
	dec hl
	ld c,$4F-$41
title_second_checksum_loop:
	ld e,a
	add hl,bc
	ld a,(ix-$30+$37)	; 4th character of game title
	cp (hl)
	ld a,e
	jr z,default_palette_found
	add a,c
	cp $5E
	jr c,title_second_checksum_loop
	
no_special_palette:
	xor a
default_palette_found:
	ld c,a
	APTR(DefaultPaletteIndexTable)
	add hl,bc
	ld a,(hl)
	ld (default_palette),a
	ret
	
hmem_init:
	.db 0,0,0,0,0,$00,$00,$00,0,0,0,0,0,0,0,0
	.db $80,$BF,$F3,0,$BF,0,$3F,$00,0,$BF,$7F,$FF,$9F,0,$BF,0
	.db $FF,$00,$00,$BF,$77,$F3,$F1,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,0,$FC,$FF,$FF,$00,$00,0,0,0,0
	.block $b0
	
flags_lut_init:
	.db $00,$00,$00,$00,$00,$00,$00,$00, $00,$10,$40,$50,$00,$10,$40,$50
	.db $01,$01,$01,$01,$01,$01,$01,$01, $20,$30,$60,$70,$20,$30,$60,$70
	.db $10,$10,$10,$10,$10,$10,$10,$10, $00,$10,$40,$50,$00,$10,$40,$50
	.db $11,$11,$11,$11,$11,$11,$11,$11, $20,$30,$60,$70,$20,$30,$60,$70
	.db $02,$02,$02,$02,$02,$02,$02,$02, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $03,$03,$03,$03,$03,$03,$03,$03, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $12,$12,$12,$12,$12,$12,$12,$12, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $13,$13,$13,$13,$13,$13,$13,$13, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $40,$40,$40,$40,$40,$40,$40,$40, $00,$10,$40,$50,$00,$10,$40,$50
	.db $41,$41,$41,$41,$41,$41,$41,$41, $20,$30,$60,$70,$20,$30,$60,$70
	.db $50,$50,$50,$50,$50,$50,$50,$50, $00,$10,$40,$50,$00,$10,$40,$50
	.db $51,$51,$51,$51,$51,$51,$51,$51, $20,$30,$60,$70,$20,$30,$60,$70
	.db $42,$42,$42,$42,$42,$42,$42,$42, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $43,$43,$43,$43,$43,$43,$43,$43, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $52,$52,$52,$52,$52,$52,$52,$52, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $53,$53,$53,$53,$53,$53,$53,$53, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	
DefaultPaletteChecksumTable:
	.db $00,$88,$16,$36,$D1,$DB,$F2,$3C,$8C,$92,$3D,$5C,$58,$C9,$3E,$70
	.db $1D,$59,$69,$19,$35,$A8,$14,$AA,$75,$95,$99,$34,$6F,$15,$FF,$97
	.db $4B,$90,$17,$10,$39,$F7,$F6,$A2,$49,$4E,$43,$68,$E0,$8B,$F0,$CE
	.db $0C,$29,$E8,$B7,$86,$9A,$52,$01,$9D,$71,$9C,$BD,$5D,$6D,$67,$3F
	.db $6B,$B3,$46,$28,$A5,$C6,$D3,$27,$61,$18,$66,$6A,$BF,$0D,$F4
SecondPaletteChecksumTable:
	.db $42,$45,$46,$41,$41,$52,$42,$45,$4B,$45,$4B,$20,$52,$2D
	.db $55,$52,$41,$52,$20,$49,$4E,$41,$49,$4C,$49,$43,$45,$20
	.db $52
	
DefaultPaletteIndexTable:
	; Dummy entry was formerly $7C, but grayscale is better
	.db $16,$08,$12,$A3,$A2,$07,$87,$4B,$20,$12,$65,$A8,$16
	.db $A9,$86,$B1,$68,$A0,$87,$66,$12,$A1,$30,$3C,$12,$85
	.db $12,$64,$1B,$07,$06,$6F,$6E,$6E,$AE,$AF,$6F,$B2,$AF
	.db $B2,$A8,$AB,$6F,$AF,$86,$AE,$A2,$A2,$12,$AF,$13,$12
	.db $A1,$6E,$AF,$AF,$AD,$06,$4C,$6E,$AF,$AF,$12,$7C,$AC
	.db $A8,$6A,$6E,$13,$A0,$2D,$A8,$2B,$AC,$64,$AC,$6D,$87
	.db $BC,$60,$B4,$13,$72,$7C,$B5,$AE,$AE,$7C,$7C,$65,$A2
	.db $6C,$64,$85