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
	jr nz,_
	set textInverse,(iy+textFlags)
_
	ld hl,(ix)
	ACALL(DrawMenuItem)
	res textInverse,(iy+textFlags)
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
	jr z,RedrawMenuClearTrampoline
RedrawMenuTrampoline:
	AJUMP(RedrawMenu)
	
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
	ld de,-19*3
	add hl,de
	jr nz,RedrawMenuTrampoline
RedrawMenuClearTrampoline:
	ld (menuFrame),hl
	AJUMP(RedrawMenuClear)
	
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
	
	ACALL(StartROM)
	jr c,RestoreHomeScreen
	AJUMP(RepopulateMenu)
	
RestoreHomeScreen:
	ld hl,pixelShadow
	ld de,pixelShadow+1
	ld bc,(8400*3) - 1
	ld (hl),0
	ldir
	set graphDraw,(iy+graphFlags)
	call _DrawStatusBar
	call _HomeUp
	jp _ClrLCDFull
	
StartROM:
	ACALL(LoadROM)
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
	ld hl,$000803
	ld (mpIntEnable),hl
	ld hl,$000017
	ld (mpIntLatch),hl
	
	ld hl,(mpLcdBase)
	push hl
	ld hl,(mpLcdCtrl)
	push hl
	ld hl,$463B01
	ACALL(GetAndSetLCDTiming)
	push de
	
	APTR(palettecode)
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
	APRINTF(StartText)
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
	push de
	 APTR(hmem_init)
	pop de
	inc b
	ldir
	
	APTR(cursorcode)
	ld de,cursormem
	ld bc,cursorcodesize
	ldir
	
	APTR(z80code)
	ld de,z80codebase
	ld bc,z80codesize
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
	
	ld hl,(cram_start)
	ld bc,-$A000
	add hl,bc
	ld (z80codebase+cram_base_0),hl
	ld (cram_bank_base),hl
	
	call prepare_next_frame
	call update_palettes
	
	call flush_code_reset_padding
	
	ACALL(ApplyConfiguration)
	
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
	xor a
	ld (mpLcdImsc),a
	pop hl
	ld (mpTimerCtrl),hl
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
	ei
	push af
	 ACALL(SaveRAM)
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
	 ACALL(LookUpAppvarForceARC)
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
	
hmem_init:
	.db 0,0,0,0,0,$00,$00,$00,0,0,0,0,0,0,0,0
	.db $80,$BF,$F3,0,$BF,0,$3F,$00,0,$BF,$7F,$FF,$9F,0,$BF,0
	.db $FF,$00,$00,$BF,$77,$F3,$F1,0,0,0,0,0,0,0,0,0
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,0,$FC,$FF,$FF,$00,$00,0,0,0,0
	.block $b0
	