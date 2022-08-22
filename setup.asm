MENU_ITEM_COUNT = 15
	
Startup:
	ld bc,-arc_start
	add hl,bc
	ld (ArcBase),hl
	
	; Get the calculator type from the OS header
	ld hl,_OSHeader
	call _GetFieldSizeFromType
	ld de,$80C0
	call _FindField
	; If not found, calcType stays at default 0 (84+CE)
	jr nz,_
	call _GetFieldSizeFromType
	ld a,(hl)
	inc hl
	and (hl)
	ld (calcType),a
_
	
	call _RunIndicOff
	ACALL(LoadConfigFile)
	
CalculateEpochLoop:
	; Grab current day count
	ld hl,(mpRtcDayCount)
	push hl
	
	 ; Multiply year by 365
	 ld hl,(osYear)
	 push hl
	 pop de
	 ld b,(365-1) / 2
_
	 add hl,de
	 add hl,de
	 djnz -_
	
	 ; Add in day-1
	 ld a,(osDay)
	 ld c,a
	 dec.s bc
	 add hl,bc
	
	 ; If we're Feb. or earlier, don't count this leap year
	 ld a,(osMonth)
	 cp 3
	 jr nc,_
	 dec de
_
	
	 ; Add in months
	 ld ix,monthLUT
_
	 ld c,(ix)
	 inc ix
	 add hl,bc
	 dec a
	 jr nz,-_
	
	 ; Add in year/4
	 srl d
	 rr e
	 srl d
	 rr e
	 add hl,de
	
	 ; Subtract out year/100
	 ex de,hl
	 ld a,25
	 call _DivHLByA
	 ex de,hl
	 or a
	 sbc hl,de
	
	 ; Add in year/400
	 srl d
	 rr e
	 srl d
	 rr e
	 add hl,de
	
	 ; Subtract out January 1, 1970
	 ld bc,-719527
	 add hl,bc
	 jr c,_
	 sbc hl,hl
_
	 ex de,hl
	
	pop bc
	; Make sure day count matches
	ld hl,(mpRtcDayCount)
	or a
	sbc hl,bc
	jr nz,CalculateEpochLoop
	ex de,hl
	sbc hl,bc
	ld (epochDayCount),hl
	
	ACALL(BackupOriginalHardwareSettings)
	; Backup original LCD settings
	ld hl,mpLcdTiming0
	ld de,originalLcdSettings
	ld bc,12
	ldir
	ld hl,(mpLcdCtrl)
	ex de,hl
	ld (hl),de
	
	; Reinitialize hardware, this is slightly slow but should return SPI to a
	; known state on Python Edition models.
	call _boot_InitializeHardware
	
	ld hl,GlobalErrorHandler
	call _PushErrorHandler
	
NoRomMenuLoop:
	; Set custom hardware settings
	ACALL(SetCustomHardwareSettings)
	
	; Set current description and main menu selection
	APTR(NoRomLoadedDescription)
	ld (current_description),hl
	ld a,7
	ld (main_menu_selection),a
	
	; Reset ROM name, state index, and selected config
	xor a
	ld (ROMName+1),a
	ld (current_state),a
	ld (current_config),a
	; Start with Load ROM menu
	inc a
	ACALL(emulator_menu)

	; Check for deleting a ROM
	ex af,af'
	cp 5
	jr z,DeleteROM
	; If not loading a new ROM, exit
	dec a
	jr nz,SaveConfigAndQuit
	
	ACALL(RestoreOriginalHardwareSettings)
LoadNewGameLoop:
	; Copy the name from ROMNameToLoad
	ld hl,ROMName
	push hl
	pop de
	ld bc,ROMNameToLoad - ROMName
	add hl,bc
	ldir
	; Switch to per-game config editing
	ld a,1
	ld (current_config),a
	
	ACALL_SAFERET(StartROM)
	; If NC is returned, we're loading another game
	jr nc,LoadNewGameLoop
	; If an error code of 0 is returned, exit
	or a
	jr z,SaveConfigAndQuit
	; Display the error, and return to the menu if ON wasn't pressed
	ACALL(DisplayError)
NoRomMenuLoopTrampoline:
	jr nz,NoRomMenuLoop
	
SaveConfigAndQuit:
	call _PopErrorHandler
SaveConfigAndQuitForError:
	ACALL(RestoreOriginalHardwareSettings)
	ACALL_SAFERET(SaveConfigFile)
	ld a,(brightness)
	ld (mpBlLevel),a
RestoreHomeScreen:
	; Set all palette entries to white to smooth the transition
	ld hl,mpLcdPalette
	push hl
	pop de
	inc de
	ld bc,$01FF
	ld (hl),c
	ldir
	; Clear pixelShadow etc.
	ld hl,pixelShadow
	push hl
	pop de
	inc de
	ld (hl),c
	ld bc,(8400*3) - 1
	ldir
	; Mark graph dirty
	set graphDraw,(iy+graphFlags)
	; Restore the frame buffer
	call _ClrLCDFull
	call _DrawStatusBar
	call _HomeUp
	; Change the LCD settings to fullscreen 16-bit
	AJUMP(RestoreOriginalLcdSettings)
	
DeleteROM:
	ACALL(RestoreOriginalHardwareSettings)
	ACALL(DeleteROMFiles)
	or a ; Non-zero
	jr NoRomMenuLoopTrampoline
	
	; Input: HL = insertion point
	;        DE = insertion size
	; Output: Carry set and A=error if error occurred
	;         DE = insertion point
	;         BC = insertion size
InsertMemSafe:
	push hl
	 push de
	  ; Check to make sure there's room for at least 2 VAT entries
	  ld hl,30
	  add hl,de
	  push hl
	   call _MemChk
	  pop de
	  ex de,hl
	  scf
	  sbc hl,de
	  ccf
	  inc hl
	  ld (errorArg),hl
	 pop hl
	pop de
	ld a,ERROR_NOT_ENOUGH_MEMORY
	ret c
	push hl
	 call _InsertMem
	pop bc
	; Resets carry
	ld hl,(asm_prgm_size)
	add hl,bc
	ld (asm_prgm_size),hl
	ret
	
	; Input: HL = filename
	;        DE = location to convert
	;        Note: Location must have size bytes initialized and
	;              the range must end exactly on userMem + (asm_prgm_size),
	;              and also the filename should not already exist.
	; Output: Carry set, HL preserved.
	;         File is created and asm_prgm_size is updated.
ConvertMemToFile:
	push hl
#ifdef DEBUG
	 ; Assert that location + size bytes == userMem + (asm_prgm_size)
	 ex de,hl
	 ld de,(hl)
	 ex de,hl
	 inc.s hl
	 inc hl
	 add hl,de
	 ld bc,(asm_prgm_size)
	 sbc hl,bc
	 ld bc,userMem
	 sbc hl,bc
	 jr nz,$
	pop hl
	push hl
#endif
	 push de
	  call _Mov9ToOP1
	  call _CmpPrgNamLen
	  ld bc,0
	  call _CreatePVar4
	  push hl
	  pop ix
	  ld hl,2
	  add hl,sp
	  ld a,(hl)
	 pop hl
	 ld (ix-5),a
	 ld (ix-4),h
	 ld (ix-3),l
	 ld de,-userMem
	 add hl,de
	 ld (asm_prgm_size),hl
	pop hl
	ret
	
LoadROMAndRAMFailed:
	push af
	 ld hl,game_config_start
	 ACALL(DelMemSafeSizeBytes)
	pop af
	ret
	
StartROM:
	; Insert memory to hold the game config and initialize to default config
	APTR(DefaultGameConfig)
	push hl
	 ld hl,game_config_start
	 ld de,game_config_end - game_config_start
	 ACALL(InsertMemSafe)
	pop hl
	ret c
	ldir
	
	; Look up the game config file and apply it
	ld hl,ROMName
	push hl
	 xor a
_
	 inc hl
	 cp (hl)
	 jr nz,-_
	 ld (hl),'C'
	 inc hl
	 ld (hl),'f'
	 inc hl
	 ld (hl),'g'
	 inc hl
	 ld (hl),a
	pop hl
	ACALL(LookUpAppvar)
	ld de,game_config_start + (FrameskipValue - config_start)
	ACALL(LoadConfigFileAny)
	
	ACALL_SAFERET(LoadROMAndRAMRestoreName)
	jr c,LoadROMAndRAMFailed
	
	xor a
	ld (emulatorMessageText),a
	ld (current_state),a
	ACALL(LoadStateFiles)
	push af
	 ; Build the existing state map
	 ld a,'9'+1
_
	 dec a
	 ld (current_state),a
	 push hl
	  ACALL(GetStateFileName)
	  ACALL(LookUpAppvar)
	 pop hl
	 ccf
	 adc hl,hl
	 ld a,(current_state)
	 cp '0'
	 jr nz,-_
	 ld a,l
	 ld (existing_state_map),a
	 ld a,h
	 and 3
	 ld (existing_state_map+1),a
	pop af
	jr nc,StartROMAutoStateNoError
	
	cp ERROR_NOT_ENOUGH_MEMORY
	jr nz,RestartFromHere
	ld hl,(cram_size)
	ld de,(game_config_end - game_config_start) + 6
	add hl,de
	ex de,hl
	ld hl,game_config_start
	ACALL(DelMemSafe)
OutOfMemoryFinish:
	ld a,ERROR_NOT_ENOUGH_MEMORY
	scf
	ret
	
StartROMAutoStateNoError:
	ld de,AutoStateLoadedMessage
	ACALL(SetEmulatorMessage)
	jr StartFromHere
	
StartROMInitStateOutOfMemory:
	xor a
	ld (current_state),a
	; Temporarily prevent auto state saving because state is clean
	ld (should_auto_save),a
	dec de
	dec de
	ld (de),a
	dec de
	inc a
	ld (de),a
	ld hl,(errorArg)
	push hl
	 ACALL_SAFERET(SaveStateFilesAndGameConfig)
	pop hl
	ld (errorArg),hl
	jr OutOfMemoryFinish
	
RestartFromHere:
	ld hl,save_state_size_bytes
	ld de,(hl)
	ld bc,save_state_size + 1
	ld (hl),bc
	inc hl
	inc hl
	inc hl
	dec de
	dec bc
	push hl
	 push bc
	  ACALL(DelMemSafe)
	 pop de
	pop hl
	ACALL(InsertMemSafe)
	jr c,StartROMInitStateOutOfMemory
	push de
	pop hl
	inc de
	dec bc
	ld (hl),0
	ldir
	
	inc hl
	ld a,(hl)
	dec a
	inc hl
	or (hl)
	jr z,_
	inc hl
	inc hl
	ld (cram_start),hl
_
	
	APTR(regs_init)
	ld de,regs_saved
	ld bc,hmem_init - regs_init
	ldir
	
	ld de,hram_saved + $0100
	ld c,hmem_init_size
	ldir
	
	push de
	pop hl
	dec hl
	ld c,$80 - hmem_init_size + 1
	ldir
	
	ld (hl),c
	ld c,$7F
	ldir
	
StartFromHere:
	ld hl,(save_state_size_bytes)
	ld a,l
	dec a
	or h
	jr z,RestartFromHere
	
	ld a,3
	ld (main_menu_selection),a
	
	ld hl,SkinFileName
	ACALL(LookUpAppvar)
	jr nc,_
	or a
	sbc hl,hl
_
	ld (skin_file_ptr),hl
	
	ACALL(SetCustomHardwareSettings)
	
#ifdef DEBUG
	APRINTF(StartText)
#endif
	
	ld iy,state_start+state_size
	ld (saveSP),sp
	
	ld sp,myADLstack
	
	ld hl,z80codebase
	push hl
	pop de
	inc de
	ld bc,hram_start - z80codebase - 1
	ld (hl),l
	ldir
	ld hl,hram_saved
	ld b,2
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
	
	ld de,overlapped_pixel_index_lut
	push de
	 inc b
	 ldir
	 ex (sp),hl
	 ; Use the index data to sprinkle appropriate overlapped pixels
_
	 ld a,l
	 and %10001000
	 sub 1
	 jr c,_
	 rlca
	 rlca
	 and 3
_
	 ld e,(hl)
	 ld (de),a
	 inc l
	 jr nz,--_
	pop hl
	; Duplicate palette overlapped indices to fill the 256-byte table
	ld de,overlapped_palette_index_lut
_
	ld a,(hl) \ inc hl
	ld (de),a \ inc e
	ld (de),a \ inc e
	ld (de),a \ inc e
	ld (de),a \ inc e
	jr nz,-_
	; Get initial OBP palette indices
	ld hl,(iy-ioregs+OBP0)
	ld e,l
	ld a,(de)
	add a,OBP0_COLORS_START
	ld (overlapped_obp0_palette_index),a
	ld e,h
	ld a,(de)
	add a,OBP1_COLORS_START
	ld (overlapped_obp1_palette_index),a
	
	; The trailing pixel data is always color 0
	scf
	sbc hl,hl
	ld (overlapped_pixel_data + 256),hl
	
	ld hl,convert_palette_LUT
_
	ld (hl),l
	inc l
	jr nz,-_
	
	ld hl,BGP_frequencies
_
	ld (hl),b
	inc l
	jr nz,-_
	dec h
	ld l,BGP_write_queue & $FF
	ld (hl),$FF
	
	; Clear the mini frame backup for a clean initial frame
	ld hl,mini_frame_backup
	push hl
	pop de
	inc de
	ld bc,memroutineLUT - mini_frame_backup - 1
	ld (hl),BG_COLOR_0
	ldir
	
	; Clear memroutineLUT to avoid corrupting z80 code when applying RTC SMC
	xor a
_
	ld (de),a
	inc de
	ld (de),a
	inc de
	djnz -_
	
	ld a,vram_tiles_start >> 16
	ld mb,a
	ld ix,vram_tiles_start + 128
	ld hl,vram_start + $1800
	ld c,$40
	ld a,c
_
	ld b,$20
_
	ld e,(hl)
	inc hl
	ld d,a
	mlt de
	ld.s (ix-128),de
	ld.s (ix-64),de
	bit 5,d
	jr nz,_
	set 6,d
_
	ld.s (ix),de
	ld.s (ix+64),de
	lea ix,ix+2
	djnz --_
	lea ix,ix-64
	inc ixh
	dec c
	jr nz,---_
	
	ld a,z80codebase >> 16
	ld mb,a
	
	APTR(digits_encoded)
	ex de,hl
	ld hl,digits
	ld b,3*11
_
	ld a,(de)
	inc de
	ld c,a
_
	sla c
	sbc a,a
	or BLACK-(1+WHITE)
	add a,1+WHITE
	ld (hl),a
	inc hl
	ld a,l
	and 7
	jr nz,-_
	djnz --_
	
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
	
	ld.sis sp,myz80stack
	ld hl,ophandlerRET  ; Return handler when no cached calls are available
	push.s hl
	ld c,4  ; Cycle count of 4 for default return handler
	push.s bc
	
	; Copy palette conversion code to SHA hardware, if possible
	APTR(sha_code)
	ld de,mpShaData
	ld c,sha_code_size
	call.is try_unlock_sha
	ldir
	jr nz,_
	; If the model is too new, execute from cached Flash instead
	ld c,sha_code_size - sha_code_entry_offset
	sbc hl,bc
	ld (convert_palette_row_smc_1),hl
	ld (convert_palette_row_smc_2),hl
	ld (convert_palette_row_smc_3),hl
_

	ld hl,(rom_start)
	ld (rom_start_smc_1),hl
	ld (z80codebase+rom_start_smc_2),hl
	
	ld a,(iy-state_size+STATE_ROM_BANK)
	ld (z80codebase+curr_rom_bank),a
	ld (z80codebase+rom_bank_check_smc_1),a
	ld c,a
	ld b,3
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld hl,(hl)
	ld (rom_bank_base),hl
	
	ld a,(cram_size+1)
	add a,a
	sbc a,a
	and 3
	ld (z80codebase+cram_size_smc),a
	
	xor a
	ld (exitReason),a
	ld (memroutine_rtc_smc_1),a
	ld a,$18	;JR
	ld (memroutine_rtc_smc_2),a
	
	ld hl,(cram_start)
	ld bc,$A000
	sbc hl,bc
	ld (z80codebase+cram_base_0),hl
	ld (z80codebase+cram_base_1),hl
	ld (z80codebase+cram_base_2),hl
	
	APTR(mbc_impl_code - MBC_IMPL_SIZE)
	ld a,(mbc)
	; For no-MBC, skip the copy (and C=0 as desired)
	cp 1
	jr c,setup_ram_bank_no_rtc_trampoline
	ld e,a
	ld d,MBC_IMPL_SIZE
	; Fold MBC3+RTC into MBC3
	cp 4
	jr c,_
	dec de ; Preserve Z
_
	; Copy the MBC implementation in
	ld b,c
	ld c,d
	mlt de
	add hl,de
	ld de,z80codebase+mbc_impl
	ldir
	
	ld c,a
	ld a,(rom_bank_mask)
	inc de
	ld (de),a ;rom_bank_mask_smc
	
	jr nz,setup_ram_bank_no_rtc_trampoline
	;MBC3+RTC
	
	; Skip over MBC5 impl to get to mbc3rtc_4000_impl
	ld c,MBC_IMPL_SIZE
	add hl,bc
	ld de,z80codebase+mbc_4000_impl
	ld c,mbc3rtc_4000_impl_size
	ldir
	
	ld hl,mbc3rtc_6000
	ld.sis (mbc_6000_smc),hl
	
	; Update rtc_last
	call update_rtc_always
	
	ld ix,save_state_size_bytes - 44
	ld bc,(ix+44)
	add ix,bc
	ld bc,(ix+46)
	add ix,bc
	ld b,10
	ld de,z80codebase+rtc_latched
_
	ld a,(ix)
	ld (de),a
	lea ix,ix+4
	inc de
	djnz -_
	
	; Ignore timestamps in the future
	push ix
	 ACALL(GetUnixTimeStamp)
	 ex (sp),ix
	 ex (sp),hl
	 ld de,(ix)
	 xor a
setup_ram_bank_no_rtc_trampoline:
	 jr nz,setup_ram_bank_no_rtc
	 sbc hl,de
	pop hl
	ld de,(ix+3)
	sbc hl,de
	jr c,_
	
	ld hl,(ix+6)
	ld a,h
	or l
	jr nz,_
	ex de,hl
	ld ix,(ix)
	ACALL(ExtractUnixTimeStamp)
	lea hl,ix
	ld de,(epochDayCount)
	or a
	sbc hl,de
	ld.sis (rtc_last),bc
	ld.sis (rtc_last+2),a
	ld.sis (rtc_last+3),hl
_
	call update_rtc_always
	ld a,(iy-state_size+STATE_RAM_BANK)
	sub 8
	jr c,setup_ram_bank
	ld c,a
	call mbc_rtc_toggle_smc
	ld hl,z80codebase+rtc_latched
	jr setup_ram_bank_any
	
setup_ram_bank_no_rtc:
	dec c
	jr nz,setup_ram_normal
	;MBC1
	ld hl,mbc1_6000
	ld.sis (mbc_6000_smc),hl
	; Check for large ROM
	and $60
	jr z,setup_ram_normal
	ld (z80codebase+mbc1_rom_size_smc),a
	and (iy-state_size+STATE_ROM_BANK)
	ld (de),a ;rom_bank_mask_smc
	dec de
	ld a,$F6 ;OR imm8
	ld (de),a ;mbc1_large_rom_smc
	jr setup_ram_bank
setup_ram_normal:
	ld hl,((mbc_ram - (mbc_4000_impl+2)) << 8) | $18
	ld.sis (mbc_4000_impl),hl
setup_ram_bank:
	ld a,(cram_size+1)
	add a,a
	sbc a,a
	and (iy-state_size+STATE_RAM_BANK)
	and 3
	
	rrca
	rrca
	rrca
	ld b,a
	ld c,0
	ld hl,(z80codebase+cram_base_0)
setup_ram_bank_any:
	add hl,bc
	ld (z80codebase+cram_actual_bank_base),hl
	
	ld b,(iy-state_size+STATE_MBC_MODE)
	; Check for MBC1 mode 0
	bit 0,b
	jr nz,_
	ld a,(mbc)
	dec a
	jr nz,_
	ld a,$20 ;JR NZ (overriding JR)
	ld (z80codebase+mbc1_ram_smc_1),a
	ld a,$30 ;JR NC (overriding JR Z)
	ld (z80codebase+mbc1_ram_smc_2),a
	; Set RAM bank 0 always in this mode
	ld hl,(z80codebase+cram_base_0)
_
	; Disable RAM
	bit 1,b
	jr z,_
	ld hl,mpZeroPage
_
	ld (cram_bank_base),hl
	
	ld de,z80codebase+intstate_smc_2
	ld a,(iy-state_size+STATE_INTERRUPTS)
	; Check value of IME
	rra
	jr c,++_
	; Check if EI delay is active
	rra
	jr nc,_
	ld hl,schedule_ei_delay_startup
	ld.sis (event_counter_checkers_ei_delay),hl
_
	ld a,$08 ;EX AF,AF' (overriding RET)
	ld (z80codebase+intstate_smc_1),a
	xor a
	ld (de),a
_
	
	; Check if CPU is halted
	ld a,(iy-state_size+STATE_CPU_MODE)
	or a
	jr z,++_
	; Adjust intstate_smc_2 and cpu_halted_smc
	ex de,hl
	ld a,(hl)
	;cpu_halted_smc = (JR cpu_continue_halt)
	ld de,((cpu_continue_halt - (cpu_halted_smc + 2)) << 16) | ($18 << 8) | (trigger_interrupt - cpu_exit_halt_trigger_interrupt)
	sub e
	jr nc,_
	xor (cpu_exit_halt_trigger_interrupt - trigger_interrupt) ^ (cpu_exit_halt_no_interrupt - cpu_halted_smc)
_
	ld e,a
	ld (hl),de
_
	
	; Set the initial DIV counter to one cycle in the future
	ld hl,(iy-state_size+STATE_DIV_COUNTER)
	push hl
	 inc hl
	 ld i,hl
	 ex de,hl
	
	 ; Calculate the LYC cycle offset (from vblank)
	 ld a,(iy-ioregs+LYC)
	 ; Special case for line 0, thanks silly PPU hardware
	 ld hl,-(CYCLES_PER_SCANLINE * 9 + 1)
	 ld c,1-CYCLES_PER_SCANLINE
	 or a
	 jr z,++_
	 ld h,-CYCLES_PER_SCANLINE
	 add a,10
	 ld l,a
	 ; Wrap vblank lines to the 0-9 range
	 daa
	 jr nc,_
	 ld l,a
	 ; Set scanline length to -1 for line 153, or -CYCLES_PER_SCANLINE otherwise
	 add a,-9
	 sbc a,a
	 or h
	 ld c,a
_
	 ; Multiply by -CYCLES_PER_SCANLINE
	 ; Note that this produces 0 cycles for LYC=144, but the cycle offset is not used
	 ; in that particular case (vblank collision is special-cased)
	 xor a
	 sub l
	 mlt hl
	 ; This should always reset carry
	 add a,h
	 ld h,a
_
	 ld (lyc_cycle_offset),hl
	 ld a,c
	 ld (z80codebase+ppu_lyc_scanline_length_smc),a
	
	 ; Get the number of cycles from one cycle in the future until vblank
	 ld hl,CYCLES_PER_SCANLINE * 144
	 ld bc,(iy-state_size+STATE_FRAME_COUNTER)
	 inc.s bc
	 ASSERT_NC
	 sbc hl,bc
	 jr nc,_
	 ld bc,CYCLES_PER_FRAME
	 add hl,bc
_
	 ; Add to the DIV counter
	 add hl,de
	 ld.sis (vblank_counter),hl
	 ld.sis (persistent_vblank_counter),hl
	
	 ; Update LY and STAT caches based on DIV and vblank counters
	 call.is updateSTAT_full_for_setup
	 ; Ensure top bit of STAT is set, for compatibility with old save states
	 or $80
	 ld.s (hl),a
	 ; Update PPU scheduler state based on LY and STAT caches
	 ld c,a
	 call stat_setup_c
	; Restore original DIV counter
	pop bc
	
	; Initialize timer values
	ld a,(iy-ioregs+TAC)
	bit 2,a
	jr z,++_
	and 3
	ld e,a
	ld d,2
	mlt de
	ld hl,timer_smc_data
	add hl,de
	ld a,(hl)
	ld (z80codebase+updateTIMA_smc),a
	inc hl
	ld h,(hl)
	; Calculate the number of cycles in the timer period (divided by 2)
	xor a
	sub (iy-ioregs+TMA)
	ld e,a
	ld d,h
	jr z,_
	mlt de
_
	ld.sis (timer_period),de
	; Calculate the number of cycles until the timer event
	ld a,(iy-ioregs+TIMA)
	cpl
	ld l,a
	ld a,h
	ld (z80codebase+timer_cycles_reset_factor_smc),a
	mlt hl
	add hl,hl
	; Add to the DIV counter
	add hl,bc
	; Factor in the low bits of the DIV counter
	add a,a
	dec a
	or l
	ld l,a
	inc hl
	inc hl
	ld.sis (timer_counter),hl
	ld hl,timer_counter_checker
	ld.sis (event_counter_checker_slot_timer),hl
	ld a,$CD
	ld (z80codebase+enableTIMA_smc),a
_
	
	; Always set the serial counter, which contains an offset from boot time
	ld hl,(iy-state_size+STATE_SERIAL_COUNTER)
	; Add to the DIV counter
	add hl,bc
	ld.sis (serial_counter),hl
	; Set the serial counter check event if needed
	ld a,(iy-ioregs+SC)
	cpl
	and $81
	jr nz,_
	ld hl,serial_counter_checker
	ld.sis (event_counter_checker_slot_serial),hl
_
	
	; Set the next audio frame sequencer counter based on DIV
	; Low byte of this counter is always 0
	ld a,b
	or $0F	; $1F in double speed mode
	inc a
	ld (z80codebase+audio_counter+1),a
	
	lea hl,iy-ioregs+NR10
	ld ix,z80codebase + audio_port_values
	ld b,audio_port_masks - audio_port_values
_
	ld a,(hl)
	ld (ix),a
	or (ix + audio_port_masks - audio_port_values)
	ld (hl),a
	inc hl
	inc ix
	djnz -_
	; Check if audio is disabled in NR52
	bit 7,(hl)
	jr nz,_
	ld a,$C9 ;RET (overriding PUSH AF)
	ld (z80codebase+write_audio_disable_smc),a
_
	
	ld hl,(iy-ioregs+LCDC-2)
	add hl,hl
	ld a,$28 ;JR Z
	jr c,_
	push hl
	 call do_lcd_disable
	pop hl
	ld a,$20 ;JR NZ
_
	ld (LCDC_7_smc),a
	add hl,hl
	add hl,hl
	sbc a,a
	and $38-$30 ;JR C or JR NC
	add a,$30
	ld (LCDC_5_smc),a
	add hl,hl
	sbc a,a
	inc a ;$00 or $80
	rrca
	ld (LCDC_4_smc),a
	ld (window_tile_ptr),a
	add hl,hl
	sbc a,a
	and $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (LCDC_3_smc),a
	add hl,hl
	jr nc,_
	ld a,$78 ;(overriding $38)
	ld (LCDC_2_smc_1),a
	ld a,15 ;(overriding 7)
	ld (LCDC_2_smc_2),a
	ld (LCDC_2_smc_4),a
	ld a,$81 ;RES 0,C (overriding RES 0,B)
	ld (LCDC_2_smc_3),a
	ld a,1 ;(overriding 9)
	ld (LCDC_2_smc_5),a
_
	add hl,hl
	jr c,_
	ld a,$C9 ;RET (overriding LD C,myspriteLY)
	ld (LCDC_1_smc),a
_
	add hl,hl
	sbc a,a
	and $39-$31 ;ADD HL,SP or LD SP,
	add a,$31
	ld (LCDC_0_smc),a
	
	ld hl,(iy-ioregs+SCY)
	ld a,l
	ld (SCY_smc),a
	ld a,h
	rrca
	rrca
	and $3E
	ld (SCX_smc_1),a
	ld a,h
	cpl
	and 7
	inc a
	ld (SCX_smc_2),a
	
	ld hl,(iy-ioregs+WY)
	ld a,l
	ld (WY_smc),a
	ld a,h
	ld (WX_smc_2),a
	cp 167
	inc a
	ld (WX_smc_3),a
	sbc a,a
	and $20-$18 ;JR NZ or JR
	add a,$18
	ld (WX_smc_1),a
	
	ld a,(iy-ioregs+IF)
	and $1F
	ld (z80codebase+active_ints),a
	
	; Prepare initial frame, but skip setting frame and scanline pointers
	; which are handled in SetScalingMode
	call prepare_initial_frame
	
	ACALL(IdentifyDefaultPalette)
	ACALL(ApplyConfiguration)
	ACALL(SetScalingMode)
	
	call flush_code_reset_padding

	; Generate the initial code block
	ld.s hl,(iy-state_size+STATE_REG_PC)
	ld.sis (event_gb_address),hl
	ex de,hl
	call lookup_code
	; Push the target JIT address to the Z80 stack
	push.s ix
	
	; Get the GB registers in BDEHL'
	ld.s bc,(iy-state_size+STATE_REG_BC)
	ld.s de,(iy-state_size+STATE_REG_DE)
	ld.s hl,(iy-state_size+STATE_REG_HL)
	exx
	; Save the block cycle offset of the current instruction to C
	ld c,a
	; Get and remap GB AF
	ld de,(iy-state_size+STATE_REG_AF)
	ld h,flags_lut >> 8
	ld l,e
	ld.s e,(hl)
	push de
	pop af
	; Get GB SP before we destroy IY
	ld.s hl,(iy-state_size+STATE_REG_SP)

	; Set the cycle count for the current event,
	; which will occur in 1 cycle to force a reschedule
	ld iy,$FFFF

	; Set the Game Boy stack, dispatch interrupt if pending,
	; and reschedule events after 1 cycle
	jp.sis start_emulation
	
ExitEmulation:
	ld ix,state_start+state_size
	ld.sis hl,(event_gb_address)
	ld (ix-state_size+STATE_REG_PC),hl
	
	pop hl
	ld.sis de,(sp_base_address_neg)
	add hl,de
	ld.s (ix-state_size+STATE_REG_SP),hl
	
	exx
	push.s hl
	push.s de
	push.s bc
	
	ex af,af'
	push af
	pop hl
	ld h,flags_lut >> 8
	ld.s l,(hl)
	ld h,a
	push.s hl
	
	xor a
	ld (ix-state_size+STATE_SYSTEM_TYPE),a
	ld b,a
	sbc hl,hl
	; Save the active interrupts in IF
	ld.s a,(hl) ;active_ints
	or $E0
	ld (ix-ioregs+IF),a
	add.s hl,sp
	lea de,ix-state_size+STATE_REG_AF
	ld c,8
	ldir.s
	
	; Calculate the DIV cycle count
	ld hl,i
	lea de,iy
	add hl,de
	ex de,hl
	
	; Save the frame-relative cycle count
	ld.sis hl,(vblank_counter)
	ld a,e
	sub l
	ld l,a
	ld a,d
	sbc a,h
	ld h,a
	ld bc,CYCLES_PER_SCANLINE * 144 + $FF0000
_
	add hl,bc
	ld bc,CYCLES_PER_FRAME
	jr nc,-_
	ld (ix-state_size+STATE_FRAME_COUNTER),hl
	
	; Save the serial cycle count
	ld.sis hl,(serial_counter)
	or a
	sbc hl,de
	ld (ix-state_size+STATE_SERIAL_COUNTER),hl
	
	; Save the DIV cycle count
	ld (ix-state_size+STATE_DIV_COUNTER),de
	
	; Save the actual value of TIMA if the timer is running
	ld a,(ix-ioregs+TAC)
	and 4
	jr z,+++_
	ld.sis hl,(timer_counter)
	dec hl
	ex de,hl
	sbc hl,de
	ld a,(z80codebase+updateTIMA_smc)
	sub 6
	jr z,++_
_
	add hl,hl
	inc a
	jr nz,-_
_
	ld (ix-ioregs+TIMA),h
_
	
	; Save the interrupt state: bit 0 = IME, bit 1 = delayed EI
	ld hl,(z80codebase+intstate_smc_2)
	ld a,l
	cp cpu_exit_halt_no_interrupt - intstate_smc_2
	ld a,(z80codebase+intstate_smc_1)
	rla
	dec a
	and 3
	ld (ix-state_size+STATE_INTERRUPTS),a
	
	; Set CPU mode, 0 = running, 1 = halted
	xor a
	add hl,hl
	rla
	ld (ix-state_size+STATE_CPU_MODE),a
	
	ld a,(z80codebase+curr_rom_bank)
	ld (ix-state_size+STATE_ROM_BANK),a
	
	; Save the current MBC mode: bit 0 = MBC1 mode, bit 1 = RAM disable
	ld a,(cram_bank_base+2)
	add a,1
	sbc hl,hl
	ld a,(z80codebase+mbc1_ram_smc_1)
	sub $20
	ld l,a
	add hl,hl
	ld a,h
	and 3
	ld (ix-state_size+STATE_MBC_MODE),a
	
	; Save the currently mapped RAM bank
	ld a,(z80codebase+cram_actual_bank_base+2)
	cp z80codebase >> 16
	jr nz,_
	ld a,(z80codebase+cram_actual_bank_base)
	sub (rtc_latched-8)&$FF
	jr ++_
_
	ld a,(z80codebase+cram_actual_bank_base+1)
	ld hl,(z80codebase+cram_base_0)
	sub h
	rlca
	rlca
	rlca
	and 3
_
	ld (ix-state_size+STATE_RAM_BANK),a
	
	; Save the actual audio port values in the audio port space
	ld hl,z80codebase + audio_port_values
	lea de,ix-ioregs+NR10
	ld bc,audio_port_masks - audio_port_values
	ldir
	
	; Zero-fill the rest of the state
	lea hl,ix-state_size+STATE_END
	lea de,ix-state_size+STATE_END+1
	ld c,state_size-STATE_END-1
	ld (hl),b
	ldir
	
	; Copy into the state file
	ld l,c
	ld de,hram_saved
	ld b,$0200 >> 8
	ldir
	
ExitEmulationWithoutState:
	; Handle RTC saving
	ld a,(cram_size)
	or a
	jr z,++_
	call update_rtc
	ld ix,save_state_size_bytes - 44
	ld bc,(ix+44)
	add ix,bc
	ld bc,(ix+46)
	add ix,bc
	sbc hl,hl
	ld de,z80codebase+rtc_latched
	ld b,10
_
	ld a,(de)
	ld (ix),a
	ld (ix+1),hl
	inc de
	lea ix,ix+4
	djnz -_
	push ix
	 ACALL(GetUnixTimeStamp)
	 ex de,hl
	pop hl
	ld (hl),ix
	inc hl
	inc hl
	inc hl
	ld (hl),de
	inc hl
	inc hl
	inc hl
	xor a
	ld (hl),a
	inc hl
	ld (hl),a
_
	ld sp,(saveSP)
	ACALL(RestoreOriginalHardwareSettings)
	ld a,(exitReason)
NewExitReason:
	dec a
	srl a
	jr z,ExitDone
	jr c,_
	rra
	jr nc,ExitOrDelete
	AJUMP(RestartFromHere)
_
	ld de,StateLoadedMessage
	ld a,(main_menu_selection)
	dec a
	jr z,_
	ACALL_SAFERET(SaveStateFiles)
	; Reindex the ROM in case a Garbage Collect occurred, and reinsert the RAM.
	ACALL_SAFERET(LoadROMAndRAMRestoreName)
	jr c,ExitDone
	ld de,StateSavedMessage
_
	ld hl,(current_state)
	push hl
	 ACALL(SetEmulatorMessage)
	pop hl
	ACALL(LoadStateFiles)
	jr nc,StartFromHereTrampoline
	cp ERROR_NOT_ENOUGH_MEMORY
	jr nz,_
	ld hl,(save_state_size_bytes)
	ld a,l
	dec a
	or h
	jr z,StartFromHereTrampoline
	ld a,ERROR_NOT_ENOUGH_MEMORY
_
	ACALL(DisplayError)
	scf
	jr z,ExitDone
	xor a
	ld (emulatorMessageText),a
StartFromHereTrampoline:
	AJUMP(StartFromHere)
ExitOrDelete:
	cp 1
	jr z,DeleteInGame
	adc a,-1 ; Sets carry
ExitDone:
	push af
	 xor a
	 ld (current_state),a
	 ACALL_SAFERET(SaveStateFilesAndGameConfig)
	pop af
	ret
	
DeleteInGame:
	ACALL(DeleteSelectedFiles)
	ACALL(SetCustomHardwareSettings)
	ld a,(current_menu)
	ACALL(emulator_menu)
	ex af,af'
	push af
	 ACALL(RestoreOriginalHardwareSettings)
	pop af
	or a
	jr z,StartFromHereTrampoline
	AJUMP(NewExitReason)
	
DeleteSelectedFiles:
	ld a,(current_menu)
	or a
	jr nz,DeleteROMFiles
	
	; Update the existing state map
	ld a,(current_state)
	ACALL(GetStateMask)
	xor (hl)
	ld (hl),a
	
	ACALL(GetStateFileName)
	call DelVarByName
	ACALL(GetStateRAMFileName)
	jp DelVarByName
	
DeleteROMFiles:
	ld hl,ROMNameToLoad
	push hl
	 call DelVarByName
	pop hl
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
	push hl
	 inc hl
	 ld (hl),a
_
	 ld hl,ROMNameToLoad
	 call DelVarByName
	pop hl
	push hl
	 inc (hl)
	 ld a,(hl)
	 cp '9'+1
	 jr c,-_
	 ld (hl),'0'
	 dec hl
	 inc (hl)
	 ld a,(hl)
	 cp '9'+1
	 jr c,-_
	pop hl
	ret
	
GetStateMask:
	sbc hl,hl
	add a,1-'0' ; Sets carry
_
	adc hl,hl
	dec a
	jr nz,-_
	ex de,hl
	ld hl,existing_state_map
	or e
	ret nz
	inc hl
	ld a,d
	ret
	
LoadConfigFile:
	ld hl,ConfigFileName
	ACALL(LookUpAppvar)
	ld de,FrameskipValue
LoadConfigFileAny:
	ret c
	
	; Check the version byte
	ld a,(hl)
	inc hl
	dec a
	ret nz
	
	ld bc,0
	ld c,(hl)
	inc hl
	ld a,c
	dec a
	sub option_config_count
	ret nc
	ldir
	
	cpl
	ld c,a
	ex de,hl
	add hl,bc
	ex de,hl
	ld c,(hl)
	inc hl
	ld a,c
	dec a
	cp key_config_count
	ret nc
	inc de
	ldir
	ret

SaveStateFilesAndGameConfig:
	ACALL_SAFERET(SaveStateFiles)
	
	; Generate the config file name
	ld hl,ROMName
	push hl
	 xor a
_
	 inc hl
	 cp (hl)
	 jr nz,-_
	 dec hl
	 ld (hl),'g'
	 dec hl
	 ld (hl),'f'
	 dec hl
	 ld (hl),'C'
	
	 ; Check if the config is still default
	 APTR(DefaultGameConfig)
	 ld de,game_config_start
	 push de
	  ld bc,game_config_end - game_config_start
	  call memcmp
	 pop de
	pop hl
	jr nz,SaveConfigFileAny
	
	; If so, delete the config file and memory
	push de
	 call DelVarByName
	pop de
SaveConfigFileDelMem:
	ex de,hl
	
	; Input: HL = deletion point, pointing to (fake) size bytes
	; Output: Memory is deleted according to size bytes, asm_prgm_size is adjusted
DelMemSafeSizeBytes:
	ld de,(hl)
	inc.s de
	inc de
	
	; Input:  HL = deletion point
	;         DE = deletion size
	; Output: Memory is deleted, asm_prgm_size is adjusted
DelMemSafe:
	push hl
	 ld hl,(asm_prgm_size)
	 or a
	 sbc hl,de
	 ld (asm_prgm_size),hl
	pop hl
	jp _DelMem
	
SaveConfigFile:
	ld hl,ConfigFileName
	ld de,config_start
SaveConfigFileAny:
	push hl
	 push de
	  ACALL(LookUpAppvar)
	 pop de
	 jr c,_
	
	 dec hl
	 dec hl
	 inc bc
	 inc bc
	 push de
	  call memcmp
	 pop de
	pop hl
	jr z,SaveConfigFileDelMem
	push hl
	 push de
	  call DelVarByName
	 pop de
_
	pop hl
	ACALL(ConvertMemToFile)
	AJUMP(ArchiveWithWarning)
	
LoadROMAndRAMRestoreName:
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	dec hl
	dec hl
	ld (hl),a
	
LoadROMAndRAM:
	ld hl,ROMName+1
	ld (errorArg),hl
	ACALL_SAFERET(LoadROM)
	ret c
	
	ld ix,rombankLUT
	ld hl,(ix)
	; Get the end of the loaded ROM banks.
	; If 256 banks were loaded this gets the start instead,
	; but that makes the mirroring copy a no-op as it should be in that case.
	ld e,3
	ld a,d
	mlt de
	add ix,de
	ld b,a
	; Make sure the ROM size is a power of 2 (and at least 2)
	; If not, fill up to the next power of 2 with blank pages
	ld de,mpZeroPage - $4000
	djnz ++_
	; If the page count was 1, always fill at least one page
_
	ld (ix),de
	lea ix,ix+3
	ld b,a
	inc a
_
	tst a,b
	jr nz,--_
	; Save the power of 2 minus 1 as the mask to apply to banks
	dec a
	ld (rom_bank_mask),a

	ld de,$4000
	add hl,de
	ld (rom_start),hl
	ld bc,$0147
	add hl,bc
	ld a,(hl)
	ld c,a
	dec b
	ld (errorArg),bc
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
	inc b
	inc b	;MBC3+RTC
	sub $0F-$05
	sub $11-$0F
	jr c,mbc_valid
	dec b	;MBC3
	cp $14-$11
	jr c,mbc_valid
	inc b
	inc b	;MBC5
	sub $19-$11
	cp $1F-$19
mbc_valid:
	ccf
mbc_valid_no_carry:
	ld a,ERROR_UNSUPPORTED_MBC
	ret c
	ld a,b
	ld (mbc),a
	
LoadRAM:
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	ld (hl),'V'
	dec hl
	ld (hl),'A'
	dec hl
	ld (hl),'S'
	
	ld de,8*1024
	ld a,(mbc)
	cp 2	;MBC2
	jr z,+++_
	cp 4	;MBC3+RTC
	jr nz,_
	ld e,48
_
	ld hl,(rom_start)
	ld bc,$0149
	add hl,bc
	ld a,(hl)
	or a
	jr nz,_
	ld d,a
_
	cp 3
	jr c,_
	ld d,(32*1024) >> 8
_
	
	ld (cram_size),de
	inc de
	push de
	 inc de
	 inc de
	 inc de
	 inc de
	 inc de
	 ld hl,save_state_size_bytes
	 ACALL(InsertMemSafe)
	pop bc
	ret c
	ex de,hl
	; Set save state to uncompressed size 0 initially
	ld de,1
	ld (hl),de
	inc hl
	inc hl
	inc hl
	; Compression type is implicitly set to 0 here
	ld (hl),bc
	
LoadRAMAny:
	ld bc,(cram_size)
	ld a,b
	or c
	ld hl,mpZeroPage
	ld (cram_start),hl
	ret z
	ld hl,decompress_buffer
	ld (hl),l
	push hl
	pop de
	inc de
	dec bc
	ldir
	ld hl,ROMName
	ACALL(LookUpAppvar)
	jr c,LoadRAMNoVar
	ACALL(DecompressFile)

LoadRAMNoVar:
	ld hl,save_state_size_bytes
	ld de,(hl)
	add hl,de
	inc hl
	inc hl
	ld bc,(hl)
	inc hl
	inc hl
	inc hl
	ld (cram_start),hl
	dec bc
	ex de,hl
	ld hl,decompress_buffer
	ldir
	or a
	ret
	
	; Inputs: HL=compressed data
	;         BC=compressed size
	; Output: decompress_buffer contains decompressed data
	;         BC=decompressed size, or 0 on error
DecompressFile:
	ld a,b
	or c
	ret z
	; Only load compression type 0 (uncompressed)
	ld a,(hl)
	cpi
	ret po
	ld de,decompress_buffer
	or a
	jr z,DecompressUncompressed
	dec a
	jr nz,_
	push hl
	 call lzf_decompress
	pop hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ret z
_
	xor a
	ld b,a
	ld c,a
	ret
	
DecompressUncompressed:
	push bc
	 ldir
	pop bc
	ret
	
	; Input: HL=file in user RAM
	; Output: File potentially resized, but not moved
CompressFile:
	push hl
	 ld bc,(hl)
	 inc hl
	 inc hl
	 inc hl
	 dec bc
	 push bc
	  call lzf_compress
	  ex (sp),hl
	 pop bc
	 or a
	 sbc hl,bc
	 ex de,hl
	pop hl
	ret c
	push bc
	 inc bc
	 ld (hl),bc
	 inc hl
	 inc hl
	 ld (hl),1
	 inc hl
	 push hl
	  ACALL(DelMemSafe)
	 pop de
	pop bc
	ld hl,lzf_compress_buffer
	ldir
	ret
	
LoadROM:
	ld hl,ROMName
	ACALL(LookUpAppvar)
	ld a,ERROR_FILE_MISSING
	ret c
	ld a,c
	sub 9
	ld c,a
	jr nc,_
	ld a,b
	or a
	ld a,ERROR_FILE_INVALID
	ret z
	dec b
_
	push bc
	 ld de,MetaHeader
	 ld bc,8
	 call memcmp
	pop bc
	ld a,ERROR_FILE_INVALID
	scf
	ret nz
	ld d,(hl)
	inc hl
	ld (current_description),hl
	
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	ld (hl),'R'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),'0'
	inc hl
	ld (hl),a
	
	ld a,d
	ld hl,rombankLUT
	push hl
	pop de
	inc de
	ld bc,256*3 - 1
	ld (hl),c
	ldir
	ld d,a
	ld e,a
LoadROMLoop:
	push de
	 ld hl,ROMName
	 ACALL(LookUpAppvar)
	pop de
	ld a,ERROR_FILE_MISSING
	ret c
	jr nz,LoadROMPageLoop
	call Arc_Unarc_Safe
	ld a,ERROR_NOT_ENOUGH_ARCHIVE
	ret c
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	dec hl
	dec hl
	ld (hl),a
	jr LoadROM

LoadROMPageLoop:
	ld a,d
	dec a
	cp (hl)
	ld a,ERROR_FILE_INVALID
	ret c
	push de
	 ld e,(hl)
	 ld d,3
	 mlt de
	 ld ix,rombankLUT
	 add ix,de
	 bit 7,(ix+2)
	 scf
	 jr z,_
	 inc hl
	 dec bc
	 push bc
	  ld c,(hl)
	  inc hl
	  ld b,(hl)
	  inc hl
	  ld de,-$4000
	  add hl,de
	  ld (ix),hl
	  add hl,bc
	  sbc hl,de
	  ex (sp),hl
	  inc bc
	  sbc hl,bc
	  ex (sp),hl
	 pop bc
_
	pop de
	ret c
	jr z,_
	dec e
	jr nz,LoadROMPageLoop
	scf
	ret
_
	dec e
	ret z
	ld hl,ROMName
	xor a
_
	inc hl
	cp (hl)
	jr nz,-_
	dec hl
	inc (hl)
	ld a,(hl)
	cp '9'+1
	jr c,LoadROMLoop
	ld (hl),'0'
	dec hl
	inc (hl)
	jr LoadROMLoop
	
GetStateRAMFileName:
	ld hl,ROMName
	push hl
	 xor a
_
	 inc hl
	 cp (hl)
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 ld b,'v'
	 or a
	 jr nz,_
	 ld a,'V'
	 ld b,'A'
_
	 ld (hl),a
	 dec hl
	 ld (hl),b
	 dec hl
	 ld (hl),'S'
	pop hl
	ret
	
GetStateFileName:
	ld hl,ROMName
	push hl
_
	 inc hl
	 ld a,(hl)
	 or a
	 jr nz,-_
	 dec hl
	 ld a,(current_state)
	 or a
	 jr nz,_
	 ld a,'A'
_
	 ld (hl),a
	 dec hl
	 ld (hl),'t'
	 dec hl
	 ld (hl),'S'
	pop hl
	ret

SaveStateFiles:
	ld hl,save_state_size_bytes
	ld de,(hl)
	inc hl
	inc hl
	add hl,de
	push hl
	 ld bc,(hl)
	 ld a,c
	 dec a
	 or b
	 jr z,SaveRAMDeleteMem

	 ACALL(CompressFile)
	pop hl
	push hl
	 ld bc,(hl)
	 inc.s bc
	 inc bc
	 call checksum
	 ld (cart_ram_checksum),ix
	
	 ACALL(GetStateRAMFileName)
	 ACALL(LookUpAppvar) 
	 jr c,SaveRAMRecreate
	
	 ex de,hl
	 ex (sp),hl
	 push hl
	  dec de
	  dec de
	  inc bc
	  inc bc
	  call memcmp
	 pop hl
	 ex (sp),hl
	 jr z,SaveRAMDeleteMem
	
	 ; Delete the existing variable
	 push hl
	 pop ix
	 ld de,(ix-7)
	 ld d,(ix-4)
	 ld e,(ix-3)
	 call _DelVarArc
	
SaveRAMRecreate:
	pop de
	ld hl,ROMName
	ACALL(ConvertMemToFile)
	; Carry is set
	jr SaveState
	
SaveRAMDeleteMem:
	pop hl
	ACALL(DelMemSafeSizeBytes)
	or a
	
SaveState:
	push af
	 ACALL(GetStateFileName)
	 call DelVarByName
	
	 ld hl,(current_state)
	 ld a,l
	 or a
	 jr nz,SaveManualState
	 or h ;should_auto_save
	 jr nz,SaveAutoState
	
SaveAutoStateDeleteMem:
	 ld hl,save_state_size_bytes
	 ACALL(DelMemSafeSizeBytes)
	 
ArchiveSaveRAM:
	pop af
	ret nc

	ACALL(GetStateRAMFileName)
ArchiveWithWarning:
	push hl
	 call _Mov9ToOP1
	 call Arc_Unarc_Safe	; Must be CALL due to special return address handling
	pop hl
	ret nc
	inc hl
	ld (errorArg),hl
	ld a,ERROR_NOT_ENOUGH_ARCHIVE
DisplayWarning:
	APTR(warning_text)
	jr DisplayErrorAny

SaveManualState:
	 ; Update the existing state map
	 ACALL(GetStateMask)
	 or (hl)
	 ld (hl),a
	 
SaveAutoState:
	 ld hl,save_state_size_bytes
	 push hl
	  ACALL(CompressFile)
	 pop de
	 ld hl,ROMName
	 ACALL(ConvertMemToFile)
	 ACALL_SAFERET(ArchiveWithWarning)
	 jr ArchiveSaveRAM

	; A = error code
	; (errorArg) = error argument
	; Returns A=0 and Z flag set if ON was pressed
DisplayError:
	APTR(error_text)
DisplayErrorAny:
	push af
	 push hl
	  ACALL(ClearMenuBuffer)
	 pop hl
	 push bc
	  ld a,WHITE
	  ld bc,1<<8|5
	  ACALL(PutStringColorXY)
	   
	  APTR(error_messages)
	 pop bc
	pop de
	 
	xor a
_
	cpir
	dec d
	jr nz,-_
	
	ex de,hl
	ld hl,(errorArg)
	push hl
	 push de
	  ACALL(PutStringFormat)
	 pop hl
	pop hl
	
	; If already in the menu, just wait for a key
	APTR(WaitForKey)
	push hl
	 ld a,i
	 ret po
	 
	 ; Otherwise, switch to the menu state and back
	 ACALL(SetCustomHardwareSettings)
	 call setup_menu_palette
	pop hl
	call CallHL
	push af
	 ACALL(RestoreOriginalHardwareSettings)
	pop af
	ret
	
LoadStateInvalid:
	ld a,ERROR_FILE_INVALID
	scf
	ret
	
LoadStateFiles:
	ACALL(GetStateRAMFileName)
	inc hl
	ld (errorArg),hl
	ld ix,(cram_size)
	ld a,ixh
	or ixl
	jr z,_
	dec hl
	ACALL(LookUpAppvar)
	ld a,ERROR_FILE_MISSING
	ret c
	dec hl
	dec hl
	inc bc
	inc bc
	call checksum
_
	push ix
	 ACALL(GetStateFileName)
	 ACALL(LookUpAppvar)
	pop de
	ld a,ERROR_FILE_MISSING
	ret c
	
	push de
	 ACALL(DecompressFile)
	pop ix
	; Only load original Game Boy
	ld a,(decompress_buffer + (regs_saved + STATE_SYSTEM_TYPE - save_state_start))
	or a
	jr nz,LoadStateInvalid
	; Ensure state size is valid
	ld hl,save_state_size
	sbc hl,bc
	jr nz,LoadStateInvalid
	
	; Get the RAM file name (for error messages)
	push bc
	 ACALL(GetStateRAMFileName)
	pop bc
	; Validate the checksum
	ld hl,(decompress_buffer + (cart_ram_checksum - save_state_start))
	lea de,ix
	or a
	sbc hl,de
	jr nz,LoadStateInvalid
	
	; The state is valid, so load it
	inc bc
	push bc
	 ld hl,(save_state_size_bytes)
	 sbc hl,bc
	 ex de,hl
	 ld hl,save_state_start
	 push hl
	  jr nc,_
	  or a
	  sbc hl,hl
	  sbc hl,de
	  ex de,hl
	 pop hl
	 ACALL(InsertMemSafe)
	pop bc
	jr nc,++_
	ret
_
	  ACALL(DelMemSafe)
	 pop de
	pop bc
_
	ld (save_state_size_bytes),bc
	dec bc
	ld hl,decompress_buffer
	ldir
	
	; Load the RAM file (whose name we initialized above)
	AJUMP(LoadRAMAny)
	
	
	; Returns: C set if not found
	;          Otherwise, HL=data pointer, BC=data size, Z set if in RAM
LookUpAppvar:
	call _Mov9ToOP1
	call _chkFindSym
	ret c
GetDataSection:
	; Check if in RAM
	call _ChkInRam
	ex de,hl
	ld bc,0
	jr z,_
	ld c,9
	add hl,bc
	ld c,(hl)
	inc hl
_
	; Reset carry but preserve Z flag
	add hl,bc
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ret
	
	
	; Populates and sorts the ROM list, and sets (romTotalCount)
ROMSearch:
	; Iterate through the VAT
	ld iy,romListStart
	ld hl,(progPtr)
	xor a
	ld c,a
ROMSearchLoop:
	; Check if we reached the VAT's end
	ex de,hl
	ld hl,(pTemp)
	sbc hl,de
	jr z,ROMSearchSort
	; Get the type byte
	ld a,(de)
	ld hl,-7
	add hl,de
	; Get the name size in D and the data pointer MSB in DEU
	ld de,(hl)
	xor appVarObj
	jr nz,ROMSearchNoMatch
	; Make sure the name is 5 characters long or less
	ld a,d
	cp 6
	jr nc,ROMSearchNoMatch
	push hl
	 push bc
	  ; Get the data pointer in DE
	  inc hl
	  inc hl
	  inc hl
	  ld d,(hl)
	  inc hl
	  ld e,(hl)
	  ; Advance to the variable data section
	  ACALL(GetDataSection)
	  ; Make sure the magic header is correct
	  ld de,MetaHeader
	  ld bc,8
	  call memcmp
	  jr z,ROMSearchMatch
	 pop bc
	pop hl
ROMSearchContinue:
	; Get the name length in D again and reset carry
	ld de,(hl)
	or a
ROMSearchNoMatch:
	; Subtract the name length to move to the next VAT entry
	ld e,1
	mlt de
	sbc hl,de
	jr ROMSearchLoop
	
ROMSearchMatch:
	  ; Get the description pointer in DE
	  ex de,hl
	  inc de
	 pop bc
	 ; Save the next node pointer
	 pea iy+3
	  ; Insert the new node into the max heap
	  ; Start with B=last node index and (SP) = last node pointer
	  ld b,c
	  push iy
ROMSearchHeapInsertLoop:
	   ; Move B to the parent node, or exit the loop if at the root
	   srl b
	   jr c,_
	   jr z,ROMSearchHeapInsertDone
	   dec b
_
	   push bc
	    ; Get the parent node pointer in IY and contents in IX,
	    ; and the description pointer in HL
	    call GetRomDescriptionByIndex
	    push de
	     call CompareDescriptions
	    pop de
	   pop bc
	   ; If the child is less than the parent, exit the loop
	   jr c,ROMSearchHeapInsertDone
	   ; Otherwise write the parent's contents to the child,
	   ; and save the parent node pointer in (SP)
	   ex (sp),iy
	   ld (iy),ix
	   jr ROMSearchHeapInsertLoop
ROMSearchHeapInsertDone:
	  pop hl
	 pop iy
	pop de
	; Write the new ROM entry to the current node pointer
	ld (hl),de
	ex de,hl
	; Increment the size
	inc c
	jr nz,ROMSearchContinue
	dec c
	
	; Finish out the heap sort
ROMSearchSort:
	ld a,c
	ld (romTotalCount),a
	or a
	ret z
ROMSearchHeapSortLoop:
	; Get the last node in the heap (or exit if it's the only node)
	dec a
	ret z
	ld b,a
	call GetRomDescriptionByIndex
	; B=0 after this call, which is the root node index
	; Update the heap size
	ld c,a
	; Hold the description pointer in DE
	ex de,hl
	; Save the old contents
	push ix
	 ; Put a pointer to the root node in (SP)
	 ld hl,romListStart
	 push hl
	  ; Move the root node contents (current max) to the former last node
	  ld ix,(hl)
	  ld (iy),ix
	  ; Balance the former last node contents starting at the root
	  scf
ROMSearchHeapBalanceLoop:
	  ; Carry must be set here, move to the left child node index
	  rl b
	  ; If this node doesn't exist, we're done balancing
	  ld a,b
	  sub c
	  jr nc,ROMSearchHeapBalanceDone
	  push bc
	   ; Get the left node pointer in IY and contents in IX,
	   ; and the description pointer in HL
	   call GetRomDescriptionByIndex
	   ; If the right node doesn't exist, balance to the left
	   inc a
	   jr z,ROMSearchHeapBalanceLeft
	   ; Check whether the left or right node should be selected for balancing
	   push de
	    ; Put the left node's description in DE
	    ex de,hl
	    ; Move IY to the right node and get its contents in IX
	    ; and its description pointer in HL
	    lea iy,iy+3
	    call GetRomDescription
	    push hl
	     push de
	      call CompareDescriptions
	     pop de
	    pop hl
	    ; If the left node is less than the right node, balance to the right
	    jr c,ROMSearchHeapBalanceRight
	    ; Otherwise, restore IY, IX, and HL to the left node
	    ex de,hl
	    lea iy,iy-3
	    ld ix,(iy)
	   pop de
	   jr ROMSearchHeapBalanceLeft
ROMSearchHeapBalanceRight:
	   pop de
	   ; Move the node index to the right node
	  pop bc
	  inc b
	  push bc
ROMSearchHeapBalanceLeft:
	   push de
	    call CompareDescriptions
	   pop de
	  pop bc
	  ; If the parent node is greater than the child node, we're done balancing
	  jr nc,ROMSearchHeapBalanceDone
	  ; Otherwise write the child's contents to the parent,
	  ; and save the child node pointer in (SP)
	  ex (sp),iy
	  ld (iy),ix
	  ; Carry is set
	  jr ROMSearchHeapBalanceLoop
	  
ROMSearchHeapBalanceDone:
	 pop hl
	pop de
	; Write the former last node contents to the current node
	ld (hl),de
	ld a,c
	jr ROMSearchHeapSortLoop
	
	  
	; Input: rtc_last
	; Output: HLIX = 48-bit timestamp based on current time
GetUnixTimeStamp:
	ld.sis ix,(rtc_last+3)
	ld de,(epochDayCount)
	add ix,de
	or a
	sbc hl,hl
	call MulHLIXBy24
	ld.sis bc,(rtc_last+2)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
_	
	
	call MulHLIXBy24
	add ix,ix \ adc hl,hl
	add ix,bc \ adc hl,de
	ld.sis bc,(rtc_last+1)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
_
	
	call MulHLIXBy24
	add ix,ix \ adc hl,hl
	add ix,bc \ adc hl,de
	ld.sis bc,(rtc_last)
	ld b,0
	add ix,bc
	jr nc,_
	inc hl
	or a
_
	
	push hl
	 sbc hl,hl
	 ld de,(timeZoneOffset)
	 sbc hl,de
	 ex de,hl
	pop hl
	ret z
	
	add ix,de
	jr c,_
	dec hl
_
	
	ret m
	inc hl
	ret
	
	
	; Input: HLIX = 48-bit UTC timestamp
	; Output: A,B,C = hours,minutes,seconds
	;         HLIX = days
ExtractUnixTimeStamp:
	ld de,(timeZoneOffset)
	add ix,de
	jr nc,_
	inc hl
_
	ex de,hl
	add hl,hl
	ex de,hl
	jr nc,_
	dec hl
_
	call DivHLIXBy60
	ld c,a
	push bc
	 call DivHLIXBy60
	pop bc
	ld b,a
	push bc
	 ld c,24
	 call DivHLIXByC
	pop bc
	ret
	
BackupOriginalHardwareSettings:
	ld ix,originalHardwareSettings
BackupHardwareSettings:
	ld hl,(mpIntEnable)
	ld (ix+0),hl
	ld hl,(mpIntLatch)
	ld (ix+3),hl
	ld a,(mpFlashWaitStates)
	ld (ix+6),a
	ld a,mb
	ld (ix+7),a
	ld a,(mpKeypadScanMode)
	ld (ix+8),a
	ld a,(mpLcdImsc)
	ld (ix+9),a
	ld a,(mpRtcCtrl)
	ld (ix+10),a
	ld hl,(mpSpiDivider)
	ld (ix+11),hl
	ret
	
SetCustomHardwareSettings:
	di
	ld a,$FB      ; EI
	ld (z80codebase + wait_for_interrupt_stub),a
	ld hl,$C94976 ; HALT \ RET.LIS
	ld (z80codebase + wait_for_interrupt_stub + 1),hl
	APTR(customHardwareSettings)
	push hl
	pop ix
	jr RestoreHardwareSettings
	
RestoreOriginalLcdSettings:
	ACALL(SetCustomHardwareSettings)
	ld hl,originalLcdSettings
	ld de,vRam
	ACALL(SetLcdSettings)
RestoreOriginalHardwareSettings:
	ld iy,flags
	ld ix,originalHardwareSettings
	scf
RestoreHardwareSettings:
	ld hl,(ix+0)
	ld (mpIntEnable),hl
	ld hl,(ix+3)
	ld (mpIntLatch),hl
	ld a,(ix+6)
	ld (mpFlashWaitStates),a
	ld a,(ix+7)
	ld mb,a
	ld a,(ix+8)
	ld (mpKeypadScanMode),a
	ld a,(ix+9)
	ld (mpLcdImsc),a
	ld a,(ix+10)
	ld (mpRtcCtrl),a
	ld hl,(ix+11)
	ld (mpSpiDivider),hl
	ret nc
	ei
	ret
	
SetMenuWindow:
	ld ix,scanlineLUT_1
	ld a,(current_buffer+1)
	cp (gb_frame_buffer_1 >> 8) & $FF
	jr nz,_
	ld ix,scanlineLUT_2
_
	ld de,mini_frame_backup
	ld a,144
backup_mini_screen_row_loop:
	ld hl,(ix)
	ld bc,160
	ldir
	lea ix,ix+3
	dec a
	jr nz,backup_mini_screen_row_loop
	
	APTR(lcdSettingsMenu)
SetLcdSettingsFirstBuffer:
	ld de,menu_frame_buffer
	
	; In: (HL) = timing (12 bytes)
	;     (HL+12) = LCD control (3 bytes)
	;     (HL+15) = offset to SPI settings
	;     DE = new LCD base address
SetLcdSettings:
	push hl
	 ; Reset interrupt mask
	 ld a,8
	 ld hl,mpLcdImsc
	 ld (hl),a
	 ; Wait for DMA completion
	 ld l,mpLcdIcr & $FF
	 ld (hl),a
	 ld l,mpLcdMis & $FF
_
	 tst a,(hl)
	 jr z,-_
	 ld l,mpLcdIcr & $FF
	 ld (hl),a

	 ; Set DMA base address
	 ex de,hl
	 ld (mpLcdBase),hl
	pop hl
	
	; Set timing parameters
	ld e,mpLcdTiming0 & $FF
	ld bc,12
	ldir
	
	; Set LCD control
	ld e,mpLcdCtrl & $FF
	ld c,3
	ldir
	
	; Get SPI settings address
	ld de,(hl)
	dec.s de
	ld hl,(ArcBase)
	add hl,de
	ex de,hl
	ld b,spiSetupSize
	jp spiFastTransfer
	
GeneratePixelCache:
	ld hl,vram_start
	ld de,vram_pixels_start
	ld c,$0C
_
	push hl
	 ld hl,(hl)
	 push bc
	  ld b,8
_
	  sla h
	  ccf
	  sbc a,a
	  or $01
	  sla l
	  jr nc,$+5 \ rlca \ adc a,0
	  ld (de),a
	  inc de
	  djnz -_
	 pop bc
	pop hl
	inc hl
	inc hl
	djnz --_
	dec c
	jr nz,--_
	ret
	
SetScalingMode:
	; Disable GRAM access from DMA to allow a clean buffer rewrite
	ld de,spiResetWindowAddress
	ld b,spiDisableRamAccessSize
	call spiFastTransfer
	
	ld hl,scanlineLUT_2
	ld (scanlineLUT_ptr),hl
	ld (scanlineLUT_sprite_ptr),hl
	ld (scanlineLUT_palette_ptr),hl
	ld hl,gb_frame_buffer_2
	ld (current_buffer),hl
	ld ix,scanlineLUT_1
	ld hl,mini_frame_backup
	ld de,gb_frame_buffer_1
	ld (current_display),de
	ld bc,0
	ld (frame_flip_end_check_smc),bc
	ld a,(active_scaling_mode)
	or a
	jr z,SetNoScalingMode
	ld (gram_curr_draw_buffer),a
	
	; Restore the mini frame backup and initialize the scanline LUTs
	ld a,144/3*2
_
	ld c,160
	ld (ix),de
	ldir
	ld c,160
	ex de,hl
	add hl,bc
	ex de,hl
	ld (ix+3),de
	ldir
	ld c,160
	ex de,hl
	add hl,bc
	ex de,hl
	ld (ix+6),de
	ldir
	lea ix,ix+9
	dec a
	jr nz,-_
	ACALL(GeneratePixelCache)
	
	ld hl,inc_real_frame_count_or_flip_gram_display
	ld (inc_real_frame_count_smc_1),hl
	ld (inc_real_frame_count_smc_2),hl
	
	ld hl,-160*240
	ld (frame_dma_size_smc),hl
	
	call do_scale_fill
	
	APTR(lcdSettings8BitStretched)
	ACALL(SetLcdSettingsFirstBuffer)
	jp do_frame_flip_always_no_gram_flip
	
SetNoScalingMode:
	; Restore the mini frame backup and initialize the scanline LUTs
	ld a,144
_
	push af
_
	 ld c,160
	 ld (ix),de
	 ldir
	 lea ix,ix+3
	 dec a
	 jr nz,-_
	 ld de,gb_frame_buffer_2
	pop af
	ccf
	jr c,--_
	
	ld hl,inc_real_frame_count
	ld (inc_real_frame_count_smc_1),hl
	ld (inc_real_frame_count_smc_2),hl
	
	ld hl,-160*144
	ld (frame_dma_size_smc),hl
	
	ACALL(GeneratePixelCache)
	
	ld a,(GameSkinDisplay)
	cp $FF
	jr nz,_
	ld a,(SkinDisplay)
_
	rra
	jr nc,_
	ld hl,(skin_file_ptr)
	ex de,hl
	sbc hl,hl
	add hl,de
_
	ld hl,gb_frame_buffer_2
	push hl
	 jr nc,no_skin
	 push de
	  ld hl,mpLcdPalette
	  ld de,palette_backup
	  ld bc,32
	  push hl
	   ldir
	  pop de
	 pop hl
	 ld c,32
	 ldir
	 ; Make sure decompressed size is 160*240
	 inc hl
	 ld a,(hl)
	 sub (160*240) >> 8
	 dec hl
	 or (hl)
	 jr nz,bad_skin_file
	pop de
	call lzf_decompress
	
	ACALL(Set8BitWindowNoScale)
	
	xor a
bad_skin_file:
	; Restore only the static palette entries, dynamic ones are generated
	ld hl,palette_backup + (BLUE*2)
	ld de,mpLcdPalette + (BLUE*2)
	ld bc,32 - (BLUE*2)
	ldir
	ret z
	
	pop hl
	push hl
no_skin:
	pop de
	inc de
	ld (hl),BLACK*$11
	ld bc,160*240-1
	ldir
Set8BitWindowNoScale:
	APTR(lcdSettings4Bit)
	ld de,gb_frame_buffer_2
	ACALL(SetLcdSettings)
	APTR(lcdSettings8BitNoScale)
	ACALL(SetLcdSettingsFirstBuffer)
	jp do_frame_flip_always_no_gram_flip
	
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
	
	; Convert 15 pixels per loop, running at 1 wait state
sha_code:
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	ld e,(hl) \ ld a,(de) \ ld (hl),a \ inc hl
	djnz sha_code
	ret
sha_code_size = $ - sha_code
sha_code_entry_offset = (15 - (160 % 15)) * 4
convert_palette_row = mpShaData + sha_code_entry_offset
convert_palette_row_loop_count = (160 / 15) + 1
	
MBC_IMPL_SIZE = 16
mbc_impl_code:
	.assume adl=0
	
	;MBC1
	.org 0
	add a,a
	add a,a
	jr c,mbc_4000 - mbc_impl
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 jp p,mbc_0000
mbc1_large_rom_continue:
	 ; Mask the new value and check if 0-page should be overridden
	 and $1F
	 jr z,mbc_zero_page_override - mbc_impl
#if $ != MBC_IMPL_SIZE
	.error "MBC1 impl size is incorrect"
#endif

	;MBC2
	.org 0
	bit 6,a
	jr nz,mbc_denied - mbc_impl
	rra
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 jr nc,mbc_0000 - mbc_impl
	 and $0F
	 jr z,mbc_zero_page_override - mbc_impl
#if $ != MBC_IMPL_SIZE
	.error "MBC2 impl size is incorrect"
#endif
	
	;MBC3
	.org 0
	add a,a
	add a,a
	jr c,mbc_4000 - mbc_impl
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 jp p,mbc_0000
	 and $7F
	 jr z,mbc_zero_page_override - mbc_impl
#if $ != MBC_IMPL_SIZE
	.error "MBC3 impl size is incorrect"
#endif
	
	;MBC5
	.org 0
	add a,a
	add a,a
	jr c,mbc_4000 - mbc_impl
	add a,a
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 jr nc,mbc5_0000 - mbc_impl
	 jp m,mbc_denied_restore
	 nop
#if $ != MBC_IMPL_SIZE
	.error "MBC5 impl size is incorrect"
#endif
	
	.org mbc_impl_code + (MBC_IMPL_SIZE * 4)
	
mbc3rtc_4000_impl:
	.org 0
	ld a,(cram_actual_bank_base+2)
	cp z80codebase>>16
	jp.lil z,mbc_rtc_switch_from_rtc_helper
	bit 3,c
	jr z,mbc_ram - mbc_4000_impl
	jp.lil mbc_rtc_switch_to_rtc_helper
mbc3rtc_4000_impl_size = $
	
	.org mbc3rtc_4000_impl + mbc3rtc_4000_impl_size
	.assume adl=1
	
customHardwareSettings:
	;mpIntEnable
	.dl $000001
	;mpIntLatch
	.dl $000011
	;mpFlashWaitStates
	.db 2
	;MBASE
	.db z80codebase >> 16
	;mpKeypadScanMode
	.db 3
	;mpLcdImsc
	.db 8
	;mpRtcCtrl
	.db $83
	;mpSpiDivider
#ifdef CEMU
	.dl $020000
#else
	.dl $070000
#endif
	
lcdSettings4Bit:
	; LcdTiming0
	.db $38,$03,$9C,$1F
	; LcdTiming1
	.db $3F,$01,$8F,$00
	; LcdTiming2
	.db $00,$78,$EF,$00
	; LcdCtrl
	.dl $013C25
	; SPI settings
	.dw spiSetupDefault+1
	
lcdSettingsMenu:
	; LcdTiming0
	.db $FC,$00,$00,$00 ; PPL=1024, HSW=1, HBP=1, HFP=1 (total=1027)
	; LcdTiming1
#ifdef CEMU
	.db $4A,$00,$03,$F2 ; LPP=75, VSW=1, VBP=242, VFP=3 (total=321)
#else
	.db $4A,$00,$03,$C4 ; LPP=75, VSW=1, VBP=196, VFP=3 (total=275)
#endif
	; LcdTiming2
	.db $00,$78,$FF,$03 ; PCD=2, CPL=1024
	; LcdCtrl
	.dl $013C27
	; SPI settings
	.dw spiSetupVsyncInterface+1
	
lcdSettings8BitNoScale:
#ifdef CEMU
	; LcdTiming0
	.db $44,$03,$04,$0D ; PPL=288, HSW=4, HBP=14, HFP=5 (total=311)
	; LcdTiming1
	.db $4F,$00,$F2,$00 ; LPP=80, VSW=1, VBP=0, VFP=242 (total=323)
	; LcdTiming2
	.db $02,$78,$1F,$01 ; PCD=4, CPL=288
#else
	; LcdTiming0
	.db $B0,$03,$6D,$1F ; PPL=720, HSW=4, HBP=32, HFP=110 (total=866)
	; LcdTiming1
	.db $1F,$00,$C7,$00 ; LPP=32, VSW=1, VBP=0, VFP=199 (total=232)
	; LcdTiming2
	.db $00,$78,$CF,$02 ; PCD=2, CPL=720
#endif
	; LcdCtrl
	.dl $013C27
	; SPI settings
	.dw spiSetupNoScale+1
	
lcdSettings8BitStretched:
#ifdef CEMU
	; LcdTiming0
	.db $60,$01,$0E,$0F ; PPL=400, HSW=2, HBP=16, HFP=15 (total=433)
	; LcdTiming1
	.db $5F,$00,$FF,$70 ; LPP=96, VSW=1, VBP=112, VFP=255 (total=464)
	; LcdTiming2
	.db $00,$78,$8F,$01 ; PCD=2, CPL=400
#else
	; LcdTiming0
	.db $C4,$03,$1D,$1F ; PPL=800, HSW=4, HBP=32, HFP=30 (total=866)
	; LcdTiming1
	.db $2F,$00,$B7,$00 ; LPP=48, VSW=1, VBP=0, VFP=183 (total=232)
	; LcdTiming2
	.db $00,$78,$1F,$03 ; PCD=2, CPL=800
#endif
	; LcdCtrl
	.dl $013C27
	; SPI settings
	.dw spiSetupDoubleScale+1
	
spiSetupDefault:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(0)   ;  Left bound
	SPI_PARAM16(319) ;  Right bound
	SPI_CMD($2B)     ; Row address set
	SPI_PARAM16(0)   ;  Upper bound
	SPI_PARAM16(239) ;  Lower bound
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($11)   ;  RGB Interface
	SPI_PARAM($F0)
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(0)   ;  Top fixed area
	SPI_PARAM16(320) ;  Scrolling area
	SPI_PARAM16(0)   ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(0)   ;  No scroll
	SPI_CMD($E4)     ; Gate Control
	SPI_PARAM($27)   ;  320 lines
	SPI_PARAM($00)   ;  Start line 0
	SPI_PARAM($10)   ;  No interlace
	SPI_END
spiSetupSize = $ - spiSetupDefault
	
spiSetupVsyncInterface:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(0)   ;  Left bound
	SPI_PARAM16(319) ;  Right bound
	SPI_CMD($2B)     ; Row address set
	SPI_PARAM16(0)   ;  Upper bound
	SPI_PARAM16(239) ;  Lower bound
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($12)   ;  VSYNC Interface
	SPI_PARAM($F0)
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(0)   ;  Top fixed area
	SPI_PARAM16(320) ;  Scrolling area
	SPI_PARAM16(0)   ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(0)   ;  No scroll
	SPI_CMD($E4)     ; Gate Control
	SPI_PARAM($27)   ;  320 lines
	SPI_PARAM($00)   ;  Start line 0
	SPI_PARAM($10)   ;  No interlace
	SPI_END
	
spiSetupNoScale:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(80)  ;  Left bound
	SPI_PARAM16(239) ;  Right bound
	SPI_CMD($2B)     ; Row address set
	SPI_PARAM16(48)  ;  Upper bound
	SPI_PARAM16(191) ;  Lower bound
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($12)   ;  VSYNC Interface
	SPI_PARAM($F0)
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(0)   ;  Top fixed area
	SPI_PARAM16(320) ;  Scrolling area
	SPI_PARAM16(0)   ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(0)   ;  No scroll
	SPI_CMD($E4)     ; Gate Control
	SPI_PARAM($27)   ;  320 lines
	SPI_PARAM($00)   ;  Start line 0
	SPI_PARAM($10)   ;  No interlace
	SPI_END
	
spiSetupDoubleScale:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(0)   ;  Left bound
	SPI_PARAM16(159) ;  Right bound
	SPI_CMD($2B)     ; Row address set
	SPI_PARAM16(0)   ;  Upper bound
	SPI_PARAM16(239) ;  Lower bound
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($12)   ;  VSYNC Interface
	SPI_PARAM($F0)
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(0)   ;  Top fixed area
	SPI_PARAM16(160) ;  Scrolling area
	SPI_PARAM16(160) ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(320) ;  Duplicate right side to left
	SPI_CMD($E4)     ; Gate Control
	SPI_PARAM($27)   ;  320 lines
	SPI_PARAM($00)   ;  Start line 0
	SPI_PARAM($14)   ;  Interlace
	SPI_END
	
regs_init:
	.db $00	; Hardware type
	.db $00 ; Interrupt enable
	;     AF,   BC,   DE,   HL,   SP,   PC
	.dw $01B0,$0013,$00D8,$014D,$FFFE,$0100
	.dw $0000 ; Frame cycle counter
	.dw -$6AF3 ; Serial transfer cycle counter
	.dw $6AF3 ; Divisor cycle counter
	.db $01 ; Cart ROM bank
	.db $00 ; Cart RAM bank
	.db $02 ; MBC mode
	.db $00 ; CPU mode
	
	
hmem_init:
	.db $CF,0,$7E,$FF,0,$00,$00,$F8,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E1
	.db $80,$BF,$F3,$FF,$BF,$FF,$3F,$00,$FF,$BF,$7F,$FF,$9F,$FF,$BF,$FF
	.db $FF,$00,$00,$BF,$77,$F3,$F1,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db $91,0,$00,$00,0,$00,$FF,$FC,$FF,$FF,$00,$00,$FF
hmem_init_size = $ - hmem_init
	
	; Used to convert between flag register formats.
	; When bit 3 is reset, converts from Game Boy F to ez80 F.
	; When bit 3 is set, converts from ez80 F to Game Boy F.
	; Note that bit 3 is always set in the ez80 F register, since operations
	; that affect flags do not modify bits 3, it's used as an optimization.
	; Care must be taken with AF/AF' swaps to ensure the correct F register
	; is used to store flags, even if "all" flags are affected by an operation.
flags_lut_init:
	.db $08,$08,$08,$08,$08,$08,$08,$08, $00,$10,$40,$50,$00,$10,$40,$50
	.db $09,$09,$09,$09,$09,$09,$09,$09, $20,$30,$60,$70,$20,$30,$60,$70
	.db $18,$18,$18,$18,$18,$18,$18,$18, $00,$10,$40,$50,$00,$10,$40,$50
	.db $19,$19,$19,$19,$19,$19,$19,$19, $20,$30,$60,$70,$20,$30,$60,$70
	.db $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $1A,$1A,$1A,$1A,$1A,$1A,$1A,$1A, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $48,$48,$48,$48,$48,$48,$48,$48, $00,$10,$40,$50,$00,$10,$40,$50
	.db $49,$49,$49,$49,$49,$49,$49,$49, $20,$30,$60,$70,$20,$30,$60,$70
	.db $58,$58,$58,$58,$58,$58,$58,$58, $00,$10,$40,$50,$00,$10,$40,$50
	.db $59,$59,$59,$59,$59,$59,$59,$59, $20,$30,$60,$70,$20,$30,$60,$70
	.db $4A,$4A,$4A,$4A,$4A,$4A,$4A,$4A, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $4B,$4B,$4B,$4B,$4B,$4B,$4B,$4B, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	.db $5A,$5A,$5A,$5A,$5A,$5A,$5A,$5A, $80,$90,$C0,$D0,$80,$90,$C0,$D0
	.db $5B,$5B,$5B,$5B,$5B,$5B,$5B,$5B, $A0,$B0,$E0,$F0,$A0,$B0,$E0,$F0
	
	; Specifies offsets into a buffer of pixel data corresponding to the
	; input 2bpp pixel data. Note that the input has the high palette bits
	; grouped in the high nibble, and the low palette bits in the low nibble.
overlapped_pixel_index_lut_init:
	.db $00,$80,$81,$88,$82,$A0,$89,$A6,$83,$8B,$A1,$A9,$8A,$A8,$A7,$E8
	.db $01,$84,$05,$8C,$11,$A2,$15,$AA,$93,$9B,$B1,$B9,$C0,$D6,$47,$E9
	.db $02,$90,$85,$98,$06,$AE,$8D,$B6,$12,$C1,$A3,$D7,$16,$48,$AB,$EA
	.db $09,$94,$0D,$9C,$19,$B2,$1D,$BA,$3F,$CD,$43,$E3,$4B,$ED,$4F,$F1
	.db $03,$13,$91,$BE,$86,$A4,$99,$D4,$07,$17,$AF,$49,$8E,$AC,$B7,$EB
	.db $21,$31,$23,$C2,$2F,$D2,$33,$D8,$25,$35,$53,$5F,$C4,$DA,$5D,$F9
	.db $0A,$40,$95,$CA,$0E,$44,$9D,$E0,$1A,$4C,$B3,$EE,$1E,$50,$BB,$F2
	.db $27,$C6,$2B,$CE,$37,$DC,$3B,$E4,$55,$70,$59,$F5,$61,$77,$65,$FB
	.db $FF,$04,$10,$14,$92,$B0,$BF,$46,$7F,$87,$9F,$A5,$9A,$B8,$D5,$E7
	.db $08,$0C,$18,$1C,$3E,$42,$4A,$4E,$8F,$97,$AD,$B5,$CC,$E2,$EC,$F0
	.db $20,$22,$2E,$32,$24,$52,$C3,$5C,$30,$BD,$D1,$D3,$34,$5E,$D9,$F8
	.db $26,$2A,$36,$3A,$54,$58,$60,$64,$C5,$C9,$DB,$DF,$6F,$F4,$76,$FA
	.db $0B,$1B,$41,$4D,$96,$B4,$CB,$EF,$FE,$0F,$3D,$45,$7E,$9E,$E1,$E6
	.db $29,$39,$57,$63,$C8,$DE,$6E,$75,$1F,$2D,$51,$5B,$BC,$D0,$F3,$F7
	.db $28,$56,$C7,$6D,$2C,$5A,$CF,$F6,$38,$62,$DD,$74,$FD,$3C,$7D,$E5
	.db $68,$69,$6A,$71,$6B,$79,$72,$7B,$67,$6C,$78,$7A,$66,$73,$FC,$7C
	
	; Specifies palette indices corresponding to the input 2bpp palettes.
	; Note that this is only used for colors 1-3, because BG color 0 is
	; special-cased and this can be used for BG+sprites.
overlapped_palette_index_lut_init:
	.db $00,$03,$06,$3F,$02,$0B,$12,$1B,$05,$25,$15,$1E,$08,$18,$0F,$3E
	.db $01,$0C,$16,$09,$0A,$23,$2B,$22,$11,$2A,$33,$30,$1A,$2D,$21,$28
	.db $04,$13,$1F,$10,$0D,$24,$31,$29,$14,$32,$38,$37,$1D,$2F,$3A,$36
	.db $07,$1C,$26,$19,$17,$2C,$34,$2E,$20,$0E,$39,$3B,$27,$35,$3C,$3D
	
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
	; Dummy entry was formerly $7C, but classic is better
	.db $1D,$08,$12,$A3,$A2,$07,$87,$4B,$20,$12,$65,$A8,$16
	.db $A9,$86,$B1,$68,$A0,$87,$66,$12,$A1,$30,$3C,$12,$85
	.db $12,$64,$1B,$07,$06,$6F,$6E,$6E,$AE,$AF,$6F,$B2,$AF
	.db $B2,$A8,$AB,$6F,$AF,$86,$AE,$A2,$A2,$12,$AF,$13,$12
	.db $A1,$6E,$AF,$AF,$AD,$06,$4C,$6E,$AF,$AF,$12,$7C,$AC
	.db $A8,$6A,$6E,$13,$A0,$2D,$A8,$2B,$AC,$64,$AC,$6D,$87
	.db $BC,$60,$B4,$13,$72,$7C,$B5,$AE,$AE,$7C,$7C,$65,$A2
	.db $6C,$64,$85
	
DefaultGameConfig:
; Size bytes
	.dw config_end - ConfigVersion
; Version
	.db 1	
; Number of option bytes
	.db option_config_count
; Option bytes defaulting to global
	.block option_config_count, $FF
; Number of key bytes
	.db key_config_count
; Key bytes defaulting to global
	.block key_config_count, $FF
	
#macro DEFINE_ERROR(name, text)
	#define NUM_ERRORS eval(NUM_ERRORS+1)
	clr()
	wr(name,"=NUM_ERRORS")
	run()
	.db text,0
#endmacro

#define NUM_ERRORS 0
warning_text:
	.db "Warning: ",0
error_text:
	.db "Error: "
error_messages:
	.db 0
	DEFINE_ERROR("ERROR_FILE_MISSING", "Missing AppVar %s")
	DEFINE_ERROR("ERROR_FILE_INVALID", "Invalid AppVar %s")
	DEFINE_ERROR("ERROR_NOT_ENOUGH_ARCHIVE", "Failed to archive AppVar\n%s")
	DEFINE_ERROR("ERROR_UNSUPPORTED_MBC", "Unsupported cartridge type %02X")
	DEFINE_ERROR("ERROR_INVALID_ROM", "ROM is invalid")
	DEFINE_ERROR("ERROR_NOT_ENOUGH_MEMORY", "Need %d more bytes free RAM")
	DEFINE_ERROR("ERROR_RUNTIME", "Encountered a runtime error!")
	DEFINE_ERROR("ERROR_INVALID_OPCODE", "Ran an invalid Game Boy opcode\nat %06X")
	DEFINE_ERROR("ERROR_KEY_CONFLICT", "Duplicate key mappings in\nper-game config have been removed")
	
ErrorNoROMsFound:
	.db "No ROMs found!",0
	
StateLoadedMessage:
	.db "State %c loaded",0
	
AutoStateLoadedMessage:
	.db "Auto state loaded",0
	
StateSavedMessage:
	.db "State %c saved",0
	
StateSlotMessage:
	.db "State %c selected",0
	
StateNotFoundMessage:
	.db "State %c not found",0
	