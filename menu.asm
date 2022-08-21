#define ITEM_LINK 0
#define ITEM_CMD 1
#define ITEM_DIGIT 2
#define ITEM_OPTION 3
#define ITEM_KEY 4
#define ITEM_ROM 5

#define ITEM_ROMONLY $80

#define ROMS_PER_PAGE 15

#define UNMAPPED_KEY 41

ApplyConfiguration:
	; Display next frame always
	ld a,$4F ;LD R,A
	ld (z80codebase+updateSTAT_enable_catchup_smc),a
	ld (z80codebase+updateSTAT_full_enable_catchup_smc),a
	ld (z80codebase+ppu_mode0_enable_catchup_smc),a
	ld (z80codebase+ppu_mode2_enable_catchup_smc),a
	ld (z80codebase+ppu_lyc_enable_catchup_smc),a
	
	; Frameskip value
	ld ix,FrameskipValue
	call read_config_item
	ld (speed_display_smc_0),a
	inc a
	ld (frameskip_value_smc),a
	ld (skippable_frames),a
	
	; Frameskip type
	call read_config_item
	ld c,a
	sub 2
	jr nz,_
	ld (speed_display_smc_0),a
_
	and $10
	add a,$18
	ld (frameskip_type_smc),a
	ld a,c
	or a
	jr z,_
	ld a,no_frameskip - (frameskip_type_smc+2)
_
	ld (frameskip_type_smc+1),a
	
	; Speed display
	call read_config_item
	ld c,a
	sub 1
	sbc a,a
	and NoSpeedDisplay - YesSpeedDisplay
	add a,YesSpeedDisplay - (speed_display_smc_1 + 1)
	ld (speed_display_smc_1),a
	ld a,c
	sub 2
	and $10
	add a,$CE
	ld (speed_display_smc_3),a
	ld a,c
	sub 3
	and $10
	add a,$18
	ld (speed_display_smc_2),a
	
	; Auto Save State
	call read_config_item
	ld (should_auto_save),a
	
	; Palette selection
	call read_config_item
	ld hl,default_palette
	or a
	ld bc,0
	jr z,_
	APTR(ManualPaletteIndexTable-1)
	ld c,a
	add hl,bc
_
	push hl
	
	 ; Time zone
	 call read_config_item
	 ld c,a
	 ; Daylight saving time
	 call read_config_item
	 APTR(TimeZoneOffsetTable)
	 add hl,bc
	 ld e,(hl)
	 ld d,225
	 mlt de
	 sbc hl,hl
	 or a
	 jr z,_
	 ld hl,60*60
_
	 sbc hl,de
	 ld a,c
	 cp 19
	 jr nc,_
	 add hl,de
	 add hl,de
_
	 ld (timeZoneOffset),hl
	
	 ; Scaling mode
	 call read_config_item
	 ld (active_scaling_mode),a
	 ; Skin display
	 inc ix
	
	 ; Turbo toggle
	 call read_config_item
	 dec a
	 and turbo_skip_toggle - (turbo_toggle_smc+1)
	 ld (turbo_toggle_smc),a
	 ld a,(turbo_active)
	 add a,a
	 add a,a
	 add a,a
	 add a,$20
	 ld (turbo_keypress_smc),a
	
	 ; Scaling type
	 call read_config_item
	 ld (active_scaling_type),a
	 ; Message display
	 inc ix
	pop hl
	; Disable color adjustment for classic palette
	ld a,(hl)
	sub $1D
	; Adjust colors
	call nz,read_config_item
	dec a
	and $C9 - $7C
	add a,$7C ;RET or LD A,L
	ld (adjust_color_enable_smc),a
	; Load palettes only after setting adjust color SMC
	ld a,(hl)
	ACALL(LoadPalettes)
	; BC=0
	
	; Check for conflicting keys between configs
	ld e,c
key_conflict_retry:
	ld ix,KeyConfig
	ld d,key_config_count
key_conflict_loop:
	call read_config_item
	jr nz,_
	; This key is inherited from global, so make sure it has no conflict
	; among game-specific keys
	ld hl,GameKeyConfig
	ld c,key_config_count
	cpir
	jr nz,_
	; If a conflict was found, reset it to global and start over
	dec hl
	ld e,$FF
	ld (hl),e
	jr key_conflict_retry
_
	dec d
	jr nz,key_conflict_loop
	
	push de
	 ; Key configuration
	 APTR(KeySMCList)
	 ex de,hl
	 ld hl,key_smc_turbo
	 lea ix,ix-key_config_count
	 ld b,key_config_count
key_config_loop:
	 call read_config_item
	 dec a
	 ld c,a
	 cpl
	 and %00111000
	 rrca
	 rrca
	 ld (hl),a
	 inc hl
	 ld a,c
	 and %00000111
	 add a,a
	 add a,a
	 add a,a
	 add a,$46
	 ld (hl),a
	 ld a,(de)
	 inc de
	 add a,l
	 jr nc,_
	 ld l,$FF
	 inc hl
_
	 ld l,a
	 djnz key_config_loop
	pop af
	ret nc
	ld a,ERROR_KEY_CONFLICT
	AJUMP(DisplayWarning)
	
RefreshRomListFrame:
	ld a,(romListFrameStart)
	ld hl,(romTotalCount)
	ld h,ROMS_PER_PAGE
	or a
	jr z,+++_
	sub l
	jr c,++_
_
	sub h
	jr nc,-_
_
	add a,l
_
	ld (romListFrameStart),a
	sub l
	neg
	cp h
	jr c,_
	ld a,h
_
	inc a
	ld (romListFrameCount),a
	ld hl,main_menu_selection + 1
	cp (hl)
	ret nc
	ld (hl),a
	ret
	
ClearMenuBuffer:
	ld hl,menu_frame_buffer
	ld (current_buffer),hl
	push hl
	pop de
	inc de
	ld bc,320*240-1
	ld (hl),BLUE
	ldir
	ret
	
	; Input: A = palette index
	; Output: BC = 0
	; Destroys: AF,DE,HL,IX
LoadPalettes:
	ld ix,(ArcBase)
	ld bc,PaletteIndex
	add ix,bc
	ld e,a
	and $1F
	ld c,a
	ld b,3
	mlt bc
	add ix,bc
	
	; Load BGP
	ld a,e
	ld de,overlapped_bg_palette_colors
	ld c,(ix+2)
	ACALL(LoadSinglePalette)
	ex de,hl
	ld l,bg_palette_colors & $FF
	ld de,mpLcdPalette + (BG_COLOR_0 * 2)
	ld c,4*2
	ldir
	
	; Load OBP0
	ld c,(ix)
	bit 5,a
	jr nz,_
	ld c,(ix+2)
_
	ACALL(LoadSinglePalette)
	
	; Load OBP1
	ld c,(ix+1)
	rlca
	jr c,_
	ld c,(ix)
	rlca
	jr c,_
	ld c,(ix+2)
_
	; Input:  BC = palette offset, DE = output table ptr
	; Output: DE = next output table ptr, BC = 0
LoadSinglePalette:
	push af
	 push ix
	  ld hl,(ArcBase)
	  add hl,bc
	  ld bc,PaletteDictionary
	  add hl,bc
	  ld ix,overlapped_palette_index_lut
load_single_palette_input_loop:
	  push hl
	   ld hl,(hl)
	   push de
	    call adjust_color
	   pop de
	   ld c,l
	   ld b,h
load_single_palette_output_loop:
	   sbc hl,hl
	   ld l,(ix)
	   add hl,hl
	   add hl,de
	   ld (hl),c
	   inc hl
	   ld (hl),b
	   ld a,ixl
	   add a,16
	   ld ixl,a
	   jr nc,load_single_palette_output_loop
	  pop hl
	  inc hl
	  inc hl
	  ld bc,4
	  adc a,c
	  ld ixl,a
	  and 3
	  jr nz,load_single_palette_input_loop
	  ld hl,64*2
	  add hl,de
	  ex de,hl
	  ldir
	 pop ix
	pop af
	ret
	
ShowConfirmStateOperation:
	; Just return NZ if the state does not exist
	call check_valid_state
	dec a
	ret m
	; Return NZ if confirmation is disabled for this type
	ld ix,ConfirmStateOperation
	call read_config_item
	ld hl,main_menu_selection
	and (hl)
	dec a
	ret m
	ld de,ConfirmLoadState
	jr z,_
	ld de,ConfirmSaveState
_
	push de
	 call setup_menu_palette
	pop de
	ld hl,(current_state)
ShowConfirmationDialog:
	push hl
	 ld hl,(ArcBase)
	 add hl,de
	 push hl
	  ACALL(ClearMenuBuffer)
	
	  ld a,5
	  ld (cursorRow),a
	  ld a,1
	  ld (cursorCol),a
	
	  ld a,WHITE
	  ACALL(PutStringFormatColor)
	 pop hl
	pop hl
	
	ld b,2
	ld c,b
ConfirmationDialogDisplayLoop:
	APTR(ConfirmText)
	ld a,20
	ld (cursorRow),a
	push bc
_
	 ld a,1
	 ld (cursorCol),a
	 ld a,WHITE
	 ld e,'>'
	 djnz _
	 ld a,OLIVE
	 ld e,' '
_
	 call SetStringColor
	 ld a,e
	 push bc
	  push hl
	   call PutChar
	  pop hl
	  ACALL(PutString)
	 pop bc
	 dec c
	 jr nz,--_
ConfirmationDialogKeyLoop:
	 ACALL(WaitForKey)
	pop bc
	ret z
	cp 1
	jr nz,_
	ld b,a
	jr ConfirmationDialogDisplayLoop
_
	cp 4
	jr nz,_
	ld b,2
	jr ConfirmationDialogDisplayLoop
_
	cp 15
	jr z,_
	push bc
	 cp 9
	 jr z,++_
	 cp 54
	 jr z,++_
	 jr ConfirmationDialogKeyLoop
_
	ld b,2
	push bc
_
	 ACALL(GetKeyCode)
	 or a
	 jr nz,-_
	pop bc
	dec b
	dec b
	ret
	
ItemSelectCmd:
	ld hl,CmdList
	ld c,a
	ld b,2
	mlt bc
	add hl,bc
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
	jp (hl)
	
ItemSelectRom:
	APTR(CmdExit)
	push hl
GetSelectedROMName:
	ld l,a
	ld h,3
	mlt hl
	ld de,romListStart
	add hl,de
	ld hl,(hl)
	ld bc,(hl)
	ld de,ROMNameToLoad+1
	push hl
	 push de
_
	  ld a,(hl)
	  ld (de),a
	  dec hl
	  inc de
	  djnz -_
	  xor a
	  ld (de),a
	 pop de
	pop ix
	inc a
	jp GetRomDescriptionFromVAT
	
ItemSelectKey:
	ACALL(GetKeyConfig)
	push bc
	 ld c,(hl)
	 ld (hl),0
	 push bc
	  push hl
	   ACALL(draw_current_menu)
	   ACALL(WaitForKey)
	  pop de
	 pop bc
	pop hl
	jr nz,_
	ld a,c
_
	ld b,a
	ld a,c
	cp UNMAPPED_KEY
	ld a,b
	jr z,RemapKey
	ld b,key_config_count
_
	cp (hl)
	jr nz,_
	ld (hl),c
_
	inc hl
	djnz --_
_
	ld (de),a
	ACALL(draw_current_menu)
	jr menu_loop
	
RemapKey:
	ld bc,key_config_count
	cpir
	jr nz,-_
	ld a,UNMAPPED_KEY
	jr -_
	
ItemSelectDigit:
	cp 2
	jr z,menu_loop
	ACALL(ShowConfirmStateOperation)
	ld a,4
	jr nz,CmdExit
	xor a
	jr ItemSelectLink

emulator_menu_ingame:
	call convert_palette_for_menu
	xor a
emulator_menu:
	push af
	 call setup_menu_palette
	 ; If the state slot was changed, verify load state is valid
	 ld hl,main_menu_selection
	 ld b,(hl)
	 djnz _
	 call check_valid_state
	 jr nz,_
	 inc (hl)
_
	pop af
	
ItemSelectLink:
	ld (current_menu),a
	or a
	sbc hl,hl
	ld l,a
	ld de,main_menu_selection
	add hl,de
	ld (current_menu_selection),hl
	dec a
	jr nz,_
	ACALL(ROMSearch)
_
	ACALL(redraw_current_menu)
ItemSelectOption:
	
menu_loop:
	ACALL(WaitForKey)
	jr nz,_
	ld a,2
	jr CmdExit
_
	call get_current_menu_selection
	dec a
	jr z,menu_down
	cp 3
	jr c,menu_left_right
	jr z,menu_up
	cp 9-1
	jr z,menu_select
	cp 54-1
	jr z,menu_select
	cp 56-1
	jr z,menu_delete
	cp 15-1
	jr nz,menu_loop
	
	ld hl,current_menu
	xor a
	cp (hl)
	jr nz,ItemSelectLink
	
CmdExit:
	ld (exitReason),a
	ex af,af'
_
	ACALL(GetKeyCode)
	or a
	jr nz,-_
	ret
	
menu_up:
	ld a,(menuPrevItem)
	ld (bc),a
	ACALL(draw_current_menu)
menu_loop_trampoline:
	jr menu_loop
	
menu_down:
	ld a,(menuNextItem)
	ld (bc),a
	ACALL(draw_current_menu)
	jr menu_loop_trampoline
	
menu_left_right:
	dec a
	add a,a
	dec a
	ld de,ItemChangeCallbacks
_
	ACALL(DoCurrentItemCallback)
	jr menu_loop_trampoline
	
menu_delete:
	ld de,ItemDeleteCallbacks
	jr -_
	
menu_select:
	ld de,ItemSelectCallbacks
DoCurrentItemCallback:
	ld hl,(current_item_ptr)
DoItemCallback:
	ld c,(hl)
	res 7,c
	inc hl
	ex de,hl
	ld b,2
	mlt bc
	add hl,bc
	ld bc,(ArcBase)
	add hl,bc
	ld hl,(hl)
	dec.s hl
	add hl,bc
	ld b,a
	ld c,0
	ld a,(de)
	jp (hl)
	
ItemDeleteRom:
	ACALL(GetSelectedROMName)
	; Don't allow deleting the currently loaded ROM
	ld bc,(current_description)
	sbc hl,bc
	ret z
	ex de,hl
	ld de,ConfirmDeleteROM
ItemDeleteStateFinish:
	ACALL(ShowConfirmationDialog)
	jr z,redraw_current_menu_trampoline
	pop de
	ld a,5
	jr CmdExit
	
ItemDeleteState:
	call check_valid_state
	ret z
	ld de,ConfirmDeleteState
	ld hl,(current_state)
	jr ItemDeleteStateFinish
	
ItemDeleteKeyUnmap:
	; Don't allow unmapping in-game buttons or menu
	ld a,d
	dec a
	cp 9
	ret c
	ld a,UNMAPPED_KEY
	cp (hl)
	jr nz,ItemDeleteKeyUnmapFinish
	ret
	
ItemChangeRom:
	ld hl,romListFrameStart
	ld a,(hl)
	djnz _
	add a,ROMS_PER_PAGE
	jr ++_
_
	sub ROMS_PER_PAGE
	jr c,redraw_current_menu
_
	ld (hl),a
redraw_current_menu_trampoline:
	jr redraw_current_menu
	
ItemChangeDigit:
	ld e,a
	ld hl,current_state
	sub 2
	jr nz,_
	dec hl
	or (hl) ;current_config
	ld hl,FrameskipValue
	jr z,_
	ld c,(hl)
	ld hl,GameFrameskipValue
	ld a,(hl)
	inc a
	jr nz,_
	ld (hl),c
_
	ld a,b
	and 9
	ld d,a
_
	rrd
	add a,d
	daa
	rld
	ld a,e
	or a
	call z,check_valid_state
	jr z,-_
	jr draw_current_menu_trampoline
	
ItemDeleteDigit:
	cp 2
	jr nz,ItemDeleteState
	ld a,(current_config)
	dec a
	ret nz
	dec a
	ld (GameFrameskipValue),a
	jr draw_current_menu_trampoline
	
ItemDeleteKey:
	ld d,a
	ACALL(GetKeyConfig)
	; If already inheriting from global, unmap instead
	ld a,(hl)
	inc a
	jr z,ItemDeleteKeyUnmap
	; If global config is selected, unmap
	ld a,(current_config)
	dec a
	jr nz,ItemDeleteKeyUnmap
	dec a
ItemDeleteKeyUnmapFinish:
	ld (hl),a
	jr draw_current_menu_trampoline
	
ItemDeleteOption:
	or a
	ret m
	ACALL(GetOption)
	ld a,(current_config)
	dec a
	ret nz
	dec a
	ld (bc),a
	jr draw_current_menu_trampoline
	
ItemChangeOption:
	ld d,b
	ACALL(GetOption)
	add a,d
	cp (hl)
	jr c,_
	add a,(hl)
	jr c,_
	xor a
_
	ld (bc),a
draw_current_menu_trampoline:
	jr draw_current_menu
	
redraw_current_menu:
	ACALL(RefreshRomListFrame)
	
	; Apply configuration if ROM is loaded (updates palette settings)
	ld a,(ROMName+1)
	or a
	jr z,_
	push af
	 ACALL(ApplyConfiguration)
	pop af
_
	
	ACALL(ClearMenuBuffer)
	
	; Skip description display if on ROM list
	ld hl,(current_menu)
	dec l
	jr z,draw_current_menu
	
	; Display only description if no ROM is loaded
	or a
	ld a,35
	jr z,draw_current_description
	
	; Draw mini screen if on main menu
	inc l
	jr nz,_
	ACALL(draw_mini_screen)
_
	
	; Draw ROM internal name and checksum
	ld ix,(rom_start)
	ld bc,$0134
	add ix,bc
	ld a,40
	ld (cursorRow),a
	ld a,b
	ld (cursorCol),a
	ld b,(ix+$014E-$0134)
	ld c,(ix+$014F-$0134)
	push bc
	 push ix
	  APTR(TitleChecksumFormat)
	  push hl
	   ld a,MAGENTA
	   ACALL(PutStringFormatColor)
	  pop hl
	 pop hl
	pop hl
	
	ld a,30
draw_current_description:
	ld (cursorRow),a
	ld a,1
	ld (cursorCol),a
	ld hl,(current_description)
	ld a,MAGENTA
	ACALL(PutNStringColor)
	
draw_current_menu:
	; Erase the help text
	ld hl,menu_frame_buffer
	ld de,320*205
	add hl,de
	push hl
	pop de
	inc de
	ld bc,320*30-1
	ld (hl),BLUE
	ldir
	
	call get_current_menu_selection
	ld a,(bc)
	ld c,a
	push af
	
	 ; HL = menu structure, C = highlighted item index
draw_menu:
	 ld b,(hl)
	 inc hl
	 inc b
	 push bc
	  ld a,WHITE
	  call SetStringColor
	  ex de,hl
	  ld hl,$FF00
	  push hl
	   jr draw_menu_title
draw_menu_loop:
	 dec c
	 push bc
	  jr nz,_
	  ld a,205
	  ld (cursorRow),a
	  ld a,1
	  ld (cursorCol),a
	  ld a,MAGENTA
	  push de
	   ACALL(PutStringColor)
	   ld (current_item_ptr),hl
	   ld a,WHITE
	   jr draw_menu_item
_
	  xor a
	  cpir
	  ld a,(hl)
	  add a,a
	  jr nc,_
	  cp ITEM_DIGIT<<1
	  ld a,(ROMName+1)
	  call z,check_valid_load_state
	  or a
	  ld a,GRAY
	  jr z,draw_menu_item_push
_
	 pop bc
	 push bc
	  ; Update the "next item" offset
	  ld a,c
	  cp e
	  jr c,_
	  ld e,a
_
	  ; Update the "prev item" (minus 1) offset
	  dec a
	  cp d
	  jr nc,_
	  ld d,a
_
	  ld a,OLIVE
draw_menu_item_push:
	  push de
draw_menu_item:
	   call SetStringColor
	 
	   ld de,ItemDisplayCallbacks
	   ACALL(DoItemCallback)
	   inc de
	   
draw_menu_title:
	   ld a,(de)
	   inc de
	   ld (cursorRow),a
	   ld a,(de)
	   inc de
	   ld (cursorCol),a
	   push hl
	    push de
	     inc c
	     ld a,' '
	     jr nz,_
	     ld a,'*'
_
	     call PutChar
	     ACALL(PutStringFormat)
	    pop hl
	   pop de
	   xor a
	   ld c,a
	   cpir
	  pop de
	 pop bc
	 djnz draw_menu_loop
	pop af
	ld c,a
	jr z,_
	sub e
	ld (menuNextItem),a
	ld a,c
	scf
	sbc a,d
	ld (menuPrevItem),a
	ret
_
	; Draw the ROM list
	dec c
	ld a,(romListFrameCount)
	ld b,a
	jr z,_
	push bc
	 ld hl,romListItem
	 ld (current_item_ptr),hl
	 ld a,205
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
	 APTR(LoadRomHelpText)
	 ld a,MAGENTA
	 ACALL(PutStringColor)
	pop bc
	ld a,c
_
	ld (menuPrevItem),a
	
	ld a,25
	ld (cursorRow),a
	ld a,1
	ld (cursorCol),a
	add a,c
	cp b
	jr nz,_
	xor a
_
	inc a
	ld (menuNextItem),a
	
	ld a,(romListFrameStart)
	ld e,a
	add a,c
	dec a
	ld (romListItem+1),a
	ld d,3
	mlt de
	ld iy,romListStart
	add iy,de
	djnz draw_rom_list_loop
	APTR(LoadRomNoRomsText)
	ld a,GRAY
	AJUMP(PutStringColor)
	
draw_rom_list_loop:
	dec c
	push bc
	 ld a,OLIVE
	 jr nz,_
	 ld a,WHITE
_
	 call GetRomDescription
	 ACALL(PutNStringColor)
	
	 ld hl,cursorCol
	 ld (hl),1
	 inc hl
	 ld a,(hl)
	 add a,10
	 ld (hl),a
	  
	pop bc
	lea iy,iy+3
	djnz draw_rom_list_loop
	ret
	
draw_mini_screen:
	ld hl,mini_frame_backup
	ld de,menu_frame_buffer + (320*55 - 8)
	ld a,144
draw_mini_screen_row_loop:
	ld bc,160
	ex de,hl
	add hl,bc
	ex de,hl
	ldir
	dec a
	jr nz,draw_mini_screen_row_loop
	ret
	
	; Returns key code in A, or 0 if ON is pressed.
	; Also returns Z flag set if ON is pressed.
WaitForKey:
_
	ld de,$000010
	call ack_and_wait_for_interrupt
	ACALL(GetKeyCode)
	or a
	jr nz,-_
_
	ld de,$000010
	call ack_and_wait_for_interrupt
	ACALL(GetKeyCode)
	or a
	ret nz
	ld a,(mpIntMaskedStatus)
	or a
	jr z,-_
	ld (mpIntAcknowledge),a
	xor a
	ret
	
GetKeyCode:
	ld c,0
	ld b,56
	ld hl,mpKeypadGrp0
GetKeyCodeLoop:
	ld a,b
	and 7
	jr nz,_
	inc hl
	inc hl
	ld e,(hl)
_
	sla e
	jr nc,_
	xor a
	cp c
	ret nz
	ld c,b
_
	djnz GetKeyCodeLoop
	ld a,c
	ret
	
GetOption:
	ld hl,current_config
	ld bc,0
	ld c,a
	add a,a
	ld a,(hl)
	jr c,++_
	dec a
	ld hl,OptionConfig
	jr nz,_
	ld hl,GameOptionConfig
_
	add hl,bc
	ld a,(hl)
	cp $FF
	jr nz,_
	push bc
	 ld c,game_config_start - config_start
	 sbc hl,bc
	 ld a,(hl)
	 add hl,bc
	pop bc
_
	push hl
	 ld hl,OptionList
	 sla c
	 add hl,bc
	 ld bc,(ArcBase)
	 add hl,bc
	 ld hl,(hl)
	 dec.s hl
	 add hl,bc
	pop bc
	ret
	
	; Returns config pointer in HL, start pointer in BC
GetKeyConfig:
	or a
	sbc hl,hl
	ld l,a
	ld a,(current_config)
	or a
	ld bc,KeyConfig
	jr z,_
	ld bc,GameKeyConfig
_
	add hl,bc
	ret
	
ItemDisplayDigit:
	or a
	sbc hl,hl
	cp 2
	ld a,(current_state)
	jr nz,++_
	ld a,(current_config)
	or a
	ld a,(GameFrameskipValue)
	jr z,_
	cp $FF
	jr nz,++_
	ld c,a
_
	ld a,(FrameskipValue)
_
	ld l,a
ItemDisplayLink:
ItemDisplayCmd:
ItemChangeLink:
ItemChangeCmd:
ItemChangeKey:
ItemDeleteLink:
ItemDeleteCmd:
	ret
	
ItemDisplayKey:
	ACALL(GetKeyConfig)
	ld a,(calcType)
	or a
	jr z,_
	ld a,57
_
	ld c,(hl)
	inc c
	jr nz,_
	ld bc,game_config_start - config_start
	sbc hl,bc
	ld c,b
_
	dec c
	add a,(hl)
	push de
	 APTR(KeyNames)
	pop de
	jr ItemDisplayKeyEntry
	
ItemDisplayOption:
	ACALL(GetOption)
	inc hl
	push af
	 ld a,(bc)
	 ld c,a
	pop af
ItemDisplayKeyEntry:
	ld b,a
	or a
	ret z
	xor a
_
	cp (hl)
	inc hl
	jr nz,-_
	djnz -_
	ret
	
ConfirmText:
	.db "No\n",0
	.db "Yes",0
	
ConfirmDeleteROM:
	.db "Delete ROM files for %s?",0
	
ConfirmDeleteState:
	.db "Delete state slot %c?",0
	
ConfirmLoadState:
	.db "Load state slot %c?",0
	
ConfirmSaveState:
	.db "Overwrite state slot %c?",0
	
TitleChecksumFormat:
	.db "%.16s  %04X",0
	
MenuList:
	.dw MainMenu+1
	.dw LoadGameMenu+1
	.dw GraphicsMenu+1
	.dw ControlsMenu+1
	.dw EmulationMenu+1
	
OptionList:
	.dw OptionFrameskipType+1
	.dw OptionSpeedDisplay+1
	.dw OptionAutoSaveState+1
	.dw OptionPaletteSelection+1
	.dw OptionTimeZone+1
	.dw OptionDST+1
	.dw OptionScalingMode+1
	.dw OptionSkinDisplay+1
	.dw OptionTurboMode+1
	.dw OptionScalingType+1
	.dw OptionMessageDisplay+1
	.dw OptionAdjustColors+1
	.dw OptionConfirmState+1
	.dw OptionConfigSelect+1
	
CmdList:
	.dw CmdExit+1
	.dw CmdExit+1
	.dw CmdExit+1
	.dw CmdExit+1
	
ItemDisplayCallbacks:
	.dw ItemDisplayLink+1
	.dw ItemDisplayCmd+1
	.dw ItemDisplayDigit+1
	.dw ItemDisplayOption+1
	.dw ItemDisplayKey+1
	; No ROM item display callback
	
ItemChangeCallbacks:
	.dw ItemChangeLink+1
	.dw ItemChangeCmd+1
	.dw ItemChangeDigit+1
	.dw ItemChangeOption+1
	.dw ItemChangeKey+1
	.dw ItemChangeRom+1
	
ItemSelectCallbacks:
	.dw ItemSelectLink+1
	.dw ItemSelectCmd+1
	.dw ItemSelectDigit+1
	.dw ItemSelectOption+1
	.dw ItemSelectKey+1
	.dw ItemSelectRom+1
	
ItemDeleteCallbacks:
	.dw ItemDeleteLink+1
	.dw ItemDeleteCmd+1
	.dw ItemDeleteDigit+1
	.dw ItemDeleteOption+1
	.dw ItemDeleteKey+1
	.dw ItemDeleteRom+1
	
MainMenu:
	.db 10
	.db 5,8
	.db "TI-Boy CE Alpha v0.2.1\n https://calc84maniac.github.io/tiboyce",0
	.db "Select to load the game state from the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_DIGIT | ITEM_ROMONLY,0, 55,0,"Load State Slot %c",0
	.db "Select to save the game state to the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_DIGIT | ITEM_ROMONLY,1, 65,0,"Save State Slot %c",0
	.db "Choose configuration options to edit.\n Inherited global options show a '*'.\n Press DEL to delete a per-game option.",0
	.db ITEM_OPTION | ITEM_ROMONLY,option_config_count-$81, 85,0,"Config: %-8s",0
	.db "Select to set appearance and\n frameskip behavior.",0
	.db ITEM_LINK,2, 105,0,"Graphics Options",0
	.db "Select to change the in-game behavior\n of buttons and arrow keys.",0
	.db ITEM_LINK,3, 115,0,"Control Options",0
	.db "Select to manage miscellaneous options.",0
	.db ITEM_LINK,4, 125,0,"Emulation Options",0
	.db "Select to load a new game\n (will exit a currently playing game).",0
	.db ITEM_LINK,1, 145,0,"Load new game",0
	.db "Select to reset the Game Boy\n with the current game loaded.",0
	.db ITEM_CMD | ITEM_ROMONLY,3, 155,0,"Restart game",0
	.db "Select to exit this menu and\n resume gameplay.",0
	.db ITEM_CMD | ITEM_ROMONLY,0, 165,0,"Return to game",0
	.db "Select to exit the emulator and\n return to TI-OS.",0
	.db ITEM_CMD,2, 185,0,"Exit TI-Boy CE",0
	
NoRomLoadedDescription:
	.db _-$-1,"No ROM currently loaded"
_
	
LoadGameMenu:
	.db 1
	.db 10,14
	.db "Load New ROM",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 185,0,"Back",0
	
LoadRomHelpText:
	.db "Press 2nd/Enter to start the game.\n Press left/right to scroll pages.\n Press DEL to delete ROM files.",0
	
LoadRomNoRomsText:
	.db "No ROMs found!",0
	
GraphicsMenu:
	.db 10
	.db 10,12,"Graphics Options",0
	.db "",0
	.db ITEM_OPTION,6, 55,0,"Scaling mode: %-10s",0
	.db "Static: Scale absolutely.\n Scrolling: Scale relative to tilemap.",0
	.db ITEM_OPTION,9, 65,0,"Scaling type: %-9s",0
	.db "Display a skin in \"no scaling\" mode.\n Requires the TIBoySkn.8xv AppVar.",0
	.db ITEM_OPTION,7, 75,0,"Skin display: %-3s",0
	.db "Off: Do not skip any frames.\n Auto: Skip up to N frames as needed.\n Manual: Render 1 of each N+1 frames.",0
	.db ITEM_OPTION,0, 95,0,"Frameskip type: %-6s",0
	.db "",0
	.db ITEM_DIGIT,2, 105,0,"Frameskip value: %u",0
	.db "Show percentage of real GB performance.\n Turbo: Display when turbo is activated.\n Slowdown: Display when below fullspeed.",0
	.db ITEM_OPTION,1, 125,0,"Speed display: %-8s",0
	.db "Display emulator message overlays.",0
	.db ITEM_OPTION,10, 135,0,"Message display: %-3s",0
	.db "Default: Use GBC game-specific palette.\n Others: Use GBC manual palette.",0
	.db ITEM_OPTION,3, 155,0,"Palette selection: %-10s",0
	.db "Off: Use specified colors directly.\n On: Adjust to emulate a GBC display.",0
	.db ITEM_OPTION,11, 165,0,"Adjust colors: %-3s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 185,0,"Back",0
	
ControlsMenu:
	.db 16
	.db 10,12,"Control Options",0
	.db "",0
	.db ITEM_KEY,1,  55,0,"Right: %-9s",0
	.db "",0
	.db ITEM_KEY,2,  65,0,"Left:  %-9s",0
	.db "",0
	.db ITEM_KEY,3,  75,0,"Up:    %-9s",0
	.db "",0
	.db ITEM_KEY,4,  85,0,"Down:  %-9s",0
	.db "",0
	.db ITEM_KEY,5, 55,20,"A:      %-9s",0
	.db "",0
	.db ITEM_KEY,6, 65,20,"B:      %-9s",0
	.db "",0
	.db ITEM_KEY,7, 75,20,"Select: %-9s",0
	.db "",0
	.db ITEM_KEY,8, 85,20,"Start:  %-9s",0
	.db "Open the emulator menu.",0
	.db ITEM_KEY,9, 105,0,"Open menu:       %-9s",0
	.db "Enable or toggle turbo mode.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,0, 115,0,"Turbo mode:      %-9s",0
	.db "Save state to the current slot.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,10, 125,0,"Save state:      %-9s",0
	.db "Load state from the current slot.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,11, 135,0,"Load state:      %-9s",0
	.db "Show or select the current state slot.\n Press a number while holding to select.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,12, 145,0,"State slot:      %-9s",0
	.db "Turn screen brightness up.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,13, 155,0,"Brightness up:   %-9s",0
	.db "Turn screen brightness down.\n Press DEL to unmap this key.",0
	.db ITEM_KEY,14, 165,0,"Brightness down: %-9s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 185,0,"Back",0
	
EmulationMenu:
	.db 6
	.db 10,11,"Emulation Options",0
	.db "Automatically save state on ROM exit.\n State will be resumed upon next load.",0
	.db ITEM_OPTION,2, 55,0,"Auto save state: %-3s",0
	.db "",0
	.db ITEM_OPTION,12, 65,0,"Confirm state save/load: %-9s",0
	.db "",0
	.db ITEM_OPTION,8, 75,0,"Turbo mode: %-6s",0
	.db "The time offset for games with clocks.\n Should match the time set in the OS.\n Relevant when sharing save files.",0
	.db ITEM_OPTION,4, 95,0,"Time zone: UTC%-6s",0
	.db "Set to on if DST is currently active.",0
	.db ITEM_OPTION,5, 105,0,"Daylight Saving Time: %-3s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 185,0,"Back",0
	
OptionConfigSelect:
	.db 2
	.db "global",0
	.db "per-game",0
	
OptionFrameskipType:
	.db 3
	.db "manual",0
	.db "auto",0
	.db "off",0
	
OptionScalingMode:
	.db 2
	.db "no scaling",0
	.db "fullscreen",0
	
OptionAutoSaveState:
OptionDST:
OptionSkinDisplay:
OptionMessageDisplay:
OptionAdjustColors:
	.db 2
	.db "off",0
	.db "on",0
	
OptionConfirmState:
	.db 4
	.db "never",0
	.db "load only",0
	.db "overwrite",0
	.db "both",0
	
OptionTurboMode:
	.db 2
	.db "toggle",0
	.db "hold",0
	
OptionSpeedDisplay:
	.db 4
	.db "never",0
	.db "turbo",0
	.db "slowdown",0
	.db "always",0
	
OptionScalingType:
	.db 2
	.db "static",0
	.db "scrolling",0
	
OptionPaletteSelection:
	.db 14
	.db "default",0
	.db "grayscale",0
	.db "brown",0
	.db "pastel mix",0
	.db "blue",0
	.db "green",0
	.db "red",0
	.db "orange",0
	.db "dark blue",0
	.db "dark green",0
	.db "dark brown",0
	.db "yellow",0
	.db "inverted",0
	.db "classic",0
	
OptionTimeZone:
	.db 32
	.db "",0
	.db "+1:00",0
	.db "+2:00",0
	.db "+3:00",0
	.db "+3:30",0
	.db "+4:00",0
	.db "+4:30",0
	.db "+5:00",0
	.db "+5:30",0
	.db "+6:00",0
	.db "+6:30",0
	.db "+7:00",0
	.db "+8:00",0
	.db "+9:00",0
	.db "+9:30",0
	.db "+10:00",0
	.db "+11:00",0
	.db "+12:00",0
	.db "+13:00",0
	.db "-12:00",0
	.db "-11:00",0
	.db "-10:00",0
	.db "-9:00",0
	.db "-8:00",0
	.db "-7:00",0
	.db "-6:00",0
	.db "-5:00",0
	.db "-4:00",0
	.db "-3:30",0
	.db "-3:00",0
	.db "-2:00",0
	.db "-1:00",0
	
KeyNames:
	; 84 Plus CE keys
	.db "(press)",0
	.db "down",0
	.db "left",0
	.db "right",0
	.db "up",0
	.db 0,0,0,0
	.db "enter",0
	.db "+",0
	.db "-",0
	.db "x",0
	.db "div",0
	.db "^",0
	.db "clear",0
	.db 0
	.db "(-)",0
	.db "3",0
	.db "6",0
	.db "9",0
	.db ")",0
	.db "tan",0
	.db "vars",0
	.db 0
	.db ".",0
	.db "2",0
	.db "5",0
	.db "8",0
	.db "(",0
	.db "cos",0
	.db "prgm",0
	.db "stat",0
	.db "0",0
	.db "1",0
	.db "4",0
	.db "7",0
	.db ",",0
	.db "sin",0
	.db "apps",0
	.db "XT0n",0
	.db "(none)",0
	.db "sto>",0
	.db "ln",0
	.db "log",0
	.db "x^2",0
	.db "x^-1",0
	.db "math",0
	.db "alpha",0
	.db "graph",0
	.db "trace",0
	.db "zoom",0
	.db "window",0
	.db "y=",0
	.db "2nd",0
	.db "mode",0
	.db "del",0
	; 83 Premium CE keys
	.db "(appuyer)",0
	.db "bas",0
	.db "gauche",0
	.db "droite",0
	.db "haut",0
	.db 0,0,0,0
	.db "entrer",0
	.db "+",0
	.db "-",0
	.db "x",0
	.db "div",0
	.db "^",0
	.db "annul",0
	.db 0
	.db "(-)",0
	.db "3",0
	.db "6",0
	.db "9",0
	.db ")",0
	.db "frac",0
	.db "var",0
	.db 0
	.db ".",0
	.db "2",0
	.db "5",0
	.db "8",0
	.db "(",0
	.db "resol",0
	.db "prgm",0
	.db "stats",0
	.db "0",0
	.db "1",0
	.db "4",0
	.db "7",0
	.db ",",0
	.db "trig",0
	.db "matrice",0
	.db "XT0n",0
	.db "(rien)",0
	.db "sto>",0
	.db "ln",0
	.db "log",0
	.db "x^2",0
	.db "<>",0
	.db "math",0
	.db "alpha",0
	.db "graphe",0
	.db "trace",0
	.db "zoom",0
	.db "fenetre",0
	.db "f(x)",0
	.db "2nde",0
	.db "mode",0
	.db "suppr",0
	
KeySMCList:
	.db key_smc_right - key_smc_turbo - 1
	.db key_smc_left - key_smc_right - 1
	.db key_smc_up - key_smc_left - 1
	.db key_smc_down - key_smc_up - 1
	.db key_smc_a - key_smc_down - 1
	.db key_smc_b - key_smc_a - 1
	.db key_smc_select - key_smc_b - 1
	.db key_smc_start - key_smc_select - 1
	.db key_smc_menu - key_smc_start - 1
	.db key_smc_save_state - key_smc_menu - 1
	.db key_smc_load_state - key_smc_save_state - 1
	.db key_smc_state_slot - key_smc_load_state - 1
	.db key_smc_brightness_up - key_smc_state_slot - 1
	.db key_smc_brightness_down - key_smc_brightness_up - 1
	
TimeZoneOffsetTable:
	.db $00,$10,$20,$30,$38,$40,$48,$50,$58,$60,$68,$70,$80,$90,$98,$A0,$B0,$C0,$D0
	.db $C0,$B0,$A0,$90,$80,$70,$60,$50,$40,$38,$30,$20,$10
	
ManualPaletteIndexTable:
	.db $16,$12,$17,$B8,$05,$B0,$07,$AD,$7C,$79,$BA,$13
	.db $1D ;Classic
	
PaletteIndex:
	.db $80,$B0,$40, $88,$20,$68, $DE,$00,$70, $DE,$20,$78
	.db $20,$20,$38, $20,$B0,$90, $20,$B0,$A0, $E0,$B0,$C0
	.db $98,$B6,$48, $80,$E0,$50, $1E,$1E,$58, $20,$B8,$E0
	.db $88,$B0,$10, $20,$00,$10, $20,$E0,$18, $E0,$18,$00
	.db $18,$E0,$20, $A8,$E0,$20, $18,$E0,$00, $20,$18,$D8
	.db $C8,$18,$E0, $00,$E0,$40, $28,$28,$28, $18,$E0,$60
	.db $20,$18,$E0, $00,$00,$08, $E0,$18,$30, $D0,$D0,$D0
	.db $20,$E0,$E8, $F0,$F0,$F0 ; Classic
	
PaletteDictionary:
	.dw $7FFF,$32BF,$00D0,$0000
	.dw $639F,$4279,$15B0,$04CB
	.dw $7FFF,$6E31,$454A,$0000
	.dw $7FFF,$1BEF,$0200,$0000
	.dw $7FFF,$421F,$1CF2,$0000
	.dw $7FFF,$5294,$294A,$0000
	.dw $7FFF,$03FF,$012F,$0000
	.dw $7FFF,$03EF,$01D6,$0000
	.dw $7FFF,$42B5,$3DC8,$0000
	.dw $7E74,$03FF,$0180,$0000
	.dw $67FF,$77AC,$1A13,$2D6B
	.dw $7ED6,$4BFF,$2175,$0000
	.dw $53FF,$4A5F,$7E52,$0000
	.dw $4FFF,$7ED2,$3A4C,$1CE0
	.dw $03ED,$7FFF,$255F,$0000
	.dw $036A,$021F,$03FF,$7FFF
	.dw $7FFF,$01DF,$0112,$0000
	.dw $231F,$035F,$00F2,$0009
	.dw $7FFF,$03EA,$011F,$0000
	.dw $299F,$001A,$000C,$0000
	.dw $7FFF,$027F,$001F,$0000
	.dw $7FFF,$03E0,$0206,$0120
	.dw $7FFF,$7EEB,$001F,$7C00
	.dw $7FFF,$3FFF,$7E00,$001F
	.dw $7FFF,$03FF,$001F,$0000
	.dw $03FF,$001F,$000C,$0000
	.dw $7FFF,$033F,$0193,$0000
	.dw $0000,$4200,$037F,$7FFF
	.dw $7FFF,$7E8C,$7C00,$0000
	.dw $7FFF,$1BEF,$6180,$0000
	.dw $4778,$3290,$1D87,$0861 ; Classic