#define ITEM_LINK 0
#define ITEM_CMD 1
#define ITEM_DIGIT 2
#define ITEM_OPTION 3
#define ITEM_KEY 4

ApplyConfiguration:
	; Display next frame always
	ld a,1
	ld (render_this_frame),a
	
	; Frameskip value
	ld hl,FrameskipValue
	ld a,(hl)
	inc a
	ld (frameskip_value_smc),a
	ld (skippable_frames),a
	inc hl
	
	; Frameskip type
	ld a,(hl)
	dec a
	ld a,$18	;JR
	jr nz,_
	ld a,$28	;JR Z
_
	ld (frameskip_type_smc),a
	ld a,(hl)
	or a
	jr z,_
	ld a,no_frameskip - (frameskip_type_smc+2)
_
	ld (frameskip_type_smc+1),a
	inc hl
	
	; FPS display
	ld a,(hl)
	dec a
	and $08
	or $20
	ld (fps_display_smc),a
	inc hl
	
	; Auto-Archive
	inc hl
	
	; Key configuration
	ld ix,key_smc_right
	push hl
	 APTR(KeySMCList)
	 ex de,hl
	pop hl
	ld b,9
key_config_loop:
	ld a,(hl)
	dec a
	cpl
	and %00111000
	rrca
	rrca
	ld (ix+2),a
	ld a,(hl)
	inc hl
	dec a
	and %00000111
	add a,a
	add a,a
	add a,a
	add a,$46
	ld (ix+3),a
	ld a,(de)
	inc de
	add a,ixl
	jr nc,_
	ld ixl,$FF
	inc ix
_
	ld ixl,a
	djnz key_config_loop
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
	
ItemSelectKey:
	ACALL(GetKeyConfig)
	ld (hl),0
	push hl
	 ACALL(draw_current_menu)
	 ACALL(WaitForKey)
	pop hl
	ld (hl),a
	ACALL(draw_current_menu)
	AJUMP(menu_loop)
	
BackToMainMenu:
	xor a
ItemSelectLink:
	ld (current_menu),a
	or a
	ld hl,current_menu_selection
	ld a,(main_menu_selection)
	jr z,_
	ld a,(hl)
	ld (main_menu_selection),a
	ld a,1
_
	ld (current_menu_selection),a
	ACALL(redraw_current_menu)
ItemSelectOption:
CmdLoadNewGame:
CmdRestartGame:
	jr menu_loop
	
ItemSelectDigit:
	cp 2
	jr z,menu_loop
	jr menu_loop
	
emulator_menu:
	push bc
	 xor a
	 ld (current_menu),a
	 ld a,(main_menu_selection)
	 ld (current_menu_selection),a
	 
	 ACALL(redraw_current_menu)
	
	 ld hl,(mpLcdBase)
	 push hl
	  ld hl,(current_buffer)
	  ld (mpLcdBase),hl
	  
menu_loop:
	  ACALL(WaitForKey)
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
	  cp 15-1
	  jr nz,menu_loop
	  
menu_exit:
	  ld a,(current_menu)
	  or a
	  jr nz,BackToMainMenu
	  
CmdReturnToGame:
	  ld a,(current_menu_selection)
	  ld (main_menu_selection),a

	  ACALL(ApplyConfiguration)

_
	  ACALL(GetKeyCode)
	  or a
	  jr nz,-_
	 pop hl
	 ld (mpLcdBase),hl
	pop bc
	ret
	
menu_up:
	ld a,c
	dec a
	jr nz,_
	ld a,(hl)
_
	ld (current_menu_selection),a
	ACALL(draw_current_menu)
	jr menu_loop
	
menu_down:
	ld a,c
	cp (hl)
	jr nz,_
	xor a
_
	inc a
	ld (current_menu_selection),a
	ACALL(draw_current_menu)
	jr menu_loop
	
menu_left_right:
	dec a
	add a,a
	dec a
	ld de,ItemChangeCallbacks
	ACALL(DoCurrentItemCallback)
	jr menu_loop
	
menu_select:
	ld de,ItemSelectCallbacks
DoCurrentItemCallback:
	ld hl,(current_item_ptr)
DoItemCallback:
	ld c,(hl)
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
	ld a,(de)
	jp (hl)
	

ItemChangeDigit:
	ld hl,current_state
	cp 2
	jr nz,_
	ld hl,FrameskipValue
_
	ld a,(hl)
	add a,b
	cp 10
	jr c,_
	add a,10
	jr c,_
	xor a
_
	ld (hl),a
	jr draw_current_menu
	
ItemChangeOption:
	ld d,b
	ACALL(GetOption)
	ld a,(bc)
	add a,d
	cp (hl)
	jr c,_
	add a,(hl)
	jr c,_
	xor a
_
	ld (bc),a
	jr draw_current_menu
	
redraw_current_menu:
	ld hl,(current_buffer)
	push hl
	pop de
	inc de
	ld bc,160*240-1
	ld (hl),BLUE_BYTE
	ldir
	
	ld a,(current_menu)
	or a
	jr nz,_
	ACALL(draw_mini_screen)
_
	
	ld a,20
	ld (cursorRow),a
	ld a,1
	ld (cursorCol),a
	ld hl,(current_description)
	ld a,MAGENTA
	ACALL(PutNStringColor)
	 
	ld a,30
	ld (cursorRow),a
	ld a,1
	ld (cursorCol),a
	ld ix,(rom_start)
	ld bc,$0134
	add ix,bc
	ld b,(ix+$014E-$0134)
	ld c,(ix+$014F-$0134)
	push bc
	 push ix
	  APTR(TitleChecksumFormat)
	  push hl
	   ACALL(PutStringFormat)
	  pop hl
	 pop hl
	pop hl
	
draw_current_menu:
	ld hl,(current_buffer)
	ld de,160*205
	add hl,de
	push hl
	pop de
	inc de
	ld bc,160*30-1
	ld (hl),BLUE_BYTE
	ldir
	
	call get_current_menu_selection
	
	; HL = menu structure, C = highlighted item index
draw_menu:
	ld b,(hl)
	inc hl
	inc b
	push bc
	 ld a,WHITE
	 call SetStringColor
	 ex de,hl
	 jr draw_menu_title
draw_menu_loop:
	dec c
	push bc
	 jr z,_
	 xor a
	 cpir
	 ld a,OLIVE
	 jr ++_
_
	 ld a,205
	 ld (cursorRow),a
	 ld a,1
	 ld (cursorCol),a
	 ld a,MAGENTA
	 ACALL(PutStringColor)
	 ld (current_item_ptr),hl
	 ld a,WHITE
_
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
	   ACALL(PutStringFormat)
	  pop hl
	 pop de
	 xor a
	 ld c,a
	 cpir
	pop bc
	djnz draw_menu_loop
	ret
	
draw_mini_screen:
	ld hl,160*(120-72) + 80 - 4
	ld de,(current_buffer)
	add hl,de
	ld a,d
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld d,a
	ld bc,80*256 + 144
	ld a,2
draw_mini_screen_row_loop:
	push bc
	 ld c,a
draw_mini_screen_pixel_loop:
	 ld a,(de)
	 ld (hl),a
	 inc de
	 ld a,(de)
	 rld
	 inc de
	 inc hl
	 djnz draw_mini_screen_pixel_loop
	 ld a,c
	 ld c,80
	 add hl,bc
	 dec a
	 jr nz,_
	 ld a,3
	 jr ++_
_
	 ex de,hl
	 add hl,bc
	 add hl,bc
	 ex de,hl
_
	pop bc
	dec c
	jr nz,draw_mini_screen_row_loop
	ret
	
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
	jr z,-_
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
	ld hl,OptionConfig
	ld bc,0
	ld c,a
	add hl,bc
	push hl
	 ld hl,OptionList
	 ld b,2
	 mlt bc
	 add hl,bc
	 ld bc,(ArcBase)
	 add hl,bc
	 ld hl,(hl)
	 dec.s hl
	 add hl,bc
	pop bc
	ret
	
GetKeyConfig:
	or a
	sbc hl,hl
	ld l,a
	ld bc,KeyConfig
	add hl,bc
	ret
	
ItemDisplayDigit:
	or a
	sbc hl,hl
	cp 2
	ld a,(current_state)
	jr nz,_
	ld a,(FrameskipValue)
_
	ld l,a
ItemDisplayLink:
ItemDisplayCmd:
ItemChangeLink:
ItemChangeCmd:
ItemChangeKey:
	ret
	
ItemDisplayKey:
	ACALL(GetKeyConfig)
	ld a,(hl)
	ld hl,KeyNames
	jr ItemDisplayKeyEntry
	
ItemDisplayOption:
	ACALL(GetOption)
	inc hl
	ld a,(bc)
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
	
TitleChecksumFormat:
	.db "%.16s  %04X",0
	
MenuList:
	.dw MainMenu+1
	.dw GraphicsMenu+1
	.dw ControlsMenu+1
	.dw EmulationMenu+1
	
OptionList:
	.dw OptionFrameskipType+1
	.dw OptionFPSDisplay+1
	.dw OptionAutoArchive+1
	
CmdList:
	.dw CmdExit+1
	.dw CmdExit+1
	.dw CmdExit+1
	.dw CmdReturnToGame+1
	
ItemDisplayCallbacks:
	.dw ItemDisplayLink+1
	.dw ItemDisplayCmd+1
	.dw ItemDisplayDigit+1
	.dw ItemDisplayOption+1
	.dw ItemDisplayKey+1
	
ItemChangeCallbacks:
	.dw ItemChangeLink+1
	.dw ItemChangeCmd+1
	.dw ItemChangeDigit+1
	.dw ItemChangeOption+1
	.dw ItemChangeKey+1
	
ItemSelectCallbacks:
	.dw ItemSelectLink+1
	.dw ItemSelectCmd+1
	.dw ItemSelectDigit+1
	.dw ItemSelectOption+1
	.dw ItemSelectKey+1
	
MainMenu:
	.db 9
	.db 5,10,"TI-Boy CE Alpha 0.01",0
	.db "Select to set appearance and\n frameskip behavior.",0
	.db ITEM_LINK,1, 50,1,"Graphics Options",0
	.db "Select to load the game state from the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_DIGIT,0, 70,1,"Load State Slot %u",0
	.db "Select to save the game state to the\n current slot for this game.\n Press left/right to change the slot.",0
	.db ITEM_DIGIT,1, 80,1,"Save State Slot %u",0
	.db "Select to change the in-game behavior\n of buttons and arrow keys.",0
	.db ITEM_LINK,2, 100,1,"Control Options",0
	.db "Select to manage miscellaneous options.",0
	.db ITEM_LINK,3, 120,1,"Emulation Options",0
	.db "Select to load a new game\n (will exit a currently playing game).",0
	.db ITEM_CMD,0, 140,1,"Load new game",0
	.db "Select to reset the Game Boy\n with the current game loaded.",0
	.db ITEM_CMD,2, 150,1,"Restart game",0
	.db "Select to exit this menu and\n resume gameplay.",0
	.db ITEM_CMD,3, 160,1,"Return to game",0
	.db "Select to exit the emulator and\n return to TI-OS.",0
	.db ITEM_CMD,1, 180,1,"Exit TI-Boy CE",0
	
GraphicsMenu:
	.db 4
	.db 5,12,"Graphics Options",0
	.db "Off: Do not skip any frames.\n Auto: Skip up to N frames as needed.\n Manual: Render 1 of each N+1 frames.",0
	.db ITEM_OPTION,0, 70,1,"Frameskip type: %-9s",0
	.db "",0
	.db ITEM_DIGIT,2, 80,1,"Frameskip value: %u",0
	.db "",0
	.db ITEM_OPTION,1, 100,1,"FPS display: %-3s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 160,1,"Back",0
	
ControlsMenu:
	.db 10
	.db 5,12,"Control Options",0
	.db "",0
	.db ITEM_KEY,0,  50,1,"Right:  %-7s",0
	.db "",0
	.db ITEM_KEY,1,  60,1,"Left:   %-7s",0
	.db "",0
	.db ITEM_KEY,2,  70,1,"Up:     %-7s",0
	.db "",0
	.db ITEM_KEY,3,  80,1,"Down:   %-7s",0
	.db "",0
	.db ITEM_KEY,4,  90,1,"A:      %-7s",0
	.db "",0
	.db ITEM_KEY,5, 100,1,"B:      %-7s",0
	.db "",0
	.db ITEM_KEY,6, 110,1,"Select: %-7s",0
	.db "",0
	.db ITEM_KEY,7, 120,1,"Start:  %-7s",0
	.db "",0
	.db ITEM_KEY,8, 140,1,"Menu:   %-7s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 160,1,"Back",0
	
EmulationMenu:
	.db 2
	.db 5,11,"Emulation Options",0
	.db "Enable to automatically archive saves.",0
	.db ITEM_OPTION,2, 50,1,"Auto-Archive: %-3s",0
	.db "Return to the main menu.",0
	.db ITEM_LINK,0, 160,1,"Back",0
	
OptionFrameskipType:
	.db 3
	.db "manual",0
	.db "automatic",0
	.db "off",0
	
OptionFPSDisplay:
OptionAutoArchive:
	.db 2
	.db "off",0
	.db "on",0
	
KeyNames:
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
	.db 0
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
	
KeySMCList:
	.db key_smc_left - key_smc_right
	.db key_smc_up - key_smc_left
	.db key_smc_down - key_smc_up
	.db key_smc_a - key_smc_down
	.db key_smc_b - key_smc_a
	.db key_smc_select - key_smc_b
	.db key_smc_start - key_smc_select
	.db key_smc_menu - key_smc_start
	.db 0