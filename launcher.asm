; Some standalone equates
_Mov9ToOP1 = $020320
_chkFindSym = $02050C
_InsertMem = $020514
_ErrNotEnoughMem = $02072C
_ErrCustom1 = $02120C
_Arc_Unarc = $021448
_ChkInRam = $021F98
OP1 = $D005F8
tSymPtr1 = $D0257B
asm_prgm_size = $D0118C
appErr1 = $D025A9 ; use with _ErrCustom1
userMem = $D1A881
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

	.db tExtTok, tAsm84CeCmp
	.org userMem
	
	jp LookUpAppvar
	.db 1
	.db 16,16
	.db $FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF
	.db $FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
	.db $FF,$00,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$B5,$00,$FF,$00,$00
	.db $FF,$00,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00
	.db $FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
	.db $FF,$00,$FF,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
	.db $FF,$00,$FF,$00,$00,$00,$FF,$FF,$FF,$00,$FF,$00,$FF,$FF,$00,$00
	.db $FF,$00,$FF,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
	.db $FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00
	.db $FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF
	.db "TI-Boy CE",0
	
	.db $01,"EXEyoBIT"
ExeName:
	.db appVarObj
ErrorStartText:
	.db "TIBoyDat"
	.db " "
	.db "Invalid",0
	.db "Missing",0
	
AppvarInRam:
	call _Arc_Unarc
LookUpAppvar:
	ld hl,ExeName
	push hl
	 call _Mov9ToOP1
	 call _chkFindSym
	 ld (tSymPtr1),hl
	pop hl
	ld bc,9
	jr c,ErrorMissing
	call _ChkInRam
	jr z,AppvarInRam
	ex de,hl
	add hl,bc
	ld c,(hl)
	add hl,bc
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	xor a
	cp b
	ld b,a
	sbc a,a
	or c
	ld c,9
	cp c
	jr c,ErrorInvalid
	
MagicCheckLoop:
	dec de
	ld a,(de)
	cpi
	jr nz,ErrorInvalid
	jp pe,MagicCheckLoop
	
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	
	dec d
	ld e,userMem & $FF
	push hl
	 push de
	  push bc
	   ld hl,(asm_prgm_size)
	   add hl,de
	   ex de,hl
	   add hl,bc
	   sbc hl,de
	   push de
	    call _ErrNotEnoughMem
	    ex de,hl
	   pop de
	   call _InsertMem
	   ld hl,OP1
	   ld de,$E9B0ED	;LDIR \ JP (HL)
	   ld (hl),de
	  pop bc
	  ld (asm_prgm_size),bc
	 pop de
	 ex (sp),hl
	 ret
	
ErrorInvalid:
	ld c,9+8
	ex de,hl
ErrorMissing:
	ld l,ErrorStartText & $FF
	ld de,appErr1
	ldir
	ld c,8
	add hl,bc
	ldir
	jp _ErrCustom1

	.echo "Loader size is ",$-userMem," bytes"