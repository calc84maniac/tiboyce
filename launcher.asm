; Some standalone equates
_Mov9ToOP1 = $020320
_chkFindSym = $02050C
_InsertMem = $020514
_PutS = $0207C0
_NewLine = $0207F0
_Arc_Unarc = $021448
_ChkInRAM = $021F98
OP1 = $D005F8
asm_prgm_size = $D0118C
userMem = $D1A881
appVarObj = $15
tExtTok = $EF
tAsm84CeCmp = $7B

	.db tExtTok, tAsm84CeCmp
	.org userMem
	
LookUpAppvar:
	ld hl,ExeName
	call _Mov9ToOP1
	call _chkFindSym
	ld hl,ErrorMissingText
	jr c,Error
	call _ChkInRAM
	jr nz,AppvarFound
	call _Arc_Unarc
	jr LookUpAppvar
AppvarFound:
	ex de,hl
	ld bc,9
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
	ld a,c
	ld c,MagicHeaderEnd - MagicHeader
	jr nz,_
	cp c
	jr c,ErrorInvalid
_
	ld de,MagicHeader
_
	ld a,(de)
	inc de
	cpi
	jr nz,ErrorInvalid
	jp pe,-_
	
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	
	push hl
	 ld hl,userMem
	 push hl
	  push bc
	   ld de,(asm_prgm_size)
	   add hl,de
	   push bc
	    ex (sp),hl
	    sbc hl,de
	   pop de
	   call _InsertMem
	  pop bc
	  ld (asm_prgm_size),bc
	 pop de
	 ld hl,$E9B0ED	;LDIR \ JP (HL)
	 ld (OP1),hl
	pop hl
	jp OP1
	
ErrorInvalid:
	ld hl,ErrorInvalidText
Error:
	push hl
	 ld hl,ErrorGenericText
	 call _PutS
	pop hl
	call _PutS
	jp _NewLine
	
ExeName:
	.db appVarObj, "TIBoyCE",0
	
ErrorGenericText:
	.db "Error: TI-Boy CE Appvar   "
	.db "executable is ",0
	
ErrorMissingText:
	.db "missing!",0
	
ErrorInvalidText:
	.db "invalid!",0
	
MagicHeader:
	.db "TIBoyEXE"
MagicHeaderEnd:

	.echo "Loader size is ",$-userMem," bytes"