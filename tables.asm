#macro OPCYCLE_NEXT
	ret c
	inc d
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
#endmacro

	.echo "Wasted space before tables: ", (203-$)&$FF
	.block (203-$)&255	

opcycleCB_hl:
	; Check for BIT
	jr z,opcycleCB_bit_hl
	; 2b op, variable rec, 4cc
	add ix,bc
	ld c,3
	add a,4
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
opcycleCB_bit_hl:
	; 2b op, 4b rec, 3cc
	lea ix,ix+4
	ld c,3
	add a,c
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)

opcycle_abs_read_write_special:
	add ix,bc
	ex de,hl
	; Check read vs. write
	bit 4,(hl)
	inc hl
	inc hl
	jr z,opcycle_abs_write_special
	; Check for port read
	ld e,(hl)
	inc hl
	inc e
	jr nz,opcycle_abs_read_write_special_finish
	; Port reads are 6 bytes
	lea ix,ix-2
	jr opcycle_abs_read_write_special_finish

opcycle_abs_write_special:
	; Check for MBC write
	bit 7,(hl)
	inc hl
	jr nz,opcycle_abs_read_write_special_finish
	; MBC writes are 4 bytes
	lea ix,ix-4
	jr opcycle_abs_read_write_special_finish

	.block (-$)&255
opcycleroutines:
opcycleCB:
	; Look up second opcode byte in opcoderecsizes_CB
	ex de,hl
	inc hl
	ld e,(hl)
	inc hl
	ex de,hl
	inc h
	inc h
	ld c,(hl)
	dec h
	ex de,hl
	ld e,(hl)
	; Check for (HL) access
	srl c
	jr c,opcycleCB_hl
	; 2b op, variable rec, 2cc
	add ix,bc
	ld c,3
	add a,2
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)

	; 3b op, variable rec, 4cc
opcycle_abs_read_write:
	bit.s 0,(ix)
	add ix,bc
	jr z,opcycle_abs_read_write_finish
	bit.s 5,(ix-3+1)
	lea ix,ix+5-3
	jr z,opcycle_abs_read_write_special
opcycle_abs_read_write_finish:
	ex de,hl
	add hl,bc
opcycle_abs_read_write_special_finish:
	add a,4
	OPCYCLE_NEXT

	; 1b op, 4b rec, 1cc
opcycleROT:
	add ix,bc
	; 1b op, 1b rec, 1cc
opcycle1byte:
	inc ix
	; 1b op, 0b rec, 1cc
opcycleNOP:
	inc de
	ex de,hl
	inc a
	ret z
	inc d
opcycle_first:
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)

	; 1b op, 2b rec, 1cc
opcycle1byte_ix:
	dec ix
	; 1b op, 3b rec, 1cc
opcycleROTC:
opcycleDAA:
opcycle3F:
opcycleDI:
opcycleEI:
	add ix,bc
	inc de
	ex de,hl
	inc a
	ret z
	inc d
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)

	; 3b op, 7b rec, 5cc
opcycle08:
	lea ix,ix+7
	ex de,hl
	add hl,bc
	add a,5
	jr opcycle_next

	; 2b op, 3b rec, 2cc
opcycle2byte_ix:
	inc de
	; 1b op, 3b rec, 2cc
opcycleMEM:
opcycle19:
opcycle29:
opcycle39:
opcycleF9:
	add ix,bc
	inc de
	ex de,hl
	add a,2
opcycle_next:
	OPCYCLE_NEXT

	; 1b op, 6b rec, 2cc
opcycleE2:
opcycleF2:
	inc ix
	; 1b op, 5b rec, 2cc
opcycle09:
	lea ix,ix+4
	; 1b op, 1b rec, 2cc
opcycle1byte_2cc:
	inc ix
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT

	; 1b op, 7b rec, 2cc
opcycleMEM_7b:
	add ix,bc
	; 1b op, 4b rec, 2cc
opcycle33:
opcycle3B:
opcycleMEM_4b:
	lea ix,ix+4
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT

	; 2b op, 2b rec, 2cc
opcycle2byte:
	inc de
	; 1b op, 2b rec, 2cc
opcycle1byte_2cc_ix:
	lea ix,ix+2
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT

	; 3b op, 7b rec, 3cc
opcycle31:
	add ix,bc
	; 3b op, 4b rec, 3cc
opcycle3byte_ix:
	inc ix
	; 3b op, 3b rec, 3cc
opcycle3byte:
	add ix,bc
	ex de,hl
	add hl,bc
	add a,c
	OPCYCLE_NEXT

	; 2b op, variable rec, 3cc
opcycleE0:
	bit.s 0,(ix)
	add ix,bc
	jr nz,opcycleE0_port
	inc de
	inc de
	ex de,hl
	add a,c
	OPCYCLE_NEXT

	; 2b op, variable rec, 3cc
opcycleF0:
	bit.s 0,(ix)
	add ix,bc
	inc de
	jr z,opcycleF0_normal
	jr opcycleF0_port

	; 2b op, 5b rec, 3cc
opcycleE0_port:
opcycleF8:
	inc ix
	; 2b op, 4b rec, 3cc
opcycle36:
	inc de
	; 1b op, 4b rec, 3cc
opcyclePOP:
	inc ix
	; 1b op, 3b rec, 3cc
opcycleF0_port:
opcycle34:
opcycle35:
opcycleF1:
	add ix,bc
opcycleF0_normal:
	inc de
	ex de,hl
	add a,c
	OPCYCLE_NEXT

	; 2b op, 5b rec, 4cc
opcycleE8:
	inc de
	lea ix,ix+2
	; 1b op, 3b rec, 4cc
opcyclePUSH:
	inc de
	add ix,bc
	ex de,hl
	add a,4
	OPCYCLE_NEXT

	; Bugged halt instruction
opcycleHALT:
	; Dispatch to the bugged instruction without incrementing the pointer
	ex de,hl
	inc d
	inc hl
	ld e,(hl)
	dec hl
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)

	; Invalid opcodes within a sub-block
opcycleINVALID:
opcycleJR:
opcycleJRcond:
opcycleJP:
opcycleJPcond:
opcycleCALL:
opcycleCALLcond:
opcycleRET:
opcycleRETcond:
opcycleRETI:
opcycleRST:
opcycleJPHL:
opcycleSTOP:
	jp runtime_error

	.echo "Opcycle routine size: ", $ - opcycleroutines
	.block 256 - ($ - opcycleroutines)

; A table indexing opcode cycle counting routines.
; All entry points live in a 256-byte space.
opcounttable:
;00
	.db opcycleNOP - opcycleroutines
	.db opcycle3byte_ix - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle2byte_ix - opcycleroutines
	.db opcycleROTC - opcycleroutines
;08
	.db opcycle08 - opcycleroutines
	.db opcycle09 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle2byte_ix - opcycleroutines
	.db opcycleROTC - opcycleroutines
;10
	.db opcycleSTOP - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;18
	.db opcycleJR - opcycleroutines
	.db opcycle19 - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;20
	.db opcycleJRcond - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleDAA - opcycleroutines
;28
	.db opcycleJRcond - opcycleroutines
	.db opcycle29 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
;30
	.db opcycleJRcond - opcycleroutines
	.db opcycle31 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle33 - opcycleroutines
	.db opcycle34 - opcycleroutines
	.db opcycle35 - opcycleroutines
	.db opcycle36 - opcycleroutines
	.db opcycle1byte - opcycleroutines
;38
	.db opcycleJRcond - opcycleroutines
	.db opcycle39 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle3B - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle3F - opcycleroutines
;40
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
;48
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
;50
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;58
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;60
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;68
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;70
	.db opcycleMEM_7b - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleHALT - opcycleroutines
	.db opcycleMEM - opcycleroutines
;78
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleNOP - opcycleroutines
;80
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;88
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;90
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;98
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A0
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A8
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B0
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B8
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;C0
	.db opcycleRETcond - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleJP - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;C8
	.db opcycleRETcond - opcycleroutines
	.db opcycleRET - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleCB - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleCALL - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;D0
	.db opcycleRETcond - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;D8
	.db opcycleRETcond - opcycleroutines
	.db opcycleRETI - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;E0
	.db opcycleE0 - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleE2 - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;E8
	.db opcycleE8 - opcycleroutines
	.db opcycleJPHL - opcycleroutines
	.db opcycle_abs_read_write - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;F0
	.db opcycleF0 - opcycleroutines
	.db opcycleF1 - opcycleroutines
	.db opcycleF2 - opcycleroutines
	.db opcycleDI - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;F8
	.db opcycleF8 - opcycleroutines
	.db opcycleF9 - opcycleroutines
	.db opcycle_abs_read_write - opcycleroutines
	.db opcycleEI - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines

; A table of recompiled bitwise opcode sizes.
; The value is scaled by 2, and bit 0 is set for (HL) accesses.
; BIT b,(HL) opcodes report a size of 0 for easy detection,
; but their actual size is 4 bytes.
opcoderecsizes_CB:
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 6,6,6,6,6,6,7,10
	.db 16,16,4,4,4,4,9,4

	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4

	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4

	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4

; A table of recompiled opcode sizes.
; Does not apply to block-ending opcodes or variable-length implementations.
; Currently CB, E0, EA, F0, and EA are variable-length.
; CALL opcodes are offset by 1 to make it easier to find sub-block cycles.
opcoderecsizes:
	.db 0,4,4,2,2,2,3,3
	.db 7,5,4,2,2,2,3,3
	.db 0,3,3,1,1,1,2,4
	.db 0,3,3,1,1,1,2,4
	.db 19,3,4,1,1,1,2,3
	.db 19,3,4,1,1,1,2,1
	.db 19,7,4,4,3,3,4,1
	.db 19,3,4,4,1,1,2,3

	.db 0,2,2,2,2,2,7,2
	.db 2,0,2,2,2,2,7,2
	.db 2,2,0,1,1,1,3,1
	.db 2,2,1,0,1,1,3,1
	.db 2,2,1,1,0,1,3,1
	.db 2,2,1,1,1,0,3,1
	.db 7,7,3,3,3,3,1,3
	.db 2,2,1,1,1,1,3,0

	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1

	.db 12,4,19,0,11-1,3,2,9
	.db 12,0,19,0,11-1,11-1,2,9
	.db 12,4,19,0,11-1,3,2,9
	.db 12,0,19,0,11-1,0,2,9
	.db 0,4,6,0,0,3,2,9
	.db 5,0,0,0,0,0,2,9
	.db 0,3,6,3,0,3,2,9
	.db 5,3,0,3,0,0,2,9

; A table of Game Boy opcode cycles. Block-ending opcodes are set to 0.
; Conditional branches are assumed not taken.
; Opcodes with variable-length implementations (e.g. CB) and HALT are set to -1,
; for efficient detection.
opcodecycles:
	.db 1,3,2,2,1,1,2,1
	.db 5,2,2,2,1,1,2,1
	.db 0,3,2,2,1,1,2,1
	.db 0,2,2,2,1,1,2,1
	.db 2,3,2,2,1,1,2,1
	.db 2,2,2,2,1,1,2,1
	.db 2,3,2,2,3,3,3,1
	.db 2,2,2,2,1,1,2,1

	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 2,2,2,2,2,2,-2,2
	.db 1,1,1,1,1,1,2,1

	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1

	.db 2,3,3,0,4,4,2,4
	.db 2,0,3,-1,4,4,2,4
	.db 2,3,3,0,4,4,2,4
	.db 2,0,3,0,4,0,2,4
	.db -1,3,2,0,0,4,2,4
	.db 4,0,-1,0,0,0,2,4
	.db -1,3,2,1,0,4,2,4
	.db 3,2,-1,1,0,0,2,4

; A table of Game Boy opcode sizes.
; The HALT opcode defaults to 3 bytes, to include the following opcode bytes
; for any instruction that could be affected by the HALT bug.
opcodesizes:
	.db 1,3,1,1,1,1,2,1
	.db 3,1,1,1,1,1,2,1
	.db 1,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1

	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,1,1

	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1

	.db 1,1,3,3,3,1,2,1
	.db 1,1,3,2,3,3,2,1
	.db 1,1,3,1,3,1,2,1
	.db 1,1,3,1,3,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,1,3,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,1,3,1,1,1,2,1

; A table indexing opcode generation routines.
; All entry points live in a 256-byte space.
opgentable:
;00
	.db opgenNOP - opgenroutines
	.db opgen01 - opgenroutines
	.db opgenBCwrite - opgenroutines
	.db opgen1byte_2cc_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen2byte_remap_ix - opgenroutines
	.db opgenROT - opgenroutines
;08
	.db opgen08 - opgenroutines
	.db opgen09 - opgenroutines
	.db opgenBCread - opgenroutines
	.db opgen1byte_2cc_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen2byte_remap_ix - opgenroutines
	.db opgenROT - opgenroutines
;10
	.db opgenHALT_STOP - opgenroutines
	.db opgen3byte_remap - opgenroutines
	.db opgenDEwrite - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgenROT - opgenroutines
;18
	.db opgenJR - opgenroutines
	.db opgen19 - opgenroutines
	.db opgenDEread - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgenROT - opgenroutines
;20
	.db opgenJRcond - opgenroutines
	.db opgen3byte_remap - opgenroutines
	.db opgenHLwrite_post - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgenDAA - opgenroutines
;28
	.db opgenJRcond - opgenroutines
	.db opgen29 - opgenroutines
	.db opgenHLread_post - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgen1byte - opgenroutines
;30
	.db opgenJRcond - opgenroutines
	.db opgen31 - opgenroutines
	.db opgenHLwrite_post - opgenroutines
	.db opgen33 - opgenroutines
	.db opgenHLreadwrite - opgenroutines
	.db opgenHLreadwrite - opgenroutines
	.db opgen36 - opgenroutines
	.db opgen1byte - opgenroutines
;38
	.db opgenJRcond - opgenroutines
	.db opgen39 - opgenroutines
	.db opgenHLread_post - opgenroutines
	.db opgen3B - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen3F - opgenroutines
;40
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenHLread_bc - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
;48
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenHLread_bc - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
;50
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;58
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;60
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;68
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;70
	.db opgenHLwrite_bc - opgenroutines
	.db opgenHLwrite_bc - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHALT_STOP - opgenroutines
	.db opgenHLwrite - opgenroutines
;78
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgenNOP - opgenroutines
;80
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;88
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;90
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;98
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;A0
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;A8
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;B0
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;B8
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;C0
	.db opgenRETcond - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenJP - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;C8
	.db opgenRETcond - opgenroutines
	.db opgenRET - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenCB - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenCALL - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D0
	.db opgenRETcond - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D8
	.db opgenRETcond - opgenroutines
	.db opgenRETI - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;E0
	.db opgenE0 - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenE2 - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;E8
	.db opgenE8 - opgenroutines
	.db opgenJPHL - opgenroutines
	.db opgenEA - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;F0
	.db opgenF0 - opgenroutines
	.db opgenF1 - opgenroutines
	.db opgenF2 - opgenroutines
	.db opgenDI - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;F8
	.db opgenF8 - opgenroutines
	.db opgenF9 - opgenroutines
	.db opgenFA - opgenroutines
	.db opgenEI - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines

; A supplemental table for opcode generation.
; Determines translations or routine addresses for certain opcodes.
	;00
	.db 0,  0,0,$23,$24,$25,$26,0
	.db 0,  0,0,$2B,$2C,$2D,$2E,0
	.db 0,$01,0,$03,$04,$05,$06,0
	.db 0,  0,0,$0B,$0C,$0D,$0E,0
	.db ophandler31 >> 8
	.db   $11,$13,$13,$14,$15,$16,ophandlerDAA & $FF
	.db ophandlerDAA >> 8
	.db     0,$13,$1B,$1C,$1D,$1E,ophandler31 & $FF
	;30
	.db 0
	.db 0
	.db $1B
	.db ophandler33 & $FF
	.db ophandler3B >> 8
	.db 0
	.db ophandler39 >> 8
	.db 0
	;38
	.db 0
	.db ophandler39 & $FF
	.db $1B
	.db ophandler3B & $FF
	.db ophandler33 >> 8
	.db 0
	.db 0
	.db 0

	;40
	.db   0,$65,$60,$61,$62,$63,$63,$67
	.db $6C,  0,$68,$69,$6A,$6B,$6B,$6F
	.db $44,$45,  0,$41,$42,$43,$46,$47
	.db $4C,$4D,$48,  0,$4A,$4B,$4E,$4F
	.db $54,$55,$50,$51,  0,$53,$56,$57
	.db $5C,$5D,$58,$59,$5A,  0,$5E,$5F
	.db $7C,$7D,$70,$71,$72,$73,  0,$77
	.db $7C,$7D,$78,$79,$7A,$7B,$7E,  0

	;80
	.db $84,$85,$80,$81,$82,$83,$86,0
	.db $8C,$8D,$88,$89,$8A,$8B,$8E,0
	.db $94,$95,$90,$91,$92,$93,$96,0
	.db $9C,$9D,$98,$99,$9A,$9B,$9E,0
	.db $A4,$A5,$A0,$A1,$A2,$A3,$A6,0
	.db $AC,$AD,$A8,$A9,$AA,$AB,$AE,0
	.db $B4,$B5,$B0,$B1,$B2,$B3,$B6,0
	.db $BC,$BD,$B8,$B9,$BA,$BB,$BE,0

	;C0
	.db 0
	.db ophandlerC1 & $FF
	.db 0
	.db 0
	.db 0
	.db ophandlerC5 & $FF
	.db ophandlerRET >> 8
	.db 0
	;C8
	.db 0
	.db ophandlerRET & $FF
	.db ophandlerC5 >> 8
	.db 0
	.db 0
	.db 0
	.db ophandlerC1 >> 8
	.db 0
	;D0
	.db ophandlerF8_zero & $FF
	.db ophandlerD1 & $FF
	.db 0
	.db 0
	.db 0
	.db ophandlerD5 & $FF
	.db ophandlerRETI >> 8
	.db 0
	;D8
	.db 0
	.db ophandlerRETI & $FF
	.db ophandlerD5 >> 8
	.db 0
	.db 0
	.db 0
	.db ophandlerD1 >> 8
	.db ophandlerF8_zero >> 8
	;E0
	.db ophandlerE8_positive & $FF
	.db ophandlerE1 & $FF
	.db op_write_c_hmem & $FF
	.db 0
	.db 0
	.db ophandlerE5 & $FF
	.db ophandlerJPHL >> 8
	.db ophandlerE8_negative >> 8
	;E8
	.db ophandlerE8_negative & $FF
	.db ophandlerJPHL & $FF
	.db ophandlerE5 >> 8
	.db 0
	.db 0
	.db op_write_c_hmem >> 8
	.db ophandlerE1 >> 8
	.db ophandlerE8_positive >> 8
	;F0
	.db ophandlerF8_positive & $FF
	.db ophandlerF1 & $FF
	.db op_read_c_hmem & $FF
	.db ophandlerDI & $FF
	.db ophandlerEI >> 8
	.db ophandlerF5 & $FF
	.db ophandlerF9 >> 8
	.db ophandlerF8_negative >> 8
	;F8
	.db ophandlerF8_negative & $FF
	.db ophandlerF9 & $FF
	.db ophandlerF5 >> 8
	.db ophandlerEI & $FF
	.db ophandlerDI >> 8
	.db op_read_c_hmem >> 8
	.db ophandlerF1 >> 8
	.db ophandlerF8_positive >> 8

rom_bank_fill_routines:
	.db 0
	.db rom_bank_fill_rem_0 - rom_bank_fill_routines
	.db rom_bank_fill_exactly_1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-21 - rom_bank_fill_routines

	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_0:
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp

	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_1:
	inc sp \ inc sp
	push bc
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
rom_bank_fill_fix_sp:
	ld hl,(rom_bank_base)
	jp.sis mbc_fix_sp

	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_2:
	inc sp
	push bc
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp

rom_bank_fill_exactly_1:
	ld hl,-1
	add hl,sp
	ld (hl),c
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp

	.block (rom_bank_fill_routines+192) - $
	.db rom_bank_fill_rem_0-21 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2 - rom_bank_fill_routines
	.db rom_bank_fill_exactly_1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0 - rom_bank_fill_routines
