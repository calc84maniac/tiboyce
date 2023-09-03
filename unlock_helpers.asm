; Copyright 2015-2022 Matt "MateoConLechuga" Waltz
; Copyright 2023 Brendan "calc84maniac" Fletcher
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
;
; 3. Neither the name of the copyright holder nor the names of its contributors
;    may be used to endorse or promote products derived from this software
;    without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

port_read = heapBot - 6
port_write = heapBot - 3

port_unlock:
	di
	ld hl,$C978ED
	ld (port_read),hl
	inc h
	ld (port_write),hl
port_unlock_code = $+1
	jp 0

port_lock:
port_lock_value = $+1
	ld a,$7C
	ld bc,$0020
	call port_write
	ei
	ret

port_old_unlockhelper:
	call __frameset0
	push de
	ld bc,$0022
port_old_target = $+1
	jp 0

port_new_unlockhelper:
	push hl
	ex (sp),ix
	add ix,sp
	push hl
	push de
	ld de,$887C00
	push de
	ld bc,$10DE
	ld de,$0F22
	add hl,sp
port_new_target = $+1
	jp 0
