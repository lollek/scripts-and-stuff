section .data
	sys_read:	equ 3
	sys_write:	equ 4
	stdin:		equ 0
	stdout:		equ 1
	BUFSIZ		equ 100

section .text
	global _start

_start:
	; argc == 1 ? .print_stdid() : print_argvs()
	pop	eax
	cmp	eax, 1
	je	.print_stdin
	jmp	.print_argvs

; Print out each argv with a space after
.print_argvs:
	add	esp, 4 ; pop argv[0]
.print_argvs_loop:
	; Put next arg in eax, and exit if it's empty
	pop	eax
	cmp	eax, 0
	jz	.print_argvs_end
	; Rotate and print arg in eax
	mov	ebx, 0
	call	rot
	call	print
	; Print a space and loop
	mov	eax, ' '
	call	putchar
	jmp	.print_argvs_loop
.print_argvs_end:
	; Print newline and exit
	mov	eax, 10 ; '\n'
	call	putchar
	jmp	.end

; Print out all data from stdin
.print_stdin:
	sub	esp, BUFSIZ ; Push buffer
.print_stdin_loop:
	; Read BUFSIZ bytes from stdin
	mov	eax, sys_read
	mov	ebx, stdin
	mov	ecx, esp
	mov	edx, BUFSIZ
	int	80h
	; Exit if we've reached EOF
	cmp	eax, 1
	jle	.print_stdin_end
	; Rot all characters and print them to stdout
	mov	ebx, eax
	mov	eax, esp
	call	rot
	mov	edx, ebx
	mov	eax, sys_write
	mov	ebx, stdout
	mov	ecx, esp
	int	80h
	jmp	.print_stdin_loop
.print_stdin_end:
	add	esp, BUFSIZ ; Pop buffer
	jmp	.end

; Print newline and exit
.end:
	mov	eax, 1
	mov	ebx, 0
	int	80h

;; void rot(char *eax, int len)
; if len <= 0: rotates each char in string
; if len >  0: rotates len chars in string
rot:
	push	eax ; save original eax
	push	ebx ; save original ebx
	cmp	ebx, 0
	jle	.rot_loop_all
	jmp	.rot_n_chars
.rot_loop_all:
	; Run rotchar on all characters until we reach '\0'
	cmp	byte[eax], 0
	jz	.ret
	call	rotchar
	inc	eax
	jmp	.rot_loop_all
.rot_n_chars:
	call	rotchar
	inc	eax
	dec	ebx
	cmp	ebx, 0
	jz	.ret
	jmp	.rot_n_chars
.ret:
	pop	ebx ; restore ebx
	pop	eax ; restore eax
	ret

;; void rotchar(char *eax)
; rotates eax 13 steps if 'a' <= eax <= 'z' or 'A' <= eax <= 'Z'
rotchar:
	; Return if < 'A'
	cmp	byte[eax], 'A'
	jl	.noop
	; Rot + 13 if <= 'M'
	cmp	byte[eax], 'M'
	jle	.add13
	; Rot - 13 if <= 'Z'
	cmp	byte[eax], 'Z'
	jle	.sub13
	; Return if < 'a'
	cmp	byte[eax], 'a'
	jl	.noop
	; Rot + 13 if <= 'm'
	cmp	byte[eax], 'm'
	jle	.add13
	; Rot - 13 if <= 'z'
	cmp	byte[eax], 'z'
	jle	.sub13
	ret
.sub13:
	sub	byte[eax], 13
	ret
.add13:
	add	byte[eax], 13
	ret
.noop:
	ret

;; void print(char *eax)
; prints out string pointer to by eax
; DIRTIES EAX, EBX, ECX, EDX
print:
	mov	ecx, eax
	mov	edx, 0
.find_len:
	cmp	byte[ecx + edx], 0
	jz	.print
	inc	edx
	jmp	.find_len
.print:
	mov	eax, 4
	mov	ebx, 1
	int	80h
	ret

;; void putchar(char eax)
; prints out the char eax
; DIRTIES EAX, EBX, ECX, EDX
putchar:
	push	eax
	mov	eax, 4
	mov	ebx, 1
	mov	ecx, esp
	mov	edx, 1
	int	80h
	pop	eax
	ret
