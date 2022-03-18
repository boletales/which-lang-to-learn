.STR_START:
	.string	"start"
.STR_END:
	.string	"%d\nend\n"
.STR_COUNT:
	.string	"count_1 %d\n\n"

	.globl	main


# https://www.mztn.org/lxasm64/amd04.html
#  64   32   16   8l
# rax  eax   ax   al
# rbx  ebx   bx   bl
# rcx  ecx   cx   cl
# rdx  edx   dx   dl
# rsi  esi   si  sil
# rdi  edi   di  dil
# rbp  ebp   bp  bpl
# rsp  esp   sp  spl
# r8   r8d  r8w  r8b
# r9   r9d  r9w  r9b
# r10 r10d r10w r10b
# r11 r11d r11w r11b
# r12 r12d r12w r12b
# r13 r13d r13w r13b
# r14 r14d r14w r14b
# r15 r15d r15w r15b



main:
	# puts("start");
	leaq	.STR_START(%rip), %rdi
	call	puts@PLT


	# eax = r8d = MAX;
	mov 	$100000000, %r8d
	mov 	%r8d, %eax
	
	# eax /= 2;
	shr  %eax
.ISQRT_L:
	# r11d = eax
	mov   %eax, %r11d

	# eax = MAX / r11d
	mov   %r8d, %eax
	xor   %edx, %edx
	div   %r11d

	# eax = (eax + r11d)/2
	add   %r11d, %eax
	shr   %eax

	# if(eax != r11d) goto ISQRT_L;
	cmp   %eax, %r11d
	ja   .ISQRT_L
	
	# ^^^  %r11d = sqrt(MAX)


	# sieve[0] = 0b11;
	movb	$3, sieve(%rip)

	# esi = 0;
	mov 	$0, %esi

	# rdi = sieve;
	leaq	sieve(%rip), %rdi

.L3:
	# if ((esi += 1) > sqrt(Max)) break;
	add 	$1, %esi
	cmp 	%r11d, %esi
	je	.L19

	# edx = sieve[esi >> 3];
	mov 	%esi, %eax
	shr 	$3, %eax
	movzb	(%rdi, %rax), %edx

	# if(edx & (1 << (si & 7))) continue;
	mov 	%si, %ax
	and 	$7, %al
	bt	 	%ax, %dx
	jc	.L3
	
	# rax = esi * esi // rsi = esi
	mov 	%esi, %eax
	imul	%esi, %eax

.L4:
	# edx = eax >> 3;
	mov 	%eax, %edx
	shr 	$3, %edx

	# cl = ax & 7;
	mov 	%ax, %cx
	and 	$7, %cl
	
	# eax += esi;
	add 	%esi, %eax

	# r10b = 1 << cl;
	mov 	$1, %r10b
	sal 	%cl, %r10b
	
	# sieve[rdx=edx] |= r10b;
	orb	%r10b, (%rdi,%rdx)

	# if (eax <= r8d) continue;
	cmp 	%r8d, %eax
	jbe 	.L4
	jmp 	.L3



.L19:
	# rcx = primes;
	leaq	primes(%rip), %rcx

	# edx = 0; ebx = -1;
	xor 	%edx, %edx
	mov 	$-1, %ebx
.L14:
	# al = eax = *rdi;
	movzb	(%rdi), %eax

	# if (al & 1) goto L6;
	testb	$1, %al
	jne	.L6

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = edx;
	lea 	1(%ebx), %r10d
	mov 	%r10d, %ebx
	mov 	%edx, (%rcx,%r10,4)
.L6:
	# if (al & 2) goto L7;
	testb	$2, %al
	jne	.L7

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 1;
	lea 	1(%ebx), %r10d
	lea 	1(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L7:
	testb	$4, %al
	jne	.L8
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 2;
	lea 	1(%ebx), %r10d
	lea 	2(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L8:
	testb	$8, %al
	jne	.L9
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 3;
	lea 	1(%ebx), %r10d
	lea 	3(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L9:
	testb	$16, %al
	jne	.L10
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 4;
	lea 	1(%ebx), %r10d
	lea 	4(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L10:
	testb	$32, %al
	jne	.L11
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 5;
	lea 	1(%ebx), %r10d
	lea 	5(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L11:
	testb	$64, %al
	jne	.L12
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 6;
	lea 	1(%ebx), %r10d
	lea 	6(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L12:
	testb	%al, %al
	js	.L13
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r9d = edx + 7;
	lea 	1(%ebx), %r10d
	lea 	7(%edx), %r9d
	mov 	%r10d, %ebx
	mov 	%r9d, (%rcx,%r10,4)
.L13:
	# ++rdi; // ++sieve;
	addq	$1, %rdi

	# edx += 8;
	add 	$8, %edx

	# if (edx <= MAX) continue;
	cmp 	%r8d, %edx
	jne	.L14


	# eax = ebx;
	mov 	%ebx, %eax

	# edx = primes[rax = eax];
	mov 	(%rcx,%rax,4), %edx
	# rsi = STR_END;
	leaq	.STR_END(%rip), %rsi
	# printf
	xor 	%eax, %eax
	call	__printf_chk@PLT

	# printf(STR_COUNT) // 5761454
	leaq	.STR_COUNT(%rip), %rsi
	mov 	%ebx, %edx
	xor 	%eax, %eax
	call	__printf_chk@PLT

	# return 0;
	xor 	%eax, %eax
	ret

	.bss
primes:
	.zero	400000004
sieve:
	.zero	100000001
