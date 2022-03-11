.STR_START:
	.string	"start"
.STR_END:
	.string	"%d\nend\n"
.STR_COUNT:
	.string	"count %d\n\n"

	.globl	main
main:
.LFB31:
	# puts("start");
	leaq	.STR_START(%rip), %rdi
	call	puts@PLT

	# sieve[0] = 0b11;
	movb	$3, sieve(%rip)

	# uint i = 1;
	mov 	$1, %esi

	# rdi = sieve;
	leaq	sieve(%rip), %rdi

	# r9d = 1;
	mov 	$1, %r9d


	jmp	.L2
.L3:
	add 	$1, %esi
.L2:
	cmp 	$100000001, %esi
	je	.L19

	# edx = sieve[esi >> 3];
	movl	%esi, %eax
	shrl	$3, %eax
	movzbl	(%rdi, %rax), %edx

	# if(edx & (1 << (si & 7))) continue;
	mov 	%si, %ax
	and 	$7, %al
	bt	 	%ax, %dx
	jc	.L3
	
	# rax = esi * esi // rsi = esi
	mov 	%esi, %eax
	imulq	%rsi, %rax

	# if (rax > 99999999) continue;
	cmp 	$99999999, %eax
	ja	.L3
.L4:
	# edx = eax >> 3;
	movl	%eax, %edx
	shrl	$3, %edx

	# cl = ax & 7;
	mov 	%ax, %cx
	and 	$7, %cl
	
	# eax += esi;
	addl	%esi, %eax

	# r10b = 1 << cl;
	mov 	$1, %r10b
	sal 	%cl, %r10b
	
	# sieve[rdx] |= r10b;
	orb	%r10b, (%rdi,%rdx)

	cmp 	$100000000, %eax
	jbe	.L4
	jmp	.L3



.L19:
	# rcx = primes;
	leaq	primes(%rip), %rcx

	# edx = 0; ebx = -1;
	xorl	%edx, %edx
	movl	$-1, %ebx
.L14:
	# al = eax = *rdi;
	movzbl	(%rdi), %eax

	# if (al & 1) goto L6;
	testb	$1, %al
	jne	.L6

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = edx;
	lea 	1(%ebx), %r10d
	mov 	%r10d, %ebx
	movl	%edx, (%rcx,%r10,4)
.L6:
	# if (al & 2) goto L7;
	testb	$2, %al
	jne	.L7

	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 1;
	lea 	1(%ebx), %r10d
	lea 	1(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L7:
	testb	$4, %al
	jne	.L8
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 2;
	lea 	1(%ebx), %r10d
	lea 	2(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L8:
	testb	$8, %al
	jne	.L9
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 3;
	lea 	1(%ebx), %r10d
	lea 	3(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L9:
	testb	$16, %al
	jne	.L10
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 4;
	lea 	1(%ebx), %r10d
	lea 	4(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L10:
	testb	$32, %al
	jne	.L11
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 5;
	lea 	1(%ebx), %r10d
	lea 	5(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L11:
	testb	$64, %al
	jne	.L12
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 6;
	lea 	1(%ebx), %r10d
	lea 	6(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L12:
	testb	%al, %al
	js	.L13
	
	# ebx = r10d = ebx + 1;
	# primes[r10 = r10d] = r8d = edx + 7;
	lea 	1(%ebx), %r10d
	lea 	7(%edx), %r8d
	mov 	%r10d, %ebx
	movl	%r8d, (%rcx,%r10,4)
.L13:
	# ++rdi; // ++sieve;
	addq	$1, %rdi

	# edx += 8;
	addl	$8, %edx
	cmpl	$100000000, %edx
	jne	.L14


	# eax = ebx;
	movl	%ebx, %eax

	# edx = primes[rax = eax];
	movl	(%rcx,%rax,4), %edx
	# rsi = STR_END;
	leaq	.STR_END(%rip), %rsi
	# printf
	xorl	%eax, %eax
	call	__printf_chk@PLT

	# printf(STR_COUNT)
	leaq	.STR_COUNT(%rip), %rsi
	movl	%ebx, %edx
	xorl	%eax, %eax
	call	__printf_chk@PLT

	# return 0;
	xorl	%eax, %eax
	ret

	.bss
primes:
	.zero	400000004
sieve:
	.zero	100000001
