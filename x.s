	.file	"x.c"
	.text
	.p2align 4,,15
.globl foo
	.type	foo, @function
foo:
	pushl	%ebp
	movl	%esp, %ebp
	movl	x+16, %eax
	movl	%eax, x+8
	popl	%ebp
	ret
	.size	foo, .-foo
	.comm	x,80,32
	.ident	"GCC: (GNU) 4.0.3 20051201 (prerelease) (Debian 4.0.2-5)"
	.section	.note.GNU-stack,"",@progbits
