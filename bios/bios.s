	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	.align	4, 0x90
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	$503, %edx              ## imm = 0x1F7
	## InlineAsm Start
	inb	%dx, %al
	## InlineAsm End
	andb	$-64, %al
	movzbl	%al, %eax
	cmpl	$64, %eax
	jne	LBB0_1
## BB#2:
	movb	$1, %al
	movl	$498, %edx              ## imm = 0x1F2
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	xorl	%eax, %eax
	movl	$499, %edx              ## imm = 0x1F3
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	xorl	%eax, %eax
	movl	$500, %edx              ## imm = 0x1F4
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	xorl	%eax, %eax
	movl	$501, %edx              ## imm = 0x1F5
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	movb	$-32, %al
	movl	$502, %edx              ## imm = 0x1F6
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	movb	$32, %al
	movl	$503, %edx              ## imm = 0x1F7
	## InlineAsm Start
	outb	%al, %dx
	## InlineAsm End
	.align	4, 0x90
LBB0_3:                                 ## =>This Inner Loop Header: Depth=1
	movl	$503, %edx              ## imm = 0x1F7
	## InlineAsm Start
	inb	%dx, %al
	## InlineAsm End
	andb	$-64, %al
	movzbl	%al, %eax
	cmpl	$64, %eax
	jne	LBB0_3
## BB#4:
	movl	$496, %edx              ## imm = 0x1F0
	movl	$31744, %edi            ## imm = 0x7C00
	movl	$128, %ecx
	## InlineAsm Start
	cld
	repne
	insl	%dx, %es:(%edi)
	## InlineAsm End
	xorl	%eax, %eax
	popl	%edi
	popl	%ebp
	retl


.subsections_via_symbols
