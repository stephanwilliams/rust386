    .macosx_version_min 10, 11
    .globl	start
    .align	4, 0x90
start:
    .code16
	.align	4, 0x90
waitdisk1:
	movl	$0x1F7, %edx
	inb	%dx, %al
	andb	$0xC0, %al
	movzbl	%al, %eax
	cmpl	$0x40, %eax
	jne	waitdisk1

	movb	$1, %al
	movl	$0x1F2, %edx
	outb	%al, %dx

	xorl	%eax, %eax
	movl	$0x1F3, %edx
	outb	%al, %dx

	xorl	%eax, %eax
	movl	$0x1F4, %edx
	outb	%al, %dx

	xorl	%eax, %eax
	movl	$0x1F5, %edx
	outb	%al, %dx

	movb	$0xE0, %al
	movl	$0x1F6, %edx
	outb	%al, %dx

	movb	$0x20, %al
	movl	$0x1F7, %edx
	outb	%al, %dx

	.align	4, 0x90
waitdisk2:
	movl	$0x1F7, %edx
	inb	%dx, %al
	andb	$0xC0, %al
	movzbl	%al, %eax
	cmpl	$0x40, %eax
	jne	waitdisk2

	movl	$0x1F0, %edx
	movl	$0x7C00, %edi
	movl	$128, %ecx
	cld
	repne insl	%dx, %es:(%edi)

	ljmp $0x0000, $0x7C00
	.align 4, 0x90
