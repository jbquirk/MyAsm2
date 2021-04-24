;
; Version 1.0 a real program to test jumps
;
;
		ORG 0x0 	; this is a test
LED1    EQU 0xfe00 	;led location
stack	equ	0xffff 	;top of stack
		jmp start 	;
f1		hlt			;If here failed 
		org	0x100	;
start	LD y,LED1  	;led location
		LD A,0x01 	; start valueA
		LD SP,stack ;Setup sp
		jmp n1
f2		hlt			;Halt here if jmp fails
n1	    dec a		;should be zero
	    jpz n2		;test
f3		hlt    		;Jump zero test failed
n2		dec a		; should be neg and not zero
		jnz	n3		;
f4		hlt			; Jump not zero failed
n3		jneg n4		;
f5		hlt			; If here neg jump failed
n4		mov a,c 	; save A
		cmp	a,0xff	; should be equal
		jeq n5
f6		hlt			;Jump equal failed 
n5		mov  c,a 	;restore a
		cmp a,0x01
		jgt n6
f7		hlt			; Jump greater than failed
n6		cmp a,0xff
		jlt n7
f8		hlt			;Jmp less than failed
n7		add a,0x3 	;should generate carry
		jpc pass
f9		hlt			;Jump carry failed
pass	hlt			;pass