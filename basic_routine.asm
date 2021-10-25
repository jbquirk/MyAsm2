; This now the start of some real code
; This routine will test the ALU ADC function
; 01 setups the locations 
; 02 display operands
; 03 adds the 16 bit value
			org 0x000
			jmp start
val1		dw  0x47		;dividend
val2		dw	0x46		;divisor
			hlt 			;should not stop
			ORG 0x0100 		; this is a test
start		LD SP,0xffff 	;Setup sp
			MOV SP,CB		;Copy sp to BC			
;The next section does a basic sanity check on computer
;if it halts here you have a very basic issue in program control flow
;Uses minimal instructions just in case problems are deep.
			jmp nb
			hlt				;Halt here if jmp fails
nb      	ld DA,n1
			jmp (DA) 
			hlt				;Halt here if jump DA failed.
n1			ld A,01 		; start valueA
			dec a			;should be zero
			jpz n2			;test
			hlt    			;Jump zero test failed
n2			dec	a			; should be neg and not zero
			jnz	n3			;
			hlt				; Jump not zero failed
n3			jneg n4			;
			hlt				; If here neg jump failed
n4			mov a,c 		; save A
			cmp a,0xff 		; should be equal
			jeq n5
			hlt				;Jump equal failed 
n5			mov	c,a 		;restore a
			cmp a,0x01		;
			jgt n6
			hlt				; Jump greater than failed
n6			dec a
			cmp a,0xff		; Compare A with FF
			jlt n7
			hlt				;Jmp less than failed
n7			add a,0x3 		;should generate carry
			jpc n8
			hlt				;Jump carry failed
n8			ld	X,JMPPASS	; Here basic jump tests passed 
			CALL WRITE_STR
			ld x,crlf
			call write_str
			ld A,00			;should be 0
			inc a			;Alu operation should set flags
			jpz fail		;test
n9			dec a			; should be zero
			jnz	fail		;
n10			jneg fail	
n11			mov	a,c 		; save A
			cmp	a,0xff 		; should be not equal
			jeq fail
n12			mov	c,a 		;restore a
			cmp a,0x00
			jgt fail
			ld A,0x04
n13			cmp a,0x02
			jlt fail
n14			add a,0x3 		;should not generate carry
			jpc fail
			jmp n15
fail		hlt	
n15			LD X,JMPPASS1
			CALL WRITE_STR
			ld x,crlf
			call write_str		
			LD X,WEL 		;Load msg address
			CALL WRITE_STR
			LD X,CRLF
			CALL WRITE_STR
			ld a,0x99
			CALL DUMP_REG
			call print256
			ld hl,0x2123 ; first value
			push hl
			mov hl,da
			call conv16
			ld x,mulmsg
			call write_str
			LD X,CBUF16
			call WRITE_STR
			ld hl,0x0046 ; second value
			push hl		 ; stack setup
			mov hl,da
			call conv16
			ld x,mulmsg2
			call write_str
			LD X,CBUF16
			call WRITE_STR
			call mul16	 ;multiply
;			call dump_reg ; see what is returned LSB in DA
			ld x,mulmsg3
			call write_str
			push d
			push a
			mov hl,da
			call conv16
			LD X,CBUF16
			call WRITE_STR
			pop a
			pop d
			call conv16
			LD X,CBUF16
			call WRITE_STR	
			ld x,crlf
			call write_str
;Division test
			ld y,val1
			ld hl,(y+) ; first value dividend
			push hl
			mov hl,da
			call conv16
			LD X,CBUF16
			call WRITE_STR
			ld hl,(y+) ; second value divisor
			push hl		 ; stack setup
			mov hl,da
			call conv16
			ld x,divmsg
			call write_str
			LD X,CBUF16
			call WRITE_STR
			call dump_reg
			call div16
			call dump_reg
; values return result in DA and the remainder in HL
			call conv16
			ld x,cbuf16
			call write_str
			mov hl,da
			call conv16
			ld x,remain
			call write_str
			ld x,cbuf16
			call write_str
			HLT
			RET
;This routine dumps basic register set to the screen
DUMP_REG 	PUSHALL			; Save All
			ld x,crlf
			call write_str ; tidy up screen 
			LD X,DU
			CALL WRITE_STR
			LD X,CRLF
			CALL WRITE_STR
			MOV SP,Y    		; Grab current stack pointer.
			INC Y				; Point to Y LSB
			ld a,(Y+)      		; Load Y LSB
			MOV A,B
			ld a,(Y+) 
			CALL conv
			ld X,MSGY
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			MOV B,A
			CALL conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			ld a,(Y+)      ; Load X LSB
			MOV A,B
			ld a,(Y+) 
			CALL conv
			LD X,MSGX
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			MOV B,A
			CALL conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			ld a,(Y+)      ; Load L LSB
			MOV A,B
			ld a,(Y+) 
			CALL conv
			LD X,MSGHL
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			MOV B,A
			CALL conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR	
			ld a,(Y+)      ; Load B LSB
			MOV A,B
			ld a, (Y+) 
			CALL conv
			LD X,MSGCB
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			MOV B,A
			CALL conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			ld a,(Y+)      ; Load DA LSB
			MOV A,B
			ld a,(Y+) 
			CALL conv
			LD X,MSGDA
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			MOV B,A
			CALL conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR			
			ld a,(Y+)      ; Load FLAGS 
			push A
			CALL conv
			LD X,MSGF
			CALL WRITE_STR
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			pop a
			CALL DSPFLAGS 		; CONVERT VALUE IN A TO FLAGS VALUES
			LD DA,(Y+)      	; Load PC into DA
			ADD DA,0x0001 		;Adjust to reflect return location
			CALL  conv16
			LD X,MSGPC
			CALL WRITE_STR
			LD X,CBUF16
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
; this should be the top of stack before call to the dump routine.
			dec Y				;Adjust for correct TOP of stack
			mov  y,da			; copy y into da
			CALL conv16
			LD X,MSGSP
			CALL WRITE_STR
			LD X,CBUF16
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			ld	x,crlf
			call write_str
;			LD DA,0x9999		;test value
;			CALL conv16
;			LD X,MSGtst
;			CALL WRITE_STR
;			LD X,CBUF16
;			CALL WRITE_STR	
;			ld	x,crlf
;			call WRITE_STR
			POPALL
			RET		
;
; Print from ff to zero
print256 	pushall
			ld a,0xff
			ld d, 16   ; 16 hex characters to a line
pnrt1		call  conv
			LD X,cbuf
			CALL WRITE_STR
			LD X,SPACE
			CALL WRITE_STR
			dec d 
			jnz cont
			ld x,CRLF
			call write_str
			ld d,16		;reload count value
cont		dec a
			jnz pnrt1
			call  conv
			LD X,cbuf
			CALL WRITE_STR
			ld x,CRLF
			call write_str			
			popall
			ret
;Convert value in A to hex and print
;Will rewirte when more instructions implemented.
cbuf		 DW 0x0 ; temp store
			db 0X0 ;
conv		PUSH X   ; save X
			PUSH B   ; save B
			PUSH A   ; save A
			LD X,cbuf 
			Call convrtasc ;does the work
			POP A
			POP B
			POP X
			RET
;Convert value in DA to hex and print
;Will rewirte when more instructions implemented.
cbuf16	 	DW 0x0 		; temp store
cbuf16l	 	DW 0x0 		; LSB
			db 0X0 		;
conv16	 	PUSH X   		; save X
			PUSH B   		; save B
			PUSH D   		; Save DA no push DA yet
			PUSH A   		; save A
			LD X,cbuf16l
			CALL CONVRTASC ;
			MOV D,A 		; Load A with MSB
			LD X,CBUF16
			CALL CONVRTASC
			POP A
			POP D
			POP B
			POP X
			RET
;This routine takes the value in A and coverts to a hex ascii pointed to by X
;X = pointer to where character sting to stored
;A = binary value to convert
;Modifies A, X, B 
CONVRTASC	PUSH A
			LD B,04 ;4 bit positions
			SLR A
			CMP A,0x9 ;Is 0-9
			JGT HEXA16
			ADD A,'0'
			JMP STR16MSB
HEXA16   	ADD A,0x37
STR16MSB 	st a,(X)
			INC X
			POP A
			AND A,0xF ; Mask low bits
			CMP A,0x9 ;Is 0-9
			JGT HEXB16
			ADD A,'0'
			JMP STR16LSB
HEXB16   	ADD A,0x37
STR16LSB 	st a,(X)
			RET
;This routine takes the 8 bit value passed in A and converts to the flags
;  128  64  32  16  8   4   2   1
; +---+---+---+---+---+---+---+---+		
; | S | M | - | < | > | = | C | Z |
; +---+---+---+---+---+---+---+---+
; all registers are saved upon entry and restored on exit
;
;
;
DSPFLAGS 	PUSHAll ;SAVE REG		
			ld Y,led1
			LD H,0X7 ; COUNT
			mov a,D
			LD X,FLAGASC
			LD C,0x1	;SHIFT AMOUNT - new shifter is 0 = no shift.
LOOP1		SLR D
			JPC FLAGFOUND
			inc X
			JMP PRINTSPC
FLAGFOUND 	ld a,(X+)
			CALL WRITEA
			JMP CHECKEND
PRINTSPC 	LD A,0X20
			CALL WRITEA
CHECKEND 	mov h,a
			DEC A
			mov a,h
			JNZ LOOP1
			POPAll
			RET
FLAGASC 	ds "ZC=><-MS"
;
;
; Write string requires X to point to string. String is standard null terminated
WRITE_STR	PUSH A
			PUSH Y
			LD Y,LED1 
WRITE2		ld a,(X+)
			CALL WRITEA
			ADD A,0 	;Test for zero CMP not yet implemented
			JNZ WRITE2
			POP	Y
			POP	A
			RET
; Mul16 does a simple add x not optimised just a test for now.
mul16s1		DW 0		; lsb of result
mul16s2		DW 0		; may need storage * 10
mul16s3		DW 0		; MSB of result *10
mul16s4		DW 0		; storage sum of products * 100
mul16		mov sp,DA	; get current sp			we have old stack frame
			push b
			push c
			push x
			push y
;			call dump_reg
			ld y,mul16s1
			add DA,0x03 	; Adjust bottom var list
			mov da,x		; Put into address ret
			ld cb,(x+)		; first param
			ld da,(x+)		; second param
;			call dump_reg   ;Are we set up correctly
			push d
			mov a,d 
;			call dump_reg
			call mul        ; do 8 bit mul B * A
			pop d
			st HL,(Y)		;save partil products
;			call dump_reg	;Check values correct
			call mul		; B * D
;			call dump_reg
			push d
			push a
			ld da,(Y)		; get old DA 
			add d,l			; add hi bit
			st da,(y+)		; save result
			clr a
			adc a,h			; capture carry bit
			mov a,l			; shift correct spot in  32 bit word
			clr a 
			mov a,h         ; HL correct to MSB of 32 bit word		
			st HL,(y)		;
			dec y
			dec y			; point to lsb of 32 bit word
;			call dump_reg			
			pop a 
			mov c,b
			mov a,d
;			call dump_reg
			call mul		; C * A 
			push a
			ld da,(Y)		; get old DA 
			add d,l			; add hi bit
			st da,(y+)		; save result
			push y
			pop x			;mov y,x
			clr a
			adc a,h			; capture carry bit
			mov a,l			; shift correct spot in  32 bit word
			clr a 
			mov a,h         ; HL correct to MSB of 32 bit word
			mov hl,da		; 
			add	da,(x+)				
			st da,(y)		;
;			call dump_reg	; 
			pop a           ; clean up stack
			pop d 			; C * D
;			call dump_reg
			call mul
			mov hl,cb
			ld da,(y) 
			add da,cb       ; MSB 32 bit			
			st da,(y)
;			call dump_reg
mul16e		ld y,mul16s1
;			call dump_reg
			mov da,hl ; hl msb
;			call dump_reg
			ld da,(y) ; DA lsb
;			call dump_reg
			pop y
			pop x
			pop c 
			pop b
			ret
			;Simple 8 bit multiply a is temp reg HL is result of B * D	
		
mul			push a
			push c
			push b
			push d 
			clr a	; Clear a register
			mov a,c	;
			mov a,h 
			mov a,l ;clear result
			or d,c	;set flags check if zero if zero we are done
			jpz mul_e
mul1		add a,b ; 
			jpc mul3
			jmp mul2
mul3		push d 
			mov c,d
			inc d
			mov d,c
			pop d
mul2 		dec d
			jnz mul1
mul_e		mov a,l 
			mov c,b ; limitation
			mov b,h ; store result in hl
			pop d
			pop b			
			pop c
			pop a 
			ret
;
;      Div 16 passed on stack p2 is top p1 is next
;      CB holds the divisor
;      DA holds dividend (number to be divided)
;      return DA result and HL remainder
;
div16s1		dw 0x123		; result
div16s2		dw 0x456    	; internal temp storage remainder
div16		mov sp,DA	; get current sp			we have old stack frame
			push b
			push c
			push x
			push y
			ld y,div16s1
			add DA,0x03 	; Adjust bottom var list
			mov da,x		; Put into address ret
			ld cb,(x+)		; p2 param and divisor 
			ld da,(x+)		; p1  param dividend
;			call dump_reg   ;Are we set up correctly
			push d
			push a          ; save DA
			mov cb,da		; get ready to compare
			cmp d,0			; zero
;			call dump_reg
			jneq div16_1
			cmp a,0	
;			call dump_reg			
			jneq div16_1     ; divisor not zero this is ok
div_error	ld hl,0xffff
			st hl,(y+)
			st hl,(y+)
			pop a
			pop d
			jmp div16e     ; return all ones this value should be imposible for a divide
div16_1		push c			; stack has DA so push CB dividend and divisor
			push b			; 
			call compare    ; compare DA to CB
			pop b			; clean stack
			pop c 
			pop a
			pop d 
;			call dump_reg
			jeq	div16_12	; dividend and divisor are equal so its 1 and zero
;			call dump_reg
			jgt div16_2		; divisor is smaller OK lets do divide 
			ld hl,0
			jmp div16_13		; divisor bigger so we are done
div16_12	ld hl,1			; it is equal
			ld cb,0			; done	
			jmp div16_13
			ld hl,0			;
div16_13	st hl,(y+)		; zero is the result
			st cb,(y+)		; remainder
			jmp div16e
div16_2		nop				; place holder need to error conditions first
							; We arrive here we have checked for the basic cases and errors
							; so we actual divide DA holds dividend and CB the divisor
			ld hl,0			;setup stack variables
			push hl
			push hl
			mov sp,hl;
			push d
			push a
			mov hl,da	;
			mov da,x		; set variable X
			mov da,y
			inc y
			inc y           ;set up up Y
			pop a
			pop d
			st da,(Y)		;	
div16sh		call sll16
			st da,(Y)		; shifted dividend 
			ld da,(X) 		; test
			call dump_reg   ;
			jpc div16merge  ; we had carry out so we have one in divisor
			or a,1			; low order safe
div16merge 	call comp16 	; is da bigger than cb 
			jlt div16sh_1	;
			sub da,cb		; time to sub 
			
div16sh_1			
div16e		ld x,div16s1
			ld da,(x+)
			ld hl,(x+)
			call dump_reg
			pop y
			pop x
			pop c
			pop b
			call dump_reg
			ret						
; Write asumes Y reg is pointing to the terminal address 
;		
WRITEA  	PUSH A
			out a 
			POP A
			RET
;
; comapre P1 and P2 returns flags gt,lt or eq
; 
compare		push d 
			push a
			mov sp,DA	; get current sp			we have old stack frame
			push b
			push c
			push x		
			add DA,0x05 	; Adjust bottom var list
			mov da,x		; Put into address ret
			ld cb,(x+)		; p2  
			ld da,(x+)		; p1  param 
			call comp16
compare_e	pop x
			pop c
			pop b 
			pop a
			pop d 
			ret 
;
;		simple 16 bit compare da to cb values in registers
;
comp16	  	cmp d,c 		; check hi byte if neq we are done
			jneq comp16_e
			cmp a,b			; low byte determines <> = 
comp16_e	call dump_reg
			ret
;
;           simple 16 bit shift of DA result returned in DA
;
sll16		push b ;save CB
			push c 
			ld b,0x1
			ld c,01
			sll a     ;shift a one bit left
			call dump_reg
			jpc sll2 ;carry so need to adjust for carry
			sll d     ; shift d
			jmp sll16e 
sll2     	sll d 
			or d,0x1  ; merge carry bit
sll16e 		call dump_reg	
			pop c
			pop b
			ret
;shift 16 bit p1 left by one return in HL carry flag set if shift out
shiftleft16 push D
			push A
			mov sp,da
			push b
			push c
			push x
			add da,0x5 ;we have only one parameter
			mov da, x
			ld da,(x)
			call dump_reg
			ld b,0x1
			ld c,01
			sll a     ;shift a one bit left
			call dump_reg
			jpc shiftl2 ;carry so need to adjust for carry
			sll d     ; shift d
			jmp shiftle 
shiftl2     sll d 
			or d,0x1  ; merge carry bit
shiftle 	mov da,hl
			call dump_reg
			pop x
			pop c
			pop b 
			pop a 
			pop d
			ret 
			
LED1    	EQU 0xfe00 ;led location
WEL			DS "Version 1.0 test routines"	
DU			DS "Dump registers"
jmppass 	DS "Jump tests passed in test and jump also includes basic instructions"
jmppass1 	ds "Jump tests passed negitive tests"
MSGDA		DS "DA="
MSGCB		DS "CB="
MSGHL		DS "HL="
msgtst		DS "test99="
mulmsg		DS "Multipling "
mulmsg2     DS " X "
mulmsg3     DS " = "
divmsg		DS " / " 
remain		DS " Remainder "
MSGF		DB 'F'
			DB '='
			DB 0x0
MSGX		DB 'X'
			DB '='
			DB 0x0
MSGY		DB 'Y'
			DB '='
			DB 0x0
MSGPC		DB 'P'
			DB 'C'
			DB '='
			DB 0x0
MSGSP		DB 'S'
			DB 'P'
			DB '='
			DB 0x0
CRLF		DB '\n'
			DB '\0'
SPACE		DB ' '
			DB 0x0
