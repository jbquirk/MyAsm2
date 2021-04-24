; This now the start of some real code
; This routine will test the ALU ADC function
; 01 setups the locations 
; 02 display operands
; 03 adds the 16 bit value
			org 0x000
			jmp start
			hlt 			;should not stop
			ORG 0x0100 		; this is a test
start		LD Y,LED1  		;led location
			LD SP,0xffff 	;Setup sp
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
			HLT
			RET
;This routine dumps basic register set to the screen
DUMP_REG 	PUSHALL			; Save All
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
			LD DA,0x9999		;test value
			CALL conv16
			LD X,MSGtst
			CALL WRITE_STR
			LD X,CBUF16
			CALL WRITE_STR		
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
			LD B,03
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
			LD C,0x0	;SHIFT AMOUNT due to hardware this is one shift
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
; Write asumes Y reg is pointing to the terminal address 
;		
WRITEA  	PUSH A
			st a,(Y) 
			POP A
			RET
LED1    	EQU 0xfe00 ;led location
WEL			DS "Version 1.0 test routines"	
DU			DS "Dump registers"
jmppass 	DS "Jump tests passed in test and jump also includes basic instructions"
jmppass1 	ds "Jump tests passed negitive tests"
MSGDA		DS "DA="
MSGCB		DS "CB="
MSGHL		DS "HL="
msgtst		DS "test99="
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
