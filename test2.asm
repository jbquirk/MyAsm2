;
;
;		A set of test comments at the
;       Begining of the code
;
;
		org 0x100   ;
l1 		ld sp,0xfffe;
		ld a,8 		;
		ld hl,fred1+0		; undefined
l3		adc a,24	; test comments	
		push a		;
		and a,8		;
fred2	equ str1+2	; this will break
		and a,0xc	;
		ld x,str1	;
		db '\n'		;
		dw 24		;
		ld a,(y+)	; this fails		
		sbc da,l2	; error no value
str1		ds "this is a test" ;
		sbc da,cb
		ld y,str1	;Post defintion test
fred1	equ str1	; A good test	
		db 0x0a	; does this end up at 129
		end