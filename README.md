# MyAsm2
An assembler that targets MyCpu design
See the mircocode compiler for more detals full documentation comming soon.
Some quick basic the parser in this assemble doesn't know anything about the mnemonics its parsing
it has a basic for as follows

```

valid line is blank or comment
; | statement
statement:
 	[label] inst [op|label][,op|label] [;] eol
op:
	[(]reg [+-][)]| symbol
label:
	const|symbol [[+-] const|symbol]
	symbol
eol:
	<cr> or NULL
inst:
	symbol
reg:
	symbol
(:
	pointer) or pointer +) or pointer-)
pointer:
	reg
symbol:
	[_A-Z][_A-Z0-9]*
const:
	[0-9]* | [0x][0-9abcdef]
```

As you can see from my rather bad grammar description all keywords syntactically resolve to label . During parsing label can be convert to the various types. A line can be valid syntactically but not resolve to valid assembler code.   

You might think this doesn't make much sense why have a assembler that does see that a syntax is invalid.

The path I have taken once I have built the syntax of instruction I search the opcode list to confirm if it is valid. If not found it will generate an error on that line of code.

taking this path ensures the assembler is completely table driven. for example

```assembly
label1	ld a,(x-)   ; actual syntax is valid but no  valid opcode    
```

This instruction currently is not implemented but from the view of the syntax is says   

At label1 load A via the point register X and decrement X.  This line is parsed correctly and the builds a structure from the parse tree. 
But when it goes to find a valid op code to match its template there is no valid match so will fail at this point.

```c
struct op_code {
	 	int inst; 
	 	int size;
	 	int opcode;
		int src;
	 	int src_t;
	 	int dest;
	 	int dest_t;
	 	int operands; 
		 };
```

The parser uses tables supplied by the micro-code building process to help it fill in the structure.

For example  where it is expecting an instruction the lexer will return a symbol. The parser will next use the correct look up table for either instruction or register. There is a special register value to indicate a constant value. This value is resolved at assembly time not runtime. 

It is possible to pass the instruction register assignment phase and then fail to find a valid opcode in the op code table.

