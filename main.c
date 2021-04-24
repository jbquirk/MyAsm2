/****************************************************************************
 *                                                                          *
 * File    : main.c                                                         *
 *                                                                          *
 * Purpose : Console mode (command line) program.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00/00/00  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "../create_opcode_list/mycpu.h"
/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00/00/00  Created                                              *
 *                                                                          *
 ****************************************************************************/
/*
Major redesign I going to to a two pass model as a way of resolving my issues with forward refernces 
it prgressively got harder to fix the issues in doing forward refernces as a result I decided to move to a two pass model.
I find if I am start to create all sorts of code to solve issues I have missed something in the design.
A side effect of this change is math will now work as intended as by the time the variable is used it will
have resloved to a value. 
*/


enum tokens { num, leftb, rightb, comma, eol, plus, minus,semi, symbol,pseudo,s_const,c_const};
char *stokens[] ={ "number", "left", "right", "comma", "eol", "plus", "minus","semi","symbol","pseduo","string","c_const" };
char *stype[]= { "reg","reg16","number","pointer","preg","incp","decp","none","bad","instruction" };
struct tok {
			int tok; //token found
			int type; // type of token
			char *s; //string version of token
			char *linepos; // Current line position
			int value;
			};
struct oprnd {
			char *line; // current pos in line as returned lexer
			struct registers p_r; //filled in if valid reg comination found
			int value; 			// this will only be valid if regVAL
			} ;
// this section defines the pseudo ops  
enum _ops { dw,db,ds,org,equ,ends };
#define OP_CNT 6
struct dir {
			char *assem_op;
			int action;
			int size;
			} directives[] = { "DW",dw,2, //define word
							   "DB",db,2, //define byte
							   "DS",ds,2, //define string
							   "ORG",org,3, //define start of code
							   "EQU",equ,3, //set value to lable
							   "END",ends,3, // this will end current processing
							 };	
int error_cnt =0;
int line_cnt =0;
extern int control_rom[];
extern int address;

struct op_code op_build; // this is the proto instruction we build to lookup

struct tok tokeniz(char *s); // tested OK
#define STRING_SZ 128
char s_buf[STRING_SZ]; 
int pass=0; // 0 = init phase, 1 = define variable, 2 = generate code

void assemble(FILE *fp);
int convert(char * d, int radix);
char *statementorlabel(struct tok t);
struct oprnd operand(struct tok t); 
extern void install_symbol(char *s); //this is only called if all info on symbol is valid
struct oprnd get_constant(struct tok t);
bool statement(struct tok t);
extern int get_symbol_v(char *s);
struct oprnd get_reg(char *s);
char* toUpper(char* s);
void emit(void);
extern void dump_symtable(void);
void dumpHex(void);
extern void clear_symboltbl(void);
extern int get_last_symbol_address(void);
extern int get_last_symbol_index(void);
extern int update_value(int v,int index);
extern void fake_install(char *s);

int main(int argc, char *argv[])
{
	FILE *fp;
// initilize some values
	address = 0;
	clear_symboltbl(); // this routine sets up and clears symbol table
	pass = 1;
 if(argc > 1 )
	{ 
	printf("Argv = %s \n",argv[1]); 
   /* opening file for reading */
   fp = fopen(argv[1] , "r");
   if(fp == NULL) {
      perror("Error opening file");
      return(-1);
	  }
    }
 else
	{
	  printf("Expected file name \n");
	  return(-1);
	}
	assemble(fp);
 	rewind(fp);
	fprintf(stderr,"Pass one complete\n");
	if(error_cnt > 0)
		fprintf(stderr,"%d error(s) found",error_cnt);
	error_cnt =0;
	line_cnt = 0;
	address = 0;
	pass = 2;
	assemble(fp);
    return 0;
}
// this pulled out of main so it can clled twice once as pass one 
// and then as pass two
void assemble(FILE *fp)
{
	char *s;
	struct tok t;
	struct oprnd v;
	char buf[128];
    size_t bufsize = 128;

  while(fgets(buf,bufsize,fp)!=NULL)
   {
		line_cnt++;
		s = toUpper(buf);
		op_build.dest_t = none;
		op_build.src_t = none;
		op_build.inst = -1; 
		t = tokeniz(s);
//		printf("tok = %d, %s",t.tok,stokens[t.tok]);

//		if(t.tok == num)
//			printf(" value = %d\n",t.value);
//		else
//			printf("\n");
// valid line is blank or comment
// valid statemnt line is as follows
// [label] inst [op][,op] [;] eol
// vaild op is
// [(]reg [+][)]| symbol
// vaild symbol is
// const|label [+-] [const|label]
	if(t.tok != semi && t.tok != eol)
		{
		if(t.tok == symbol) // should be a symbol
		 {
		 s = statementorlabel(t);
		 t = tokeniz(s);
		 if(t.tok != semi && t.tok != eol)
			{
			v = operand(t);
			if(v.p_r.code == regVAL)
				{
				op_build.src_t = v.value;
				op_build.src = regVAL;
//				printf("Found value and it is %d and %s \n",v.value,stype[v.p_r.type]);
				}
			else
				{
				op_build.src_t = v.p_r.type;
				op_build.src = v.p_r.code;
//				printf("Found type = %s and %d %s\n",v.p_r.name,v.p_r.code,stype[v.p_r.type]);
				}
			t = tokeniz(v.line);
			if(t.tok == comma) //all good
				t = tokeniz(t.linepos);
			else
				{
				if(t.tok != semi && t.tok != eol)
					{
					printf("Expected \',\' found %s\n",stokens[t.tok]);
					error_cnt++;
					// error recovery will go here.
			 		}
				}
			if(t.tok != semi && t.tok != eol)
				{
				v = operand(t);
				if(v.p_r.code == regVAL)
					{
					op_build.dest_t = v.value;
					op_build.dest = regVAL;
//					printf("Found value and it is %d %s\n",v.value,stype[v.p_r.type]);
					}
				else
					{
					op_build.dest_t = v.p_r.type;
					op_build.dest = v.p_r.code;
//					printf("Found type = %s and %d %s\n",v.p_r.name,v.p_r.code,stype[v.p_r.type]);
					}
				 t.linepos = v.line; // place in line in sync
				}
			}
		  }
		else
			{
			printf("expected symbol or instruction found %s\n",stokens[t.tok]);
			}
		// check for valid eol
//		printf(" tok is %d %s",t.tok,stokens[t.tok]);
		if(t.tok != eol)
			{ 
			t = tokeniz(t.linepos);
			if(t.tok != eol)
				printf("found extra input ignored %d %s\n",t.tok ,t.linepos);
			}	
		} // if here whole line comment or blank
	if(pass ==2)
		printf("%05d:",line_cnt); 
	if(op_build.inst != -1) // we found some opcodes
		emit();	
		if(pass ==2)
			printf("%s",buf);
   }
	printf(" %d error(s) found\n", error_cnt);
	if(pass == 2)
		{
		dump_symtable();		
		dumpHex();
		} 

}
// this will only emit if there are no errors 
// 
void emit(void)
{
	int i;
	int temp = 0;
	bool found = false, ps_op = false;
//	printf("Errors found %d\n",error_cnt);
//	printf("Build %d,%s,%s\n",op_build.inst,r[op_build.src].name,r[op_build.dest].name);
//	if(op_build.dest == regVAL)
//		printf("size = %d,type %s, %d \n",op_build.size,stype[op_build.src_t],op_build.dest_t);
//	else if(op_build.src == regVAL)
//	  printf("size = %d,type %d \n",op_build.size,op_build.src_t);
//	else
//	  printf("size = %d,type %s, %s \n",op_build.size,stype[op_build.src_t],stype[op_build.dest_t]);
   if( op_build.inst >= NUMBEROPS ) // we have pseudo op
			ps_op = true;
   else	  
	for ( i=0; i < NUMBERINST; i++){
			if(op_build.inst == op_codes[i].inst)
				{ // first step
				if(op_codes[i].src_t == none) // we have single inst
					{
					// check we have no operands passed if generate warning and ignore
					if(op_build.src_t != none)
						printf("*** warning found instruction with unexpected operands ingnored\n");
					op_build.opcode = op_codes[i].opcode;
					op_build.size = op_codes[i].size+8;
					found = true;
					break;  
					}
				if(op_build.src == op_codes[i].src)
					{ // we have first part of instruction
					  if (op_build.src_t == op_codes[i].src_t)
					   {
						if(op_codes[i].dest_t == none) // we have single inst
							{
							// check we have extra operands passed if generate warning and ignore
							if(op_build.dest_t != none)
								printf("*** warning found instruction with unexpected operand\n");
							op_build.opcode = op_codes[i].opcode;
							op_build.size = op_codes[i].size+8;
							found = true;
							break;
							}  
						if(op_build.dest == op_codes[i].dest )
							{ // we have final part of instruction
								if(op_build.dest_t == op_codes[i].dest_t)
									{
									op_build.opcode = op_codes[i].opcode;
									op_build.size = op_codes[i].size+8;
									found = true;
									break;
									}
								else if( op_build.dest == regVAL && op_codes[i].dest == regVAL)
									{									
									op_build.opcode = op_codes[i].opcode;
									op_build.size = op_codes[i].size+8;
									found = true;
									break;
									} 

							} 
					   } else if(op_build.src == regVAL && op_codes[i].src == regVAL)
							{ // this single instruct like jmp XXXX									
							op_build.opcode = op_codes[i].opcode;
							op_build.size = op_codes[i].size+8;
							found = true;
							break;
							}   								
					}  
 				}
		}
	//With luck we have the opcode and the size
	if(found)
		{
		switch(op_build.size/8)
			{
			case 1:
				if(pass == 2)
					printf(" %04x %02x\t\t",address,op_build.opcode);
				control_rom[address++] = op_build.opcode;
				break;
			case 2:
				if(op_build.dest == regVAL) // 8bit value in dest_t
					{
					if(pass == 2)
						printf(" %04x %02x %02x\t",address,op_build.opcode,0xff & op_build.dest_t);
					control_rom[address++] = op_build.opcode;
					control_rom[address++] = (0xff & op_build.dest_t);
					}
				else
					{
					if(pass == 2)
						printf(" %04x %02x %02x\t",address,op_build.opcode,0xff & op_build.src_t);
					control_rom[address++] = op_build.opcode;
					control_rom[address++] = (0xff & op_build.src_t);
					}

				break;
			case 3:
				if(op_build.src == regVAL) // 16bit value in src_t
					{
					if(pass == 2)
						printf(" %04x %02x %02x %02x\t",address,op_build.opcode,0xff & op_build.src_t,(0xff00 & op_build.src_t)>>8);
					control_rom[address++] = op_build.opcode;
					control_rom[address++] = (0xff & op_build.src_t);
					control_rom[address++] = (0xff00 & op_build.src_t)>>8;
					}
				else
					{
					if(pass == 2)
						printf(" %04x %02x %02x %02x\t",address,op_build.opcode,0xff & op_build.dest_t,(0xff00 & op_build.dest_t)>>8);
					control_rom[address++] = op_build.opcode;
					control_rom[address++] = (0xff & op_build.dest_t);
					control_rom[address++] = (0xff00 & op_build.dest_t)>>8;
					}
			break;
			}
		}
	else
		{
		if(ps_op) 
			{
			switch(op_build.inst - NUMBEROPS) // remove offset and look for pseudo ops
				{
				case org: // set new orgin
					if(pass == 2)
						printf(" %04x   \t\t",address); 
					address = op_build.src_t; //mask just in case value invalid
					break;
				case db: // define byte
					if(pass == 2)
						printf(" %04x %02x\t\t",address,0xff &op_build.src_t); 
					control_rom[address++] = (0xff & op_build.src_t);
					break;
				case dw: // define word
					if(pass == 2)
						printf(" %04x %02x %02x\t",address,0xff & op_build.src_t,(0xff00 & op_build.src_t)>>8); 
					control_rom[address++] = (0xff & op_build.src_t);
					control_rom[address++] = (0xff00 & op_build.src_t)>>8;
					break;
				case ds: // define string tokenizer has stored string in s_buf
					temp = 0;
					while(s_buf[temp] != '\0')
						{
						if(temp == 0 && pass ==2 )
							{
							printf(" %04x %02x ",address,(0xff & s_buf[temp]));
//							printf("Start of string\n");
							} 
						else
							if( temp < 3 && pass ==2 ) // print first eight
								printf("%02x ",(0xff & s_buf[temp]));
						if(temp == 9 && pass ==2)
							printf(".. "); 
						control_rom[address++]=(0xff & s_buf[temp]);
						temp++;
						}				 
					control_rom[address++]= '\0';
					break;
				case equ: // assign value to label
					if( get_last_symbol_address() == address) // valid equ
 						{
						update_value(op_build.src_t,get_last_symbol_index());
						}
					else if(pass ==2)
						update_value(op_build.src_t,get_last_symbol_index());
					else
						{
						error_cnt++;
						fprintf(stderr,"error bad equ possible phase error\n");
						}
					if(pass == 2)
						printf(" %04x \t\t\t",address); 					 
					break;
				case ends:
						break; // do nothing
				default: // we should never get here but you never know.
					fprintf(stderr,"***warning unkown pseudo op %d\n",op_build.inst - NUMBEROPS); 
				}

			}
		else
			fprintf(stderr,"Warning invalid op code\n");
		} 
}
//
//This routine returns the next token from input
//
//
struct tok tokeniz(char *s)
{
	struct tok t;
	int i=0, n=0;
	char temp[30],*temp2,*p ,*e;
	p=s;
	while(isspace(*p)) //discard any white space.
		p++;
	e=p;
	if(isdigit(*p)) // we have a digit so lets process. 
		{
			if(p[0] == '0' && (p[1] == 'x' ||p[1] == 'X' )) // we have hex digit
				{
				i=0;
				e = &p[2]; //skip 0x	
				while(isxdigit(*e)) // this captures all possibile versions
					{
					e++;
					i++; //keep track how many found.
					}
				for(n =0; n < i; n++)
					temp[n] = p[n+2];
				temp[n] ='\0';
				n = convert(temp,16); //this could be a hex character
				}
			else
				{
				i=0;	
				while(isdigit(*e)) // should only be decimal
					{
					e++;
					i++; //keep track how many found.
					}
				for(n =0; n < i; n++)
					temp[n] = p[n];
				temp[n] ='\0';
				n = convert(temp,10); //this could be a hex character
				}
			t.tok = num;
			t.value = n;
			t.linepos = e;
			temp2 = (char *)malloc(sizeof(char)*strlen(p));
			strcpy(temp2,temp);
			t.s = temp2;  	
		}
	else if(isalpha(*p)|| *p == '_' ) // we have alpha lets process
		{
			e++; i++;
			while(isalnum(*e)|| *e == '_') // this captures all possibile versions
				{		
				e++;
				i++; //keep track how many found.
				}
			for(n =0; n < i; n++)
				temp[n] = p[n];
			temp[n] ='\0';
			t.tok = symbol;
			t.linepos = e;
			temp2 = (char *)malloc(sizeof(char)*(strlen(temp)+1));
			strcpy(temp2,temp);
			t.s = temp2;  	
		}
	else 
		{ // we have found all possible multi chacter items now lets look for 
		  // ( ) , + - \n 
			switch(*p)
				{
				case '(':
					p++;
					t.tok = leftb;			
					t.linepos = p;
					break;	
				case ')':
					p++;
					t.tok = rightb;			
					t.linepos = p;
					break;	
				case ',':
					p++;
					t.tok = comma;			
					t.linepos = p;
					break;	
				case '+':
					p++;
					t.tok = plus;			
					t.linepos = p;
					break;	
				case '-':
					p++;
					t.tok = minus;			
					t.linepos = p;
					break;	
				case '\n':
				case '\0': 
					p++;
					t.tok = eol;			
					t.linepos = p;
					break;	
				case ';': // comment eat rest of line to eol
					p++;
					while(*p != '\n' && *p !='\0')
						p++;
					t.tok = eol; // we return this because we've eaten the comment			
					t.linepos = p;
					break;
				case '\'': //char const will process and return as a number;
						p++; 
						if(*p == '\\' ) // check for escape
							{
							++p;
							switch(*p)
								{
								case 't':
									n = '\t';
									break;
								case 'n':
									n = '\n';
									break;
								case 'r':
									n = '\r';
									break;
								case '0':
									n = '\0';
									break;
								case '\\':
									n = '\\';
								break;
								default: // pass char along
									n = *p;
								}
							p++;
							}
						else
							n = (int)*p++;
						if(*p != '\'') // bad char 
							printf("Bad character const\n");
						p++ ; 
						t.tok = num;
						t.value = n;
						t.linepos = p;
						break;
				case '\"': // string found 
					p++;
					int temp1=0;
						while(*p != '\n' && *p !='\0' && *p != '\"' )
							{
							s_buf[temp1++] = *p++;
							if( temp1 == STRING_SZ ) // string to big
								{
								printf("String is to large\n");
								error_cnt++;
								break;
								}
							}
						if(*p !='\"' ) // unterminated string
							{
							printf("unterminated string\n");
							error_cnt++;
							// most liket EOL so error recovery should occur 
							break;
							}
						// Looks like valid string 
						p++; 
						s_buf[temp1] = '\0'; // terminate string
						t.tok = s_const;			
						t.linepos = p;
						break;
				default: // if here we have bad imput;
					error_cnt++;
					printf("Unexpected symbol ignoring %c \n",*p); // if here input is bad attemp recovery
					p++;
					t=tokeniz(p);							// by geting net token	
			}
		}
	return(t);
}
int convert(char * d, int radix)
{
	long int n;
	char *p; //just to keep function happy
	p = d; 
		n = strtol(d, &p,radix);
	return((int)n);
} 
// first part of parser checks for statement or label
// A line can start with ether a label or a statement 
// The order is label -> statement or statement;
// before entry we check if it is a symbol 
char *statementorlabel(struct tok t)
{
		char *line;
		struct tok t1;
		op_build.inst = -1;
		line = t.linepos;
		if(!statement(t))
			{
			if(pass == 1)
				install_symbol(t.s); // install symbol
			else
				fake_install(t.s); // just update some flages
			t1 = tokeniz(t.linepos);
			if(t1.tok != eol && t1.tok != semi) // have we hit EOL
				{
				line = t1.linepos;
				if ( t1.tok != symbol) // we have an error
					{
					error_cnt++;
					printf("unexpected %s \n",stokens[t1.tok]);
					}
				else
					{
					statement(t1);
					if(op_build.inst == -1)
						{
						error_cnt++	; 
						printf("No valid instruction found \n");
						}
					}
				}
			}
return(line);			
}

bool statement(struct tok t)
{
		int i=0;
		op_build.inst = -1;
		bool code = false; // set to true if instrction found
		if(t.tok == symbol ) // off to a good start
			{
				for(  i=0; i < NUMBEROPS+1; i++)
					if(strcmp(t.s,a_inst[i]) ==0 )
						{
						code = true;
						break;
						}			
// check for pseudo ops this are processed in emit so for now pretend are instructions
			if(!code ) // Look for the pseudo ops
				for(i=0; i < OP_CNT; i++)
					if(strcmp(t.s,directives[i].assem_op) ==0 )
						{
						code = true;
//						printf("Found pseduo op - %s",directives[i].assem_op);
						i = directives[i].action + NUMBEROPS;
//						printf("Instruction set to %d\n",i);
						break;
						}
			}	 

		if (code)
			{
			op_build.inst = i;
//			printf("%s instruction found\n",a_inst[i]);
			}
	return(code);
}

//
//This routine expect to find an operand and bulids defintion based on what is found
//very badly defined grammar below
// oprn:CONST|MREG
// MREG:reg|point_t
// point_t:point|incp|decp 
// CONST:value|NUM
//       value OP value
// OP: +|-
//point: ( reg )
//incp: ( reg +)
//decp: ( reg -)
struct oprnd operand(struct tok t)
{

	struct oprnd op1,op2; // hopefully filled in by lower part of parse tree;
	struct tok t1,t2,t3;

	op1.p_r.type = none;
	op1.line = t.linepos; 
	if(t.tok != eol && t.tok != semi) // we are done if terminals found
		{// no terminals lets process futher
		switch(t.tok) //MREG:reg|point_t
			{
			case num:
				op1 = get_constant(t);
				break;
			case leftb: //we have a possible pointer 
				t1= tokeniz(t.linepos);
				if(t1.tok == symbol)
					op2 = get_reg(t1.s); //check if valid reg
				else
					{
					printf("expected register found %s\n",t1.s);
					error_cnt++;
					op1.line = t1.linepos; //continue on from error;
					break;
					}
				if(op2.p_r.type != number && op2.p_r.size == 16) // most likely valid reg found
					{
					t2=tokeniz(t1.linepos);
					if(t2.tok==rightb) // we have valid pointer
						{
						op1 = op2;
						op1.p_r.type=preg;
						op1.line = t2.linepos;
						break;
						}
					else if(t2.tok == minus || t2.tok == plus)  // if +/- valid else invalid pointer structure
						{
						t3=tokeniz(t2.linepos);
						if(t3.tok ==  rightb) // have valid pointer 
							{
							op1 = op2; // transfer values for return
							if( t2.tok == minus) // fixup type
								op1.p_r.type = decp;
							else
								op1.p_r.type = incp;
							op1.line = t3.linepos;
							break;

							}
						else // here if unexpected
							{
							printf("expected \')\' found %s\n",t3.s);
							error_cnt++;
							op1.line = t3.linepos; // try to recover here
							break;
							}		
						} 
					}
				 else // we have incorect reg type
					{
					printf("expected an address reg found %s\n",t1.s);
					op1.line = t1.linepos; // try to recover here
					error_cnt++;
					break;
					}
			case symbol: // check reg will look valid else will try and get symbolic value
					op1 = get_reg(t.s);
					op1.line = t.linepos;
					if(op1.p_r.type == number) // we have symbol
						op1 = get_constant(t); // we call this in case there is math to be done
					else // must reg so set type to reflect this.
						op1.p_r.type = reg;
					break;	 
			}
	}

	return(op1);
}
// CONST:value|NUM
//       value OP value
struct oprnd get_constant(struct tok t)
{

	struct tok t1, t2;
	struct oprnd op1;
	int n;	// this is the acumulator
	if(t.tok == symbol)
		n = get_symbol_v(t.s); //should be valid symbol for now we assume ok
	else
		n = t.value; // the lexer has converted value
	op1.p_r.type = number; // if here must have some value;
	op1.p_r.code = regVAL;
	op1.value = n;
	t1 = tokeniz(t.linepos);
	switch(t1.tok)
		{
		case plus:
			t2 = tokeniz(t1.linepos);
			if(t2.tok == num)
				n = n + t2.value; // we are done
			else if(t2.tok == symbol)
				n = n + get_symbol_v(t2.s); // again very limited errror checking.
			else
				{
				error_cnt++; 
				printf("Error expect a value got %s\n",stokens[t2.tok]);
				}
			op1.line = t2.linepos;
			op1.value = n;
			break;
		case minus:
			t2 = tokeniz(t1.linepos);
			if(t2.tok == num)
				n = n - t2.value; // we are done
			else if(t2.tok == symbol)
				n = n - get_symbol_v(t2.s); // again very limited errror checking.
			else 
				{
				error_cnt++; 
				printf("Error expect a value got %s\n",stokens[t2.tok]);
				}
			op1.line = t2.linepos;
			op1.value = n; 	
			break;
		case comma: //the value of n is the final value
		case eol:
		case semi:
			// all valid syntax return with value push back on token stack
			op1.line = t.linepos;
			break;
		default:
			printf("invalid syntax found %s \n", t1.s);  
			op1.line = t.linepos; //backup;
			op1.p_r.type = none; //can't trust values found
			op1.value = -1;

		}
	return(op1);
}
// This called with either a reg value or a symbol
// it should have these to possible config if the rest is ok
//		
struct oprnd get_reg(char *s)
{
		struct oprnd r1;
		int i,n;
//		char temp[20];

//		strcpy(temp,s);

		for ( i=0; i < NUM_REG+1; i++)
			{
			if (strcmp(s,r[i].name) == 0)
				break;
			}
		if(i == NUM_REG+1)
			{
				n= get_symbol_v(s);
				r1.p_r.type = number;
				r1.p_r.code = regVAL;
				r1.value = n;
			}
		else
			{
			r1.p_r = r[i];
			}
	return(r1);				
}
// Make smart toupper
// comment unchanged 
// Strings unchanged
// Char const unchaged.
char* toUpper(char* s) {
	bool string = false, comment = false, char_con =false;	
  for(char *p=s; *p; p++)
	{
	switch(*p)
			{
			case '\"':
				string = !string;
				break;
			case '\'':
				char_con = !char_con;
				break;
			case ';':
				if(!comment)
					comment = true; // once no more conversion
				break;
			}
	if(comment || char_con || string)
		*p =*p;
	else
		*p=toupper(*p);

	}
  return s;
}
void dumpHex(void)
{
   FILE *fp;
   int cnt = 1;
   fp = fopen("a.out" , "w");
   if(fp == NULL) {
      perror("Error output opening file");
	}
	fprintf(fp,"v2.0 raw\n");
		for ( int i=0; i < 32768; i++)
			{
			if( control_rom[i] == control_rom[i+1]) // we have a run
				{ 
				cnt++;
				} else
				{
					if(cnt == 1)
						fprintf(fp,"%02x\n",control_rom[i]);
					else 
						fprintf(fp,"%d*%02x\n",cnt,control_rom[i]);
					
					cnt =1;
				}
			}
	fclose(fp);
	printf("Hex dump\n");
	for ( int i=0; i < address+1; i++)
		{
			int j = i % 16;
			if ( j == 0)
			{ 
				printf("\n%04x:",i);
			}
		printf("%02x ",control_rom[i]);
		}
	printf("\n");
}
