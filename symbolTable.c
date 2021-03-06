#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

/*
Most of the changes will be in here for two pass control
*/

#define ROM_SZ 0xffff 
int control_rom[ROM_SZ];

// This structure contains data to fix up forward refernces this will now just be used
// to create xreffence 
struct fixup {	
				int line;    // 
				int address; // address of locations to fix once defined
				struct fixup *next; // pointer null if end of list
				} ;
//The symbol table
//The address field is set to zero so simple math like +/- will work on the symbols
//A small change make this work OK, I hope.
struct symbol {
				char *smb;
				int address;	// 16 bit address
				int line;		// line where defined
				int flags;
				struct fixup *f; // xcross reffence
				};
//Flags to indicate the state of the symbol
// defined means sysmbol has a valid address
// If math is attempted with undefined sysmbol we will report an error 
// This makes it easier to manage. 

enum state { defined,undefined,addressok,end };
// A hard coded symbol table size this may need to be increased
#define TABLESZ 2000
struct symbol symbol_table[TABLESZ];
int address = 0; // default start address
int last_defined_symbol = 0; // Contains the address of the last defined symbol
extern int line_cnt;
extern int error_cnt;
extern int pass;
// function declarations
void clear_symboltbl(void);

int get_symbol(char *buf);
void define_symbol(char *buf, int stat);
int new_symbol(char *buf,int stat);
void add_fixup(int index);
void fixup(int index);
void dump_symtable(void);
void install_symbol(char *s);
int get_symbol_v(char *s);
int get_last_symbol_address(void);
int get_last_symbol_index(void);
int update_value(int v,int index);
void remove_fixup(int add, int index);
void fake_install(char *s);

// helper functions
int update_value(int v, int index)
{
	// this routine is used by equ to set address field to a a value
	// this done this way to hide symbol table internals from main
	symbol_table[index].address = v;
//	remove_fixup(address+1, index);
//	fixup(index); //this should fixup all reffs to new value
}
// should only be called durring pass 2
void fake_install(char *s)
{
	int index;
	index = get_symbol(s); // this should not fail durring pass 2
	last_defined_symbol = index;
}
int get_last_symbol_index(void)
{
		return(last_defined_symbol);
}
int get_last_symbol_address(void)
{
return(symbol_table[last_defined_symbol].address);	
}
//
// This routine returns the value of a symbol if it is the first time seen the value is zero
// This not an error currently math seems to work but will fail I will look at code to stop
// First if symbol found value is extracted and returned if not found it is insertsed as undefined 
// Symbol this will be fixed up once the symbol is seen it is an error to finish undefined symbol.
//
int get_symbol_v(char *s)
{
	int index;
	static int lastaddress = 0;
	index = get_symbol(s);
	if(index == -1 && pass==1 ) // An error if undefined in pass 2
		{
		index = new_symbol(s,undefined); // if here we have an undefined symbol;
		if(index == -1) // things are really bad if -1 close eyes and hope it goes away
			return(index);
		}
	else 
		{// if here we have valid index
		if(lastaddress !=address && pass == 1) // due to the stucture of the parser this is called twice
			add_fixup(index); // this now just for xreffence
		index = symbol_table[index].address;
		}
	if(index == -1 && pass == 2 ) 
		{
		error_cnt++;
		printf("In %d we have undefined symbol %s\n",line_cnt,s);
		return(0); // just keeps rest code happy should recover from this. 
		}	
	lastaddress = address;
	return(index);
}

// This public routine calls the symbol table mangament rountine to installs a symbol
// the symbol will create error if symbol already defined
// we may add local and global scope hence this doesn't neeed to know.

void install_symbol(char *s)
{
	define_symbol( s, addressok);		
}
void clear_symboltbl(void)
{
	symbol_table[0].flags = end;
	symbol_table[0].f = NULL;
}
//
// Previous sections have now checked if valid of not and only valid string 
//
void define_symbol(char *buf, int stat) // this code tries to insert symbol into table
{

	int index;
// if here we have a valid symbol.
	if((index = get_symbol(buf))!= -1) // check if already defined
	{
			last_defined_symbol = index;
			if(symbol_table[index].flags == defined )
				{
				if(stat == addressok)
					{
					error_cnt++;
					printf("****************Error-symbol %s already defined\n",buf);
					}
				}
			else if (symbol_table[index].flags == undefined ) // we need to fixup 
				{
					symbol_table[index].flags = defined;
					symbol_table[index].address = address;
					symbol_table[index].line = line_cnt;
//					fixup(index); // In two pass this should not be required
				}
	} else
		new_symbol(buf,stat); // create new sysmbol in table

}
// This routine inserts a new symbol into the symbol table
// Currently as comment indicates error processing here needs work.
// Stat is used to indicate we have and address for this symbol.
// returns index of new symbol
int new_symbol(char *buf,int stat) // for simplier code it is assumed dups are already checked
{
		int i=0;
		char * tbuf;
		while(symbol_table[i].flags != end || i == TABLESZ)
			i++;
		if(i == TABLESZ)
 			{
			error_cnt++; // I feel if this happens we are broken
			printf("*********Error - symbol table overflow\n");
			return(-1); // this is really bad error processing
 			}
		// we have found free slot
		if( stat == addressok ) // we define the address
				{
				symbol_table[i].address = address;
				symbol_table[i].flags = defined; //the symbol is defined with address;
				last_defined_symbol = i; // keep track 
				symbol_table[i].line = line_cnt;
				}
		else // this section manages undefined symbols
		{
			symbol_table[i].flags = undefined;
			symbol_table[i].f = (struct fixup *)NULL; // tag so fixup places data link here
			add_fixup(i); // go add fixup chain to this symbol
		}
		tbuf = malloc(strlen(buf)+1); //allocate mem
		strcpy(tbuf,buf);
		symbol_table[i].smb = tbuf;
		symbol_table[i+1].flags = end;
	return(i);		
}
// this removes a fix up entery used by EQU presently
void remove_fixup(int add, int index)
{
	struct fixup *f1,*f2;
	int loc_add;
	
	f1 = symbol_table[index].f; // get fixup chain;
	loc_add = symbol_table[index].address;
	if( f1 != NULL) // have chain;
		{ //look for our address
			if(f1->address == add) // found
				{
				f2 = f1->next;
				f1 = f2; //remove f1
				}
 			while(f1->next != NULL)
				{
				if(f1->address == add) // found
					{
					f2 = f1->next;
					f1 = f2; //remove f1
					}
				else
					f1 = f1->next; // Walk chain
				}
		}
}
void add_fixup(int index)
{
	struct fixup *fix,*f1;
	
	f1 = symbol_table[index].f; // get fixup chain;(
	fix = malloc(sizeof(struct fixup));
	fix->address = address;
	fix->line = line_cnt;
	fix->next = NULL;
	if( f1 != NULL) // have chain;
 		{
		while(f1->next != NULL)
			f1 = f1->next; // Walk chain
		f1->next = fix ; //insert new link
		}
	else // at head
		symbol_table[index].f = fix;
}
// search symbol table
// There no optimisations here an idea might be to hash symbols and use hash value
// As index
// but currrently it works fine.

int get_symbol(char *new_sym)
{
	int i =0;
	while(symbol_table[i].flags != end)
	{
		if ( strcmp(symbol_table[i].smb, new_sym) == 0)
			return i;
		i++;
	}
	return -1;			
}
// This function fixes up the address of all refences to the symbol now
// that the addrsss is known.
void fixup( int index)
{
	// this code walk the linked list of address and place symbol address in locations
	struct fixup *f1;
	int loc_add;
	
	f1 = symbol_table[index].f; // get fixup chain;
	loc_add = symbol_table[index].address;
	if( f1 != NULL) // have chain;
 		{
		control_rom[f1->address] = (loc_add &0xff);
		control_rom[f1->address+1] = (loc_add & 0xff00) >> 8;
		while(f1->next != NULL)
			{
			f1 = f1->next; // Walk chain
			control_rom[f1->address] = (loc_add &0xff);
			control_rom[f1->address+1] = (loc_add & 0xff00) >> 8;
			}
		}
}
void dump_symtable(void)
{
	struct fixup *f1;

	int index =0;
	while(symbol_table[index].flags != end)
	{
		printf("%s: %04x line: %d\n",symbol_table[index].smb,symbol_table[index].address,symbol_table[index].line);
		if(symbol_table[index].flags == undefined)
		{
			error_cnt++;
			printf("*********Error symbol undefined\n");
		}
		if(symbol_table[index].f != NULL) // we have fix list
			{
			f1 = symbol_table[index].f;
			printf("Address: %04x ",f1->address);
			while(f1->next !=NULL)
				{
				f1 = f1->next;
				printf("%04x ",f1->address);
				} 
			printf("\n");
  			}
		if(symbol_table[index].f != NULL) // we have fix list
			{
			f1 = symbol_table[index].f;
			printf("line: %04d ",f1->line);
			while(f1->next !=NULL)
				{
				f1 = f1->next;
				printf("%04x ",f1->line);
				} 
			printf("\n");
  			}
		printf("\n");
		index++;
	}
	if( error_cnt > 0 )
		printf("%d error(s) found \n",error_cnt); 
}
