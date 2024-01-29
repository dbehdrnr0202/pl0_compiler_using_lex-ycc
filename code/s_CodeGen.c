#include "s_mypl0Ast.h"
#include "s_pl0Ast.tab.h"
#include "s_interpreter.c"
#include <string.h>
#include <stdlib.h>


#define CONST 	0
#define VAR 	1
#define PROC 	2
#define IDENT 	3  /* CONST + VAR */
#define ARR		4

#define TBSIZE 100
//추가한거
#define LVSIZE 20
#define MAX_ARR_SIZE 	20
#define MAX_ID_SIZE		20
#define HASHSIZE 23
//hash bucket: -1은 unlink되어있는 bucket
int hashBucket[HASHSIZE] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
//해당 level의 첫 table의 엔트리 인덱스를 저장한다.
int level_table[LVSIZE];
//hash function
int Hash(char *);

void Block(myAstNode *);
void Statement(myAstNode *);
void Expression(myAstNode *);

int Lookup(char *, int);  // lookup sym name for type, 
		      // if found setup LDiff/OFFSET and return 1 else return 0
void Enter(char *, int, int, int); // symbol insert
void SetBlock();
void ResetBlock();
void DisplayTable(); // symbol table dump
int GenLab(char *);	// label 생성(번호와 label) 및 현재 코드주소 저장
void EmitLab(int);	// label 출력 및 forward jump 주소 확정
void EmitLab1(int);	// label 출력 및 backward jump 주소 저장
void EmitPname(char *label);	// procedure 이름 출력
void Emit1(char *, fct, int, int);  // INT, LOD, LIT, STO 코드생성
void Emit2(char *, int, char *);  // CAL 코드생성
void Emit3(char *, fct, int);	// jmp, jpc 코드 생성
void Emit(char *, int);	// opr 코드생성

//배열을 symbol table에 enter한다
void EnterArr(char*, int, int, int, int, int);
//symbol table에 동일한 배열이 존재하는지 확인한다.
int LookupArr(char *, int);
//ldi, sti 코드를 emit한다.
void Emit4(char*, int);
//입력받은 배열을 저장하는 노드의 값을 코드로 emit한다
void Arr(myAstNode*); 
//입력받은 배열을 저장하는 노드의 주소를 코드로 emit한다.
void GetArrAddr(myAstNode*);

// symbol table
struct {
	char name[MAX_ID_SIZE];
	int type;		/* 0-CONST	1-VARIABLE	2-PROCEDURE */
	int lvl;
	int offst;
	int link;
	int length;
	int index;
	} table[TBSIZE];
	
int tx=0, cdx=0, level=0, lev=0;
int LDiff=0, Lno=0, OFFSET=0;
int ALen=0;

Instruction i;
int Lab[20];

void Traverse(myAstNode * node) {
	Block(node); Emit("END", 7); DisplayTable();
}

void Arr(myAstNode* node)	{
	GetArrAddr(node);
	Emit4("LDI", Ldi);
}
void GetArrAddr(myAstNode* node)	{
	myAstNode * link;
	if (node->op==TARR)	{
		link=node->left;
		Expression(link->right);
		if (LookupArr(link->value.ident, link->right->value.num)==-1)	return;
		Emit1("LDA", Lda, LDiff, OFFSET - ALen + 1);
		Emit("ADD",2);
	}			
}

void Block(myAstNode * node) {	
	myAstNode * link;
	char Lname[10]; int lab;
	int tx_save, offset=3;
	//	if(node == NULL ) return ;
	if(node->op!=0) printf("block dec error\n");
	lab=GenLab(Lname); Emit3("JMP", Jmp, lab); // jump to block main body
	// processing declaration part
	node=node->left;
	if (node->op==TCONST) {
		link = node->left;
		while(link)	{
			if (link->op!=-2)
				printf("ERROR: Wrong Decl type in const Decl\n");
			Enter(link->value.ident, CONST, level, link->right->value.num);
			link = link->right->right;
		}
		node=node->right;		
	}	
	if (node->op==TVAR) {
		link = node->left;
		while(link)	{ 
			//var에 배열을 추가함
			if (link->op==TARR)	{
				myAstNode* arr_node = link->left;
				int arr_size = arr_node->right->value.num;
				for (int index = 0;index<arr_size;index++)
					EnterArr(arr_node->value.ident, ARR, level, offset++, arr_size, index);
				link=link->right;
			}
			else {
				Enter(link->value.ident, VAR, level, offset++); 
				link = link->right; 
			}
		}
		node=node->right;
	}
	while (node->op==TPROC) { //printf("processing proc\n");
		link=node->left;
		
		Enter(link->value.ident,PROC,level,cdx);
		tx_save=tx; 
		SetBlock();
		
		EmitPname(link->value.ident);
		Block(link->right);
		Emit("RET", 0); 
		DisplayTable(); tx=tx_save; //--level;
		ResetBlock();

		node=node->right;			 
	}
	// processing statement part
	EmitLab(lab); Emit1("INT", Int, 0, offset);
	Statement(node);  
}
//condition완성
void Condition(myAstNode * node) {
	if(node == NULL ) { printf(" expression error in Condition\n"); return; }
	switch(node->op) {
		case ODD:	
			Expression(node->left); Emit("ODD",6);break;	
		case '=':	//
			Expression(node->left); Expression(node->left->right); Emit("=",8);break;	
		case NE:	//
			Expression(node->left); Expression(node->left->right); Emit("NE",9);break;	
		case '<':	//
			Expression(node->left); Expression(node->left->right); Emit("LT",10);break;	
		case '>':	//
			Expression(node->left); Expression(node->left->right); Emit("GT",12);break;	
		case GE:	//
			Expression(node->left); Expression(node->left->right); Emit("GE",11);break;	
		case LE:	
			Expression(node->left); Expression(node->left->right); Emit("LE",13);break;	
	}
}
//expression 완성
void Expression(myAstNode * node) {
	if(node == NULL ) { printf(" expression error\n"); return; }
	switch(node->op) {
		//-1 : number일 경우
		case -1: 	Emit1("LIT", Lit, 0, node->value.num); 		break;
		//-2 : ident일 경우
		case -2:	//ID일 경우(const인 경우와 var인 경우가 존재한다 그렇기에 symboltable을 lookup해야한다.)
					//lookup을 한 결과가 -1가 아닐 경우 찾은 것 : emit한다
					if (Lookup(node->value.ident, CONST)!=-1)	{
						Emit1("LIT", Lit, LDiff, OFFSET);
						break;
					}
					if (Lookup(node->value.ident, VAR)!=-1)		{
						Emit1("LOD", Lod, LDiff, OFFSET); 
						break;
					}
					printf("ERROR no matching symbol\n");
					break;
		// unary operator
		case NEG:	//
			Expression(node->left); Emit("NEG",1); break;
		// binary operator
		case '+':	Expression(node->left); Expression(node->left->right); Emit("ADD",2); break;
		case '-':	Expression(node->left); Expression(node->left->right); Emit("SUB",3); break;
		case '*':	Expression(node->left); Expression(node->left->right); Emit("MUL",4); break;
		case '/':	Expression(node->left); Expression(node->left->right); Emit("DIV",5); break;
		case TARR:	Arr(node);	break;
	}
}

void Statement(myAstNode * node) {
	// Statement Part
	char Lname1[10], Lname2[10]; int lab1, lab2;
	//만약에 if then else를 만들거면 Lname3를 넣어야한다.
	char Lname3[10]; int lab3;

	myAstNode * temp;
	if (node==NULL)	return;
	temp=node->left;
	switch(node->op) {
	case ASSIGN:	if(temp->op==-2)	{
						Expression(temp->right); 
						if (Lookup(temp->value.ident, VAR)!=-1)
							Emit1("STO", Sto, LDiff, OFFSET);
					}
					else if (temp->op==TARR&&LookupArr(temp->left->value.ident, temp->right->value.num)!=-1)	{
						GetArrAddr(temp);
						Expression(temp->right);
						Emit4("STI", Sti);
					}
					break;
					//
	case TCALL:		if (Lookup(temp->value.ident, PROC)!=-1)	{
						Emit2("CAL", level, temp->value.ident);
					}
					break;
	case TBEGIN:	while (temp) { Statement(temp); temp=temp->right; }
					break;
	case TIF: 	 
				//if then
				if (!(temp->right->right))	{
					Condition(temp);
				 	lab1=GenLab(Lname1); 
				 	Emit3("JPC", Jpc, lab1); 
				 	Statement(temp->right);
				 	EmitLab(lab1);
				}
				//if then else 
				if (temp->right->right)	{
					Condition(temp);
				 	lab2=GenLab(Lname2);
					Emit3("JPC", Jpc, lab2);
					lab1=GenLab(Lname1); 
					Emit3("JMP", Jmp, lab1);
					EmitLab(lab1);
					Statement(temp->right);
					lab3=GenLab(Lname3);
					
					Emit3("JMP", Jmp, lab3);
				 	
					EmitLab(lab2);
					Statement(temp->right->right);
					Emit3("JMP", Jmp, lab3);
					EmitLab(lab3);
				}
				break;
	case TWHILE: 
				 lab1=GenLab(Lname1); 
				 EmitLab1(lab1); Condition(temp); 
				 lab2=GenLab(Lname2); 
				 Emit3("JPC", Jpc, lab2); Statement(temp->right);
				 Emit3("JMP", Jmp, lab1); 
				 EmitLab(lab2); break;
	otherwise:
			
		break;
	}
}


int Lookup(char *name, int type) {
	int hash = Hash(name);
    int link = hashBucket[hash];
    while (1) {
		//not founded
		if (link == -1) {
			//printf("lookup error: %s\n", name);
            //yyerror("cannot find the symbol");
            break;
        }
		//found
		if (strcmp(name, table[link].name) == 0&&type==table[link].type) {
			OFFSET = table[link].offst;
			LDiff = level-table[link].lvl;
            return 0;
        }
        link = table[link].link;
    }
    return link;
}
int LookupArr(char *name, int index) { //int idx=tx;
	int hash = Hash(name);
    int link = hashBucket[hash];
    while (1) {
		//not founded
		if (link == -1) {
			break;
        }
		//found
		if (strcmp(name, table[link].name) == 0&&table[link].type==ARR) {
			OFFSET = table[link].offst;
			LDiff = level-table[link].lvl;
			ALen = table[link].length;
            return 0;
        }
        link = table[link].link;
    }
    return link;
}
void Enter(char *name, int type, int lvl, int offst) {
//procedure일 경우 offst: 주소값
//const일 경우 offst: 들어갈 값
//tx : symbol table index값
	if (Lookup(name, type)!=-1)
		printf("ERROR: duplicated symbol entered\n");
	int hash = Hash(name);
	//이전 hash bucket에 있었던 값을 저장 : link로 이어주기
    int link = hashBucket[hash];
    hashBucket[hash] = tx;
	strcpy(table[tx].name, name);
	table[tx].type = type;
	table[tx].lvl = lvl;
	table[tx].offst = offst;
	table[tx].link = link;                                                                                                                                                                                                                               
	tx++;
}
void EnterArr(char *name, int type, int lvl, int offst, int length, int index) {
	int hash = Hash(name);
	//이전 hash bucket에 있었던 값을 저장 : link로 이어주기
    int link = hashBucket[hash];
    hashBucket[hash] = tx;
	strcpy(table[tx].name, name);
	table[tx].type = type;
	table[tx].lvl = lvl;
	table[tx].offst = offst;
	table[tx].link = link;                                        
	table[tx].length = length;     
	table[tx].index = index;                                                                                                                                                                                  
	tx++;
}
void DisplayTable() { 
	int idx=tx;
	printf("=======Display Table=======\n");
	printf("name\ttype\tlvl\toffst\tindex\n");
	while (--idx>=0) { 
		printf("%s	%d	%d	%d	%d\n", table[idx].name, table[idx].type, table[idx].lvl, table[idx].offst, table[idx].index);
	}
}
int GenLab(char *label) {
	Lab[Lno]=cdx;	
	sprintf(label, "LAB%d", Lno);
	return Lno++;
}
//cdx : code address
void EmitLab(int label) {
	Code[Lab[label]].a=cdx;	
	printf("LAB%d\n", label);
}
void EmitLab1(int label) {
	Lab[label]=cdx;	
	printf("LAB%d\n", label);
}
void EmitPname(char *label) {
	printf("%s\n", label);
}
//INT, LOD, LIT, STO 코드생성
void Emit1(char *code, fct op, int ld, int offst) {
	printf("	%s	%d	%d\n", code, ld, offst);
	i.f=op; i.l=ld; i.a=offst;
	Code[cdx++]=i;
}
//CAL 코드생성
//level difference가 필요하다. 아주 중요하다
//name을 lookup해서 procedure을 찾은 뒤 level difference를 찾을 수 있다.
void Emit2(char *code, int ld, char *name) {	// emit "Cal l,addr"
	//lookup부분만 추가하는게 맞나??
	if (Lookup(name, PROC)==-1)
		printf("ERROR: undeclared procedure used\n");
	//

	printf("	%s	%d	%s\n", code, ld, name);
	i.f=Cal; i.l=ld; i.a=OFFSET; // it must be fixed up
	Code[cdx++]=i;
}
//jmp, jpc 코드생성
void Emit3(char *code, fct op, int label) {
	printf("	%s	LAB%d\n", code, label);
	i.f=op; i.l=0; i.a=Lab[label]; // it must be fixed up
	Code[cdx++]=i;
}

//ldi, sti 코드 생성
void Emit4(char *code, int op)	{
	printf("	%s\n", code);
	i.f=op; i.l=0; i.a=0;
	Code[cdx++]=i;	
}

//opr 코드생성
//code : opr code, op: op num
void Emit(char *code, int op) {
	printf("	%s\n", code);
	i.f=Opr; i.l=0; i.a=op;
	Code[cdx++]=i;	
}

//추가한거
void SetBlock() {
	//블럭에 들어가 레벨이 증가하였기에, 현재 레벨에서 테이블에 처음으로 추가된 엔트리의 인덱스를 level_table에 저장한다. 
	level_table[++level]=tx;
}
void ResetBlock() { 
	//벗어날 때 hash chain을 끊어주는 것이 필요하다
	tx=level_table[level--];
	//벗어나기 이전의 레벨에 해당하는 첫번째 테이블 엔트리의 index(level_table[level]) 보다 같거나 클 경우 이 링크를 끊어준다.
    for (int i = 0; i < HASHSIZE; i++) {
        int link = hashBucket[i];
        while(link >= tx) {
			//계속 링크를 타면서 테이블 엔트리를 확인한다.
            link = table[link].link;
        }
		hashBucket[i]=link;
    }
}

//name 문자열의 모든 문자들의 값들을 합한 뒤, HASHSIZE로 나눈 뒤의 나머지
int Hash(char *name) {
    int hash = 0;
    const char* c;
    c = name;
    while (*c != '\0')    hash += *c++;
    return (hash)%HASHSIZE;
}