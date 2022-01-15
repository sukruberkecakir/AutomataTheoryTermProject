%{
    #include<stdio.h>
    #include<string.h>
    #include<stdlib.h>
    #include<ctype.h>
    #include"lex.yy.c"
    void yyerror(const char *s);
    int yylex();
    int yywrap();
    void addTable(char);
    int search(char *);
    void insert_type();
    void printTree(struct node*);
    void printInorder(struct node *);
    struct node* makeNode(struct node *left, struct node *right, char *token);
	extern int countn;

    struct dataType {
        char * id_name;
        char * data_type;
        char * type;
        int line_no;
    } symbolTable[100];
    int count=0;
    int q;
    char type[10];
    extern int countn;
    struct node *head;
    struct node { 
	struct node *left; 
	struct node *right; 
	char *token; 
    };
%}

%union { 
	struct var_name { 
		char name[100]; 
		struct node* nd;
	} nd_obj; 
} 

%token VOID
%token <nd_obj> PRINT INPUT INTEGER FLOAT CHAR RETURN FOR WHILE IF ELSE IMPORT TRUE FALSE
%token <nd_obj> LE GE EQ NE GT LT AND OR ADD SUBTRACT DIVIDE MULTIPLY UNARY ID INTEGER_NUM FLOAT_NUM
%token <nd_obj> STR CHARACTER BREAK CONTINUE MAIN '(' ')' '{' '}'
%type <nd_obj> program import main global_statement global_decl global //functions function_definition 
%type <nd_obj> datatype body else condition statement init expression arithmetic relop value return
%start program

%%

program: import global_statement { $$.nd = makeNode($1.nd, $2.nd, "program"); head = $$.nd; }
|
;

main: datatype MAIN { addTable('F'); } '(' ')' '{' body return '}' { $2.nd = makeNode($7.nd, $8.nd, "main"); $$.nd = $2.nd}
;

global_statement:  global_decl main { $$.nd = makeNode($1.nd, $2.nd, "Global_Statements_Main"); }
| main {$$.nd = $1.nd;}
;

global_decl: global_decl global { $$.nd = makeNode($1.nd, $2.nd, "Global_variables"); }
| global {$$.nd = $1.nd;}
;

global: datatype ID { addTable('G'); } init ';' { $2.nd = makeNode(NULL, NULL, $2.name); $$.nd = makeNode($2.nd, $4.nd, "declaration"); };

import: import import { $$.nd = makeNode($1.nd, $2.nd, "imports"); }
| IMPORT { addTable('P'); } { $$.nd = makeNode(NULL, NULL, $1.name); }
|
;
/*
functions: functions function_definition { $$.nd = makeNode($1.nd, $2.nd, "Function"); }
| function_definition { $$.nd = $1.nd; }
|
;

function_definition: datatype ID { addTable('F'); } '(' datatype ID ')' '{' body '}'
| datatype ID { addTable('F'); } '('{ yyerror("Error in here"); } ')' '{' body '}'
;*/

datatype: INTEGER { insert_type(); }
| FLOAT { insert_type(); }
| CHAR { insert_type(); }
| VOID { insert_type(); }
;

body: FOR { addTable('K'); } '(' statement ';' condition ';' statement ')' '{' body '}' { struct node *temp = makeNode($6.nd, $8.nd, "CONDITION"); struct node *temp2 = makeNode($4.nd, temp, "CONDITION"); $$.nd = makeNode(temp2, $11.nd, $1.name); }
| WHILE {addTable('K'); } '(' condition ')' '{' body '}' { $$.nd = makeNode($4.nd, $7.nd, $1.name); }
| IF { addTable('K'); } '(' condition ')' '{' body '}' else { struct node *iff = makeNode($4.nd, $7.nd, $1.name); 	$$.nd = makeNode(iff, $9.nd, "if-else"); }
| statement ';' { $$.nd = $1.nd; }
| body body { $$.nd = makeNode($1.nd, $2.nd, "statements"); }
| PRINT { addTable('K'); } '(' STR ')' ';' { $$.nd = makeNode(NULL, NULL, "print"); }
| INPUT { addTable('K'); } '(' STR ',' '&' ID ')' ';' { $$.nd = makeNode(NULL, NULL, "input"); }
| BREAK { addTable('K'); } ';'  { $$.nd = makeNode(NULL, NULL, "break"); }
| CONTINUE { addTable('K'); } ';' { $$.nd = makeNode(NULL, NULL, "continue"); }
;

else: ELSE { addTable('K'); } '{' body '}' { $$.nd = makeNode(NULL, $4.nd, $1.name); }
| { $$.nd = NULL; }
;

condition: value relop value { $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| TRUE { addTable('K'); $$.nd = NULL; }
| FALSE { addTable('K'); $$.nd = NULL; }
| { $$.nd = NULL; }
;

statement: datatype ID { addTable('V'); } init { $2.nd = makeNode(NULL, NULL, $2.name); $$.nd = makeNode($2.nd, $4.nd, "declaration"); }
| ID '=' expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, "="); }
| ID relop expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| ID UNARY { $1.nd = makeNode(NULL, NULL, $1.name); $2.nd = makeNode(NULL, NULL, $2.name); $$.nd = makeNode($1.nd, $2.nd, "ITERATOR"); }
| UNARY ID { $1.nd = makeNode(NULL, NULL, $1.name); $2.nd = makeNode(NULL, NULL, $2.name); $$.nd = makeNode($1.nd, $2.nd, "ITERATOR"); }
;

init: '=' value { $$.nd = $2.nd; }
| { $$.nd = makeNode(NULL, NULL, "NULL"); }
;

expression: expression arithmetic expression { $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| value { $$.nd = $1.nd; }
;

arithmetic: ADD 
| SUBTRACT 
| MULTIPLY
| DIVIDE
;

relop: LT
| GT
| LE
| GE
| EQ
| NE
;

value: INTEGER_NUM { addTable('C'); $$.nd = makeNode(NULL, NULL, $1.name); }
| FLOAT_NUM { addTable('C'); $$.nd = makeNode(NULL, NULL, $1.name); }
| CHARACTER { addTable('C'); $$.nd = makeNode(NULL, NULL, $1.name); }
| ID { $$.nd = makeNode(NULL, NULL, $1.name); }
;

return: RETURN { addTable('K'); } value ';' { $1.nd = makeNode(NULL, NULL, "return"); $$.nd = makeNode($1.nd, $3.nd, "RETURN"); }
| { $$.nd = NULL; }
;

%%

int main() {
    yyparse();
    printf("\n\n \t\t\t\t\t\t LEXICAL ANALYSIS \n\n");
	printf("\nSYMBOL   DATATYPE   TYPE   LINE NUMBER \n");
	printf("_______________________________________\n\n");
	int i=0;
	for(i=0; i<count; i++) {
		printf("%s\t%s\t%s\t%d\t\n", symbolTable[i].id_name, symbolTable[i].data_type, symbolTable[i].type, symbolTable[i].line_no);
	}
	for(i=0;i<count;i++){
		free(symbolTable[i].id_name);
		free(symbolTable[i].type);
	}
	printf("\n\n");
	printf("\t\t\t\t\t\t SYNTAX ANALYSIS \n\n");
	printTree(head); 
	printf("\n\n");
}

int searchTable(char *type) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbolTable[i].id_name, type)==0) {
			return -1;
			break;
		}
	}
	return 0;
}

void addTable(char c) {
    q=0;//searchTable(yytext);
	if(q==0) {
		if(c=='P') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Package");
			count++;
		}
		else if(c=='K') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("N/A");
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Keyword\t");
			count++;
		}
		else if(c=='V') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Variable");
			count++;
		}
		else if(c=='C') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("CONST");
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Constant");
			count++;
		}
		else if(c == 'F') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Function");
			count++;
		}
		else if(c=='G') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup(type);
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Global Variable");
			count++;
		}
		q=-1;
		return;
    }
	
}

struct node* makeNode(struct node *left, struct node *right, char *token) {	
	struct node *newnode = (struct node *)malloc(sizeof(struct node));
	char *newstr = (char *)malloc(strlen(token)+1);
	strcpy(newstr, token);
	newnode->left = left;
	newnode->right = right;
	newnode->token = newstr;
	return(newnode);
}

void printTree(struct node* tree) {
	printf("\n\n Inorder traversal of the Parse Tree: \n\n");
	printInorder(tree);
	printf("\n\n");
}

void printInorder(struct node *tree) {
	int i;
	if (tree->left) {
		printInorder(tree->left);
	}
	printf("%s, ", tree->token);
	if (tree->right) {
		printInorder(tree->right);
	}
}

void insert_type() {
	strcpy(type, yytext);
}

void yyerror(const char* msg) {
    fprintf(stderr, "%s in %dth line\n", msg, countn);
}