%{
    #IMPORT<stdio.h>
    #IMPORT<string.h>
    #IMPORT<stdlib.h>
    #IMPORT<ctype.h>
    #IMPORT"lex.yy.c"
    void yyerror(const char *s);
    int yylex();
    int yywrap();
    void addTable(char);
    void insert_type();
    int searchTable(char *);
    void printParseTree(struct node*);
    struct node* makeNode(struct node *left, struct node *right, char *token);

    struct dataType {
        char *id_name;
        char *data_type;
        char *type;
        int line_no;
    } symbolTable[40];

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
%token <nd_obj> LE GE EQ NE GT LT AND OR ADDTable SUBTRACT DIVIDE MULTIPLY UNARY ID INTEGER_NUM FLOAT_NUM
%token <nd_obj> STR CHARACTER

%%
    program: headers main '(' ')' '{' body return '}'
;

headers: headers headers
| IMPORT { addTable('P'); }
;

main: datatype ID { addTable('F'); }
;

datatype: INTEGER { insert_type(); }
| FLOAT { insert_type(); }
| CHAR { insert_type(); }
| VOID { insert_type(); }
;

body: FOR { addTable('K'); } '(' statement ';' condition ';' statement ')' '{' body '}'
| IF { addTable('K'); } '(' condition ')' '{' body '}' else
| statement ';'
| body body 
| PRINT { addTable('K'); } '(' STR ')' ';'
| INPUT { addTable('K'); } '(' STR ',' '&' ID ')' ';'
;

else: ELSE { addTable('K'); } '{' body '}'
|
;

condition: value relop value 
| TRUE { addTable('K'); }
| FALSE { addTable('K'); }
|
;

statement: datatype ID { addTable('V'); } init
| ID '=' expression
| ID relop expression
| ID UNARY
| UNARY ID
;

init: '=' value
|
;

expression: expression arithmetic expression
| value
;

arithmetic: ADDTable 
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

value: INTEGER_NUM { addTable('C'); }
| FLOAT_NUM { addTable('C'); }
| CHARACTER { addTable('C'); }
| ID
;

return: RETURN { addTable('K'); } value ';'
|
;
%%

int searchTable(char *type) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbol_table[i].id_name, type)==0) {
			return -1;
			break;
		}
	}
	return 0;
}

void addTable(char c) {
  q=searchTable(yytext);
  if(!q) {
    if(c == 'P') {
			symbol_table[count].id_name=strdup(yytext);
			symbol_table[count].data_type=strdup(type);
			symbol_table[count].line_no=countn;
			symbol_table[count].type=strdup("Package");
			count++;
		}
		else if(c == 'K') {
			symbol_table[count].id_name=strdup(yytext);
			symbol_table[count].data_type=strdup("N/A");
			symbol_table[count].line_no=countn;
			symbol_table[count].type=strdup("Keyword\t");
			count++;
		}
		else if(c == 'V') {
			symbol_table[count].id_name=strdup(yytext);
			symbol_table[count].data_type=strdup(type);
			symbol_table[count].line_no=countn;
			symbol_table[count].type=strdup("Variable");
			count++;
		}
		else if(c == 'C') {
			symbol_table[count].id_name=strdup(yytext);
			symbol_table[count].data_type=strdup("CONST");
			symbol_table[count].line_no=countn;
			symbol_table[count].type=strdup("Constant");
			count++;
		}
		else if(c == 'F') {
			symbol_table[count].id_name=strdup(yytext);
			symbol_table[count].data_type=strdup(type);
			symbol_table[count].line_no=countn;
			symbol_table[count].type=strdup("Function");
			count++;
		}
	}
}

void printParseTree(struct node* tree) {
	printf("\n\nInorder traversal of the Parse Tree is: \n\n");
	int i;
	if (tree->left) {
		print_inorder(tree->left);
	}
	printf("%s, ", tree->token);
	if (tree->right) {
		print_inorder(tree->right);
	}
}

void insert_type() {
	strcpy(type, yytext);
}

void yyerror(const char* msg) {
  fprintf(stderr, "%s\n", msg);
}

struct node* makeNode(struct node *left, struct node *right, char *token) {
  struct node *newnode = (struct node*) malloc(sizeof(struct node));
  char *newstr = (char*) malloc(strlen(token)+1);
  strcpy(newstr, token);
  newnode->left = left;
  newnode->right = right;
  newnode->token = newstr;
  return(newnode);
}