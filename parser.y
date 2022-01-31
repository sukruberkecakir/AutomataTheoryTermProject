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
    int searchTable(char *);
    void insert_type();
    void printTree(struct node*);
    void printInorder(struct node *);
	void check_declaration(char *);
	void check_return_type(char *);
	int check_types(char *, char *);
	char *get_DataType(char *);
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
	int sem_errors=0;
	int ic_idx=0;
	int temp_var=0;
	int label=0;
	int is_loop=0;
	int is_fcall=0;
	int is_main = 1;
	//char a[40][50];
	char buff[100];
	char icg[100][100];
	char errors[10][100];
	char reserved[18][18] = {"int", "float", "char", "void", "if", "else", "for", "main", "return", "import", "break", "continue", "print", "while"
							, "true", "false", "f", "F"};
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
	struct var_name2 { 
			char name[100]; 
			struct node* nd;
			char type[5];
	} nd_obj2; 
	struct var_name3 {
			char name[100];
			struct node* nd;
			char if_body[5];
			char else_body[5];
	} nd_obj3;
} 

%token VOID
%token <nd_obj> PRINT INTEGER FLOAT CHAR RETURN FOR WHILE IF ELSE IMPORT TRUE FALSE FLOAT_NUM
%token <nd_obj> LE GE EQ NE GT LT ADD SUBTRACT DIVIDE MULTIPLY UNARY ID INTEGER_NUM
%token <nd_obj> STR CHARACTER BREAK CONTINUE '(' ')' '{' '}' 'f' 'F'
%type <nd_obj> program import global_statement global_decl global function_definition function_specifier func_call semicolon
%type <nd_obj> datatype body else statement return arg body_statements conditional_body operators conditional_body_statements
%type <nd_obj2> init value expression globvalue globexpression globinit
%type <nd_obj3> condition
%start program

%%

program: global_statement { $$.nd = $1.nd; head = $$.nd; }
| program global_statement { $$.nd = makeNode($1.nd, $2.nd, ""); head = $$.nd; }
;

global_statement: global_decl { $$.nd = $1.nd; }
| function_definition {$$.nd = $1.nd;}
| import {$$.nd = $1.nd;}
| { $$.nd = NULL; }
;

global_decl: global_decl global { $$.nd = makeNode($1.nd, $2.nd, "Global_variables"); }
| global {$$.nd = $1.nd;}
;

semicolon: ';'
|error{int temp = countn - 1;sprintf(errors[sem_errors], "Line %d: Semicolon missing!\n", temp);
		sem_errors++;}

global: datatype ID { addTable('G'); } globinit semicolon { $2.nd = makeNode(NULL, NULL, $2.name); 
	int t = check_types($1.name, $4.type); 
	if(t>0) { 
		if(t == 1) {
			struct node *temp = makeNode(NULL, $4.nd, "floattoint"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		} 
		else if(t == 2) { 
			struct node *temp = makeNode(NULL, $4.nd, "inttofloat"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		} 
		else if(t == 3) { 
			struct node *temp = makeNode(NULL, $4.nd, "chartoint"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		} 
		else if(t == 4) { 
			struct node *temp = makeNode(NULL, $4.nd, "inttochar"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		} 
		else if(t == 5) { 
			struct node *temp = makeNode(NULL, $4.nd, "chartofloat"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		} 
		else{
			struct node *temp = makeNode(NULL, $4.nd, "floattochar"); 
			$$.nd = makeNode($2.nd, temp, "Global Declaration"); 
		}
	} 
	else { 
		$$.nd = makeNode($2.nd, $4.nd, "Global Declaration"); 
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $2.name, $4.name);
}
| ID { char str1[50]; strcpy(str1, $1.name); check_declaration(str1); } '=' globexpression { $1.nd = makeNode(NULL, NULL, $1.name);
	char *id_type = get_DataType($1.name); 
	if(strcmp(id_type, $4.type)) {
		if(!strcmp(id_type, "int")) {
			if(!strcmp($4.type, "float")){
				struct node *temp = makeNode(NULL, $4.nd, "floattoint");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "chartoint");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			
		}
		else if(!strcmp(id_type, "float")) {
			if(!strcmp($4.type, "int")){
				struct node *temp = makeNode(NULL, $4.nd, "inttofloat");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "chartofloat");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			
		}
		else{
			if(!strcmp($4.type, "int")){
				struct node *temp = makeNode(NULL, $4.nd, "inttochar");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "floattochar");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
		}
	}
	else {
		$$.nd = makeNode($1.nd, $4.nd, "="); 
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
}

import: import import { $$.nd = makeNode($1.nd, $2.nd, "imports"); }
| IMPORT { addTable('P'); } { $$.nd = makeNode(NULL, NULL, $1.name); }
| { $$.nd = makeNode(NULL, NULL, "NULL"); }
;

function_definition: function_specifier datatype ID { if(is_fcall || !(strcmp($3.name, "main"))) { addTable('F'); } if(!(strcmp($3.name, "main")))} '(' arg ')' '{' body return '}' 
{if(!strcmp($3.name, "main")){char str1[50]; 
strcpy(str1, strcat($3.name," function")); $3.nd = makeNode($9.nd, $10.nd, str1); $$.nd = $3.nd; } else if(is_fcall){ char str1[50]; 
strcpy(str1, strcat($3.name," function")); $3.nd = makeNode($9.nd, $10.nd, str1); $$.nd = $3.nd;}}
;

func_call: ID { is_fcall = 1;} '(' arg ')' semicolon {$$.nd = makeNode($1.nd,NULL,"Function call");}

arg: arg ',' datatype ID { addTable('V'); }
| datatype ID { addTable('V'); }
|{ $$.nd = NULL; }
;

function_specifier: 'f'
|'F'
;

datatype: INTEGER { insert_type(); }
| FLOAT { insert_type(); }
| CHAR { insert_type(); }
| VOID { insert_type(); }
| error {sprintf(errors[sem_errors], "Line %d: Datatype error!\n", countn);
		sem_errors++;}
;

body: body body_statements { $$.nd = makeNode($1.nd, $2.nd,"Body"); }
| body_statements { $$.nd = $1.nd; }

conditional_body: conditional_body conditional_body_statements { $$.nd = makeNode($1.nd, $2.nd,"Body"); }
| conditional_body_statements { $$.nd = $1.nd; }

body_statements: FOR { addTable('K'); is_loop = 1; } '(' statement semicolon condition semicolon statement ')' '{' conditional_body '}' {
	 struct node *temp = makeNode($6.nd, $8.nd, "CONDITION"); struct node *temp2 = makeNode($4.nd, temp, "CONDITION"); $$.nd = makeNode(temp2, $11.nd, $1.name); 
	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", $6.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $6.else_body);}
| WHILE { addTable('K'); is_loop = 1; } '(' condition ')' '{' conditional_body '}' { $$.nd = makeNode($4.nd, $7.nd, $1.name); 
	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", $4.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body);}
| IF { addTable('K'); is_loop = 0; } '(' condition ')' { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.if_body); } '{' body '}' 
	{ sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body); } else { struct node *iff = makeNode($4.nd, $8.nd, $1.name); 	$$.nd = makeNode(iff, $9.nd, "if-else"); 
	sprintf(icg[ic_idx++], "GO TO next\n");}
| statement semicolon { $$.nd = $1.nd; }
| func_call { $$.nd = $1.nd; }
| PRINT { addTable('K'); } '(' STR ')' semicolon { $$.nd = makeNode(NULL, NULL, "print"); sprintf(icg[ic_idx++], "%s\n", $4.name);}
| PRINT { addTable('K'); } '(' ID {char str1[50]; strcpy(str1, $4.name); check_declaration(str1);} ')' semicolon { $$.nd = makeNode($4.nd, NULL, "print");}
;

conditional_body_statements: FOR { addTable('K'); is_loop = 1;} '(' statement semicolon condition semicolon statement ')' '{' conditional_body '}' { 
	struct node *temp = makeNode($6.nd, $8.nd, "CONDITION"); struct node *temp2 = makeNode($4.nd, temp, "CONDITION"); $$.nd = makeNode(temp2, $11.nd, $1.name); 
	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", $6.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $6.else_body);}
| WHILE {addTable('K'); is_loop = 1; } '(' condition ')' '{' conditional_body '}' { $$.nd = makeNode($4.nd, $7.nd, $1.name); 
	sprintf(icg[ic_idx++], buff);
	sprintf(icg[ic_idx++], "JUMP to %s\n", $4.if_body);
	sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body);}
| IF { addTable('K'); is_loop = 0; } '(' condition ')' { sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.if_body); } '{' body '}'
	{ sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body); } else { struct node *iff = makeNode($4.nd, $7.nd, $1.name); 	$$.nd = makeNode(iff, $9.nd, "if-else"); 
	sprintf(icg[ic_idx++], "GO TO next\n");}
| statement semicolon { $$.nd = $1.nd; }
| PRINT { addTable('K'); } '(' STR ')' semicolon { $$.nd = makeNode(NULL, NULL, "print"); sprintf(icg[ic_idx++], "%s\n", $4.name);}
| PRINT { addTable('K'); } '(' ID {char str1[50]; strcpy(str1, $4.name); check_declaration(str1);} ')' semicolon { $$.nd = makeNode(NULL, NULL, "print"); }
| BREAK { addTable('K'); } semicolon  { $$.nd = makeNode(NULL, NULL, "break"); 
		sprintf(icg[ic_idx++], "BREAK LOOP\n");
	}
| CONTINUE { addTable('K'); } semicolon { $$.nd = makeNode(NULL, NULL, "continue"); 
	sprintf(icg[ic_idx++], "CONTINUE LOOP\n");}

else: ELSE { addTable('K'); } '{' body '}' { $$.nd = makeNode(NULL, $4.nd, $1.name); }
| { $$.nd = NULL; }
;

condition: value LT value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| value GT value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| value LE value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| value GE value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| value EQ value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| value NE value { $$.nd = makeNode($1.nd, $3.nd, $2.name); 
	if(is_loop) {
		sprintf($$.if_body, "L%d", label++);
		
		sprintf(icg[ic_idx++], "\nLABEL %s:\n", $$.if_body);
		sprintf(icg[ic_idx++], "\nif NOT (%s %s %s) GOTO L%d\n", $1.name, $2.name, $3.name, label);
		sprintf($$.else_body, "L%d", label++);
	} else {
		sprintf(icg[ic_idx++], "\nif (%s %s %s) GOTO L%d else GOTO L%d\n", $1.name, $2.name, $3.name, label, label+1);
		sprintf($$.if_body, "L%d", label++);
		sprintf($$.else_body, "L%d", label++);
	}}
| TRUE { addTable('K'); $$.nd = NULL; }
| FALSE { addTable('K'); $$.nd = NULL; }
;

statement: datatype ID { addTable('V'); } init { 
	$2.nd = makeNode(NULL, NULL, $2.name); 
	int t = check_types($1.name, $4.type); 
	if(t>0) { 
		if(t == 1) {
			struct node *temp = makeNode(NULL, $4.nd, "floattoint"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		} 
		else if(t == 2) { 
			struct node *temp = makeNode(NULL, $4.nd, "inttofloat"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		} 
		else if(t == 3) { 
			struct node *temp = makeNode(NULL, $4.nd, "chartoint"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		} 
		else if(t == 4) { 
			struct node *temp = makeNode(NULL, $4.nd, "inttochar"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		} 
		else if(t == 5) { 
			struct node *temp = makeNode(NULL, $4.nd, "chartofloat"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		} 
		else{
			struct node *temp = makeNode(NULL, $4.nd, "floattochar"); 
			$$.nd = makeNode($2.nd, temp, "Declaration"); 
		}
	} 
	else { 
		$$.nd = makeNode($2.nd, $4.nd, "Declaration"); 
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $2.name, $4.name);
}
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} '=' expression {
	$1.nd = makeNode(NULL, NULL, $1.name); 
	char *id_type = get_DataType($1.name); 
	if(strcmp(id_type, $4.type)) {
		if(!strcmp(id_type, "int")) {
			if(!strcmp($4.type, "float")){
				struct node *temp = makeNode(NULL, $4.nd, "floattoint");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "chartoint");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			
		}
		else if(!strcmp(id_type, "float")) {
			if(!strcmp($4.type, "int")){
				struct node *temp = makeNode(NULL, $4.nd, "inttofloat");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "chartofloat");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			
		}
		else{
			if(!strcmp($4.type, "int")){
				struct node *temp = makeNode(NULL, $4.nd, "inttochar");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
			else{
				struct node *temp = makeNode(NULL, $4.nd, "floattochar");
				$$.nd = makeNode($1.nd, temp, "="); 
			}
		}
	}
	else {
		$$.nd = makeNode($1.nd, $4.nd, "="); 
	}
	sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $4.name);
}
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} UNARY {
	 $1.nd = makeNode(NULL, NULL, $1.name); $3.nd = makeNode(NULL, NULL, $3.name); $$.nd = makeNode($1.nd, $3.nd, "ITERATOR"); 
	 if(!strcmp($3.name, "++")) {
		sprintf(buff, "t%d = %s + 1\n%s = t%d\n", temp_var, $1.name, $1.name, ++temp_var);
	}
	else {
		sprintf(buff, "t%d = %s - 1\n%s = t%d\n", temp_var, $1.name, $1.name, ++temp_var);
	}}
| UNARY ID { char str1[50]; strcpy(str1, $2.name); check_declaration(str1); $1.nd = makeNode(NULL, NULL, $1.name);
	 $2.nd = makeNode(NULL, NULL, $2.name); $$.nd = makeNode($1.nd, $2.nd, "ITERATOR"); 
	 	if(!strcmp($1.name, "++")) {
		sprintf(buff, "t%d = %s + 1\n%s = t%d\n", temp_var, $2.name, $2.name, temp_var++);
	}
	else {
		sprintf(buff, "t%d = %s - 1\n%s = t%d\n", temp_var, $2.name, $2.name, temp_var++);

	}}
| operators { $$.nd = $1.nd; }
;

operators: ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} LT expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} GT expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} LE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} GE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} EQ expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| ID {char str1[50]; strcpy(str1, $1.name); check_declaration(str1);} NE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $4.nd, $3.name); }
| expression LT expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| expression GT expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| expression LE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| expression GE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| expression EQ expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }
| expression NE expression { $1.nd = makeNode(NULL, NULL, $1.name); $$.nd = makeNode($1.nd, $3.nd, $2.name); }

init: '=' value { $$.nd = $2.nd; sprintf($$.type, $2.type); strcpy($$.name, $2.name); } 
| { sprintf($$.type, "null"); $$.nd = makeNode(NULL, NULL, "NULL"); strcpy($$.name, "NULL"); } 
;

globinit: '=' globvalue { $$.nd = $2.nd; sprintf($$.type, $2.type); strcpy($$.name, $2.name); } 
| { sprintf($$.type, "null"); $$.nd = makeNode(NULL, NULL, "NULL"); strcpy($$.name, "NULL"); } 
;

expression: expression ADD expression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
} 
| expression SUBTRACT expression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| expression MULTIPLY expression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| expression DIVIDE expression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| value { strcpy($$.name, $1.name); sprintf($$.type, $1.type); $$.nd = $1.nd; } 
;

globexpression: globexpression ADD globexpression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| globexpression SUBTRACT globexpression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| globexpression MULTIPLY globexpression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| globexpression DIVIDE globexpression { 
	if(!strcmp($1.type, $3.type)) {
		sprintf($$.type, $1.type);
		$$.nd = makeNode($1.nd, $3.nd, $2.name); 
	}
	else {
		if(!strcmp($1.type, "int") && !strcmp($3.type, "float")) {
			struct node *temp = makeNode(NULL, $1.nd, "inttofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $3.nd, "inttofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "int") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartoint");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else if(!strcmp($1.type, "char") && !strcmp($3.type, "int")) {
			struct node *temp = makeNode(NULL, $1.nd, "chartoint");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
		else if(!strcmp($1.type, "float") && !strcmp($3.type, "char")) {
			struct node *temp = makeNode(NULL, $3.nd, "chartofloat");
			sprintf($$.type, $1.type);
			$$.nd = makeNode($1.nd, temp, $2.name);
		}
		else {
			struct node *temp = makeNode(NULL, $1.nd, "chartofloat");
			sprintf($$.type, $3.type);
			$$.nd = makeNode(temp, $3.nd, $2.name);
		}
	}
	sprintf($$.name, "t%d", temp_var);
	temp_var++;
	sprintf(icg[ic_idx++], "%s = %s %s %s\n",  $$.name, $1.name, $2.name, $3.name);
}
| globvalue { strcpy($$.name, $1.name); sprintf($$.type, $1.type); $$.nd = $1.nd; }
;

value: INTEGER_NUM { addTable('I'); strcpy($$.name, $1.name); sprintf($$.type, "int");  $$.nd = makeNode(NULL, NULL, $1.name); }
| FLOAT_NUM { addTable('D'); strcpy($$.name, $1.name); sprintf($$.type, "float");  $$.nd = makeNode(NULL, NULL, $1.name); } 
| CHARACTER { addTable('C'); strcpy($$.name, $1.name); sprintf($$.type, "char");  $$.nd = makeNode(NULL, NULL, $1.name); } 
| ID { strcpy($$.name, $1.name); char *id_type = get_DataType($1.name); sprintf($$.type, id_type); check_declaration($1.name); $$.nd = makeNode(NULL, NULL, $1.name); }
;

globvalue: INTEGER_NUM { addTable('I'); strcpy($$.name, $1.name); sprintf($$.type, "int");  $$.nd = makeNode(NULL, NULL, $1.name); }
| FLOAT_NUM { addTable('D'); strcpy($$.name, $1.name); sprintf($$.type, "float");  $$.nd = makeNode(NULL, NULL, $1.name); } 
| CHARACTER { addTable('C'); strcpy($$.name, $1.name); sprintf($$.type, "char");  $$.nd = makeNode(NULL, NULL, $1.name); }
| ID { strcpy($$.name, $1.name); char *id_type = get_DataType($1.name); sprintf($$.type, id_type); check_declaration($1.name); $$.nd = makeNode(NULL, NULL, $1.name); }
;

return: RETURN{ addTable('K'); } value semicolon { check_return_type($3.name); $1.nd = makeNode(NULL, NULL, "return"); $$.nd = makeNode($1.nd, $3.nd, "RETURN"); }
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
	printf("\t\t\t\t\t\t\t\t SEMANTIC ANALYSIS \n\n");
	if(sem_errors>0) {
		printf("Semantic analysis completed with %d errors\n", sem_errors);
		for(int i=0; i<sem_errors; i++){
			printf("\t - %s", errors[i]);
		}
	} else {
		printf("Semantic analysis completed with no errors");
	}
	/*for(int i=0; i<1; i++){
		printf("%s", a[i]);
	}*/
	printf("\n\n");
	printf("\t\t\t\t\t\t\t INTERMEDIATE CODE GENERATION \n\n");
	for(int i=0; i<ic_idx; i++){
		printf("%s", icg[i]);
	}
	printf("\n\n");
}

int searchTable(char *name) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbolTable[i].id_name, name)==0) {
			if(is_reserved(symbolTable[i].id_name)) break;
			return -1;
			break;
		}
	}
	return 0;
}

void check_return_type(char *value) {
	int i;
	for(i=count-1; i>=0; i--){
		if(!strcmp(symbolTable[i].type, "Function")){
			char *main_datatype = get_DataType(symbolTable[i].id_name);
			char *return_datatype = get_DataType(value);
			if(!strcmp(main_datatype, return_datatype)){
				break;
			}
			else {
				sprintf(errors[sem_errors], "Line %d: %s function return type mismatch\n", countn, symbolTable[i].id_name);
				sem_errors++;
				break;
			}
		}	
	}
}

void check_declaration(char *name) {
    q = searchTable(name);
    if(!q) {
        sprintf(errors[sem_errors], "Line %d: Variable \"%s\" not declared before usage or the datatype is wrong\n", countn, name);
		sem_errors++;
    }
}

char *get_DataType(char *var){
	for(int i=0; i<count; i++) {
		if(!strcmp(symbolTable[i].id_name, var)) {
			return symbolTable[i].data_type;
		}
	}
}

int is_reserved(char *name){
	for(int i=0; i<10; i++) {   
        if(!strcmp(reserved[i], name)){
			return 1;
		}
	}
	return 0;
}


void addTable(char c) {
	if(c == 'V'){
		if(is_reserved(strdup(yytext))){
        	sprintf(errors[sem_errors], "Line %d: Variable name \"%s\" is a reserved keyword!\n", countn, yytext);
			sem_errors++;
			return;
		}
	}
	if(c == 'G'){
		if(is_reserved(strdup(yytext))){
        	sprintf(errors[sem_errors], "Line %d: Global Variable name \"%s\" is a reserved keyword!\n", countn, yytext);
			sem_errors++;
			return;
		}
	}
	if(c == 'F'){
		if(is_reserved(strdup(yytext))){
			char str1[4];
			strcpy(str1, "main");
			char str2[4];
			strcpy(str2, strdup(yytext));
			if(strcmp(str2, str1) && is_main == 1){
				is_main = 0;
			}
			if(is_main > 0){
				sprintf(errors[sem_errors], "Line %d: Function name \"%s\" is a reserved keyword! Only one can be declared!\n", countn, yytext);
				sem_errors++;
				return;
			}
			is_main = is_main + 2;
		}
	}
    q=searchTable(yytext);
	if(!q) {
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
		else if(c=='I') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("int");
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Constant");
			count++;
		}
		else if(c=='C') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("char");
			symbolTable[count].line_no=countn;
			symbolTable[count].type=strdup("Constant");
			count++;
		}
		else if(c=='D') {
			symbolTable[count].id_name=strdup(yytext);
			symbolTable[count].data_type=strdup("float");
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
    }else if(c == 'V' && q) {
        sprintf(errors[sem_errors], "Line %d: Multiple declarations of \"%s\" not allowed!\n", countn, yytext);
		sem_errors++;
    }else if(c == 'G' && q) {
        sprintf(errors[sem_errors], "Line %d: Multiple declarations of \"%s\" not allowed!\n", countn, yytext);
		sem_errors++;
    }else if(c == 'F' && q) {
        sprintf(errors[sem_errors], "Line %d: Multiple declarations of \"%s\" not allowed!\n", countn, yytext);
		sem_errors++;
    }
	
}

int check_types(char *type1, char *type2){
	// declaration with no init
	if(!strcmp(type2, "null"))
		return -1;
	// both datatypes are same
	if(!strcmp(type1, type2))
		return 0;
	// both datatypes are different
	if(!strcmp(type1, "int") && !strcmp(type2, "float"))
		return 1;
	if(!strcmp(type1, "float") && !strcmp(type2, "int"))
		return 2;
	if(!strcmp(type1, "int") && !strcmp(type2, "char"))
		return 3;
	if(!strcmp(type1, "char") && !strcmp(type2, "int"))
		return 4;
	if(!strcmp(type1, "float") && !strcmp(type2, "char"))
		return 5;
	if(!strcmp(type1, "char") && !strcmp(type2, "float"))
		return 6;
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


void yyerror(const char *msg) {
    fprintf(stderr, "%s in %dth line\n", msg, countn);
}