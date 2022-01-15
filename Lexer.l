%{
    #include "parser.tab.h"
    int countn=1;
%}
%option yylineno

letter [a-zA-Z]
digit [0-9]
unary "++"|"--"

%%

"print"                     { strcpy(yylval.nd_obj.name,(yytext)); return PRINT; }
"input"                     { strcpy(yylval.nd_obj.name,(yytext)); return INPUT; }
"int"                       { strcpy(yylval.nd_obj.name,(yytext)); return INTEGER; }
"main"                      { strcpy(yylval.nd_obj.name,(yytext)); return MAIN; }
"float"                     { strcpy(yylval.nd_obj.name,(yytext)); return FLOAT; }
"char"                      { strcpy(yylval.nd_obj.name,(yytext)); return CHAR; }
"void"                      { strcpy(yylval.nd_obj.name,(yytext)); return VOID; }
"return"                    { strcpy(yylval.nd_obj.name,(yytext)); return RETURN; }
"for"	                    { strcpy(yylval.nd_obj.name,(yytext)); return FOR; }
"while"	                    { strcpy(yylval.nd_obj.name,(yytext)); return WHILE; }
"break"                     { strcpy(yylval.nd_obj.name,(yytext)); return BREAK; }
"continue"                  { strcpy(yylval.nd_obj.name,(yytext)); return CONTINUE; }
"if"	                    { strcpy(yylval.nd_obj.name,(yytext)); return IF; }
"else"	                    { strcpy(yylval.nd_obj.name,(yytext)); return ELSE; }
^"import"[ ]*.+             { strcpy(yylval.nd_obj.name,(yytext)); return IMPORT; }
"true"                      { strcpy(yylval.nd_obj.name,(yytext)); return TRUE; }
"false"                     { strcpy(yylval.nd_obj.name,(yytext)); return FALSE; }
[-]?{digit}+                { strcpy(yylval.nd_obj.name,(yytext)); return INTEGER_NUM; }
[-]?{digit}+\.{digit}*      { strcpy(yylval.nd_obj.name,(yytext)); return FLOAT_NUM; }
{letter}({letter}|{digit})* { strcpy(yylval.nd_obj.name,(yytext)); return ID; }
{unary}                     { strcpy(yylval.nd_obj.name,(yytext)); return UNARY; }
"<="                        { strcpy(yylval.nd_obj.name,(yytext)); return LE; }
">="                        { strcpy(yylval.nd_obj.name,(yytext)); return GE; }
"=="                        { strcpy(yylval.nd_obj.name,(yytext)); return EQ; }
"!="                        { strcpy(yylval.nd_obj.name,(yytext)); return NE; }
">"	                        { strcpy(yylval.nd_obj.name,(yytext)); return GT; }
"<"	                        { strcpy(yylval.nd_obj.name,(yytext)); return LT; }
"&&"	                    { strcpy(yylval.nd_obj.name,(yytext)); return AND; }
"||"	                    { strcpy(yylval.nd_obj.name,(yytext)); return OR; }
"+"                         { strcpy(yylval.nd_obj.name,(yytext)); return ADD; }
"-"                         { strcpy(yylval.nd_obj.name,(yytext)); return SUBTRACT; }
"/"                         { strcpy(yylval.nd_obj.name,(yytext)); return DIVIDE; }
"*"                         { strcpy(yylval.nd_obj.name,(yytext)); return MULTIPLY; }
"("			                { strcpy(yylval.nd_obj.name,(yytext)); return ('('); }
")"			                { strcpy(yylval.nd_obj.name,(yytext)); return (')'); }
"{"			                { strcpy(yylval.nd_obj.name,(yytext)); return ('{'); }
"}"			                { strcpy(yylval.nd_obj.name,(yytext)); return ('}'); }
\/\/.*                      { ; }
\/\*(.*\n)*.*\*\/           { ; } 
[ \t]*                      { ; }
[\n]                        { countn++; }
.	                        { return *yytext; }
["].*["]                    { strcpy(yylval.nd_obj.name,(yytext)); return STR; }
['].[']                     { strcpy(yylval.nd_obj.name,(yytext)); return CHARACTER; }

%%

int yywrap() {
    return 1;
}