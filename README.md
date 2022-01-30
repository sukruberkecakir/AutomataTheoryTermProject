# Automata Theory and Formal Languages Term Project

# This project is coded with yacc and lex tools.

The created programming language is able to acknowledges global variables, local variables, functions, for and while loops,
if-else conditional, conditinal operators, basic mathematical operators, integer, float, char and return

#Running The Project

You need to install bison and flex to your pc

You need to install gcc to your pc

--After installations completed install project from github and open terminal

--Sequentially write codes to terminal

bison -d -v parser.y

flex lexer.y

gcc -w parser.tab.c

a.exe<input1.test(You may change input1.test to input2.test and input3.test to see what the program do with different inputs)
