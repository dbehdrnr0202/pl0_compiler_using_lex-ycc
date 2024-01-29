# PL/0 Compiler using Lex and Yacc
This project implements a compiler for the PL/0 programming language using Lex and Yacc. PL/0 is a simple programming language designed for educational purposes, and this compiler translates PL/0 source code into machine code.

## Getting Started
### Prerequisites
Make sure you have Lex and Yacc installed on your system. You can install them using the following commands:

```
# For Ubuntu/Debian
sudo apt-get install flex bison
```
### Building the Compiler
To build the PL/0 compiler, run the following commands:
```
make
```
This will generate the executable pl0c.

## Examples
Check the examples directory for sample PL/0 programs and their corresponding assembly code generated by the compiler.

## Structure
s_pl0Ast.l: Lex specification file for tokenizing PL/0 source code.
s_pl0Ast.y: Yacc specification file for parsing PL/0 source code and generating assembly code.
s_interpreter.c: Code generation functions.

## Acknowledgments
The PL/0 language is described in "Algorithms + Data Structures = Programs" by Niklaus Wirth.
Lex and Yacc tutorials and documentation.
Feel free to contribute and improve this compiler! If you have any questions or issues, please open an issue.