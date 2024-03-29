# Principles of Compiler Design Project

## Introduction

This project is a simple compiler designed to parse and evaluate programs written in a custom programming language. The compiler consists of a lexer, a recursive descent parser, and a runtime environment. The program allows users to compile a program from a text file, clear the screen, or exit the program.

Our project, named 'Arithmetic Genius (AG),' was chosen by the group due to its capability of performing arithmetic operations. It is important to note that our project deals exclusively with mathematical expressions and does not encompass other functions of a programming language.

## Components

### 1. Lexer

The lexer is responsible for scanning the source code and generating tokens. It recognizes various token types, including keywords, identifiers, operators, constants, literals, and punctuation symbols. The lexer also maintains a symbol table to keep track of identified tokens.

### 2. Recursive Descent Parser

The recursive descent parser performs syntax analysis on the tokens generated by the lexer. It follows a top-down parsing approach and implements production rules to recognize and parse statements, assignments, expressions, and other language constructs. The parser also evaluates the parsed program and prints the resulting symbol table.

### 3. Runtime Environment

The runtime environment simulates the execution of the parsed program. It includes a simple variable storage mechanism and supports basic arithmetic operations. The environment prints variable values at the end of the program execution.

## How to Use

1. **Compile a Program:**
   - Choose option 1 from the menu.
   - Enter the filename (without extension) of the program text file when prompted.
   - The lexer will attempt to open the file, generate tokens, and pass them to the parser.
   - The parser will perform syntax analysis and evaluation, displaying the results and any encountered errors.
   - If no errors were found it will print the matched symbols and symbol table then redirect the user to another menu, the symbol table lookup menu. This menu allows printing the symbol table as many times as the user wishes, lookup for tokens inside the symbol table and clear the console screen. The last option from this menu is "return to main menu" which takes the user back to the main menu .

2. **Clear Screen:**
   - Choose option 2 from the menu to clear the console screen.

3. **Exit Program:**
   - Choose option 3 from the menu to exit the program.

## Production Rules

### Lexer

1. **Program:**
Program → StatementList END

2. **StatementList:**
StatementList → StatementList Statement ; | ε

3. **Statement:**
Statement → Assignment

4. **Assignment:**
Assignment → IDENTIFIER (OPERATOR | COMPOUND_OPERATOR) Expression

5. **Expression:**
Expression → Term { ( + | - | COMPOUND_OPERATOR) Term }

6. **Term:**
Term → Factor { ( * | / ) Factor }

7. **Factor:**
Factor → IDENTIFIER | CONSTANT | ( Expression )

### Recursive Descent Parser

1. **ParseProgram:**
ParseProgram → parseStatementList() ; match(END)

2. **parseStatementList:**
parseStatementList → parseStatement() { ; parseStatement() }

3. **parseStatement:**
parseStatement → parseAssignment()

4. **parseAssignment:**
parseAssignment → match(IDENTIFIER) (match(OPERATOR) | match(COMPOUND_OPERATOR)) parseExpression()

5. **parseExpression:**
parseExpression → parseTerm() { ( + | - | COMPOUND_OPERATOR) parseTerm() }

6. **parseTerm:**
parseTerm → parseFactor() { ( * | / ) parseFactor() }

7. **parseFactor:**
parseFactor → match(IDENTIFIER) | match(CONSTANT) | ( parseExpression() )


These rules outline the recursive descent parsing process for the language recognized by the provided code. Each non-terminal in the rules corresponds to a function or subroutine in the parser code that handles the parsing of that specific construct. The `match()` function is used to match a token according to its type and value.

