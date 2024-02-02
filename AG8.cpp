// handles file opening error without closing the program
#include <iostream>
#include <fstream>
#include <cctype>
#include <string>
#include <unordered_map>
#include <vector>

enum TokenType {
    KEYWORD,
    SPECIAL_CHARACTER,
    IDENTIFIER,
    OPERATOR,
    CONSTANT,
    LITERAL,
    PUNCTUATION_SYMBOL,
    INVALID,
    END
};

struct Token {
    TokenType type;
    std::string value;
    size_t lineNumber;

    Token() : type(INVALID), value(""), lineNumber(0) {}

    Token(TokenType t, const std::string& v = "", size_t ln = 0) : type(t), value(v), lineNumber(ln) {}
};

std::string getTokenTypeName(TokenType type) {
    switch (type) {
        case KEYWORD:
            return "Keyword";
        case SPECIAL_CHARACTER:
            return "Special Character";
        case IDENTIFIER:
            return "Identifier";
        case OPERATOR:
            return "Operator";
        case CONSTANT:
            return "Constant";
        case LITERAL:
            return "Literal";
        case PUNCTUATION_SYMBOL:
            return "Punctuation Symbol";
        case INVALID:
            return "Invalid";
        case END:
            return "End";
        default:
            return "Unknown";
    }
}

class RuntimeEnvironment {
private:
    std::unordered_map<std::string, float> variables;

public:
    void setVariable(const std::string& name, float value) {
        variables[name] = value;
    }

    float getVariable(const std::string& name) const {
        auto it = variables.find(name);
        if (it != variables.end()) {
            return it->second;
        } else {
            std::cerr << "Error: Variable '" << name << "' not found." << std::endl;
            exit(EXIT_FAILURE);
        }
    }

    bool variableExists(const std::string& name) const {
        return variables.find(name) != variables.end();
    }

    void printAllVariables() const {
        std::cout << "\nVariable Values:\n";
        for (const auto& entry : variables) {
            std::cout << entry.first << ": " << entry.second << std::endl;
        }
    }
};

class Lexer {
private:
    std::ifstream inputFile;
    size_t lineNumber;
    std::unordered_map<std::string, std::vector<Token>> symbolTable;
    bool fileOpenError;

public:
    Lexer(const std::string& filename) : inputFile(appendExtension(filename)), lineNumber(1) {
    if (!inputFile.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        fileOpenError = true;
    } else {
        fileOpenError = false;
    }
}


    Token getNextToken() {
        char currentChar;

        while (inputFile.get(currentChar)) {
            if (currentChar == '\n') {
                ++lineNumber;
                continue;
            }

            if (std::isalpha(currentChar) || currentChar == '_') {
                return parseIdentifier(currentChar);
            } else if (std::isdigit(currentChar)) {
                return parseConstant(currentChar);
            } else if (currentChar == '"' || currentChar == '\'') {
                return parseLiteral(currentChar);
            } else if (isOperator(currentChar)) {
                std::string op(1, currentChar);
                if (currentChar == '+' || currentChar == '-' || currentChar == '*' || currentChar == '/' || currentChar == '=')
                {
                    if (inputFile.peek() == '=') {
                        op += static_cast<char>(inputFile.get());
                    }
                }
                return insertIntoSymbolTable(OPERATOR, op, lineNumber);
            } else if (isPunctuation(currentChar)) {
                return insertIntoSymbolTable(PUNCTUATION_SYMBOL, std::string(1, currentChar), lineNumber);
            } else if (std::isspace(currentChar)) {
                continue;
            } else {
                return Token(INVALID, std::string(1, currentChar));
            }
        }

        return Token(END, "");
    }

    void printSymbolTable() const {
    std::cout << "\nSymbol Table:\n";
    for (const auto& entry : symbolTable) {
        std::cout << "Token Name: " << entry.first << std::endl;
        for (const Token& token : entry.second) {
            std::cout << "\tType: " << getTokenTypeName(token.type)
                      << ", Value: " << token.value << ", Line Number: " << token.lineNumber << std::endl;
        }
    }
}

        bool hasFileOpenError() const {
        return fileOpenError;
    }


private:
    Token parseIdentifier(char firstChar) {
        std::string result(1, firstChar);

        while (inputFile.get(firstChar) && (std::isalnum(firstChar) || firstChar == '_')) {
            result += firstChar;
        }

        if (inputFile) inputFile.unget();

        if (result == "if" || result == "else" || result == "while" || result == "int" || result == "float") {
            return insertIntoSymbolTable(KEYWORD, result, lineNumber);
        }

        return insertIntoSymbolTable(IDENTIFIER, result, lineNumber);
    }

    Token parseConstant(char firstDigit) {
        std::string result(1, firstDigit);

        while (inputFile.get(firstDigit) && std::isdigit(firstDigit)) {
            result += firstDigit;
        }

        if (inputFile) inputFile.unget();

        return insertIntoSymbolTable(CONSTANT, result, lineNumber);
    }

    Token parseLiteral(char quoteChar) {
        std::string result;
        char currentChar;

        while (inputFile.get(currentChar) && currentChar != quoteChar) {
            result += currentChar;
        }

        return insertIntoSymbolTable(LITERAL, result, lineNumber);
    }

    bool isOperator(char c) {
        return c == '+' || c == '-' || c == '*' || c == '/' || c == '=';
    }

    bool isPunctuation(char c) {
        return c == ',' || c == ';' || c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']' || c == '.';
    }

Token insertIntoSymbolTable(TokenType type, const std::string& value, size_t lineNum) {
    Token token(type, value, lineNum);
    symbolTable[value].push_back(token);
    return token;
}



    std::string appendExtension(const std::string& filename) {
        if (filename.find_last_of(".") == std::string::npos) {
            return filename + ".txt";
        }
        return filename;
    }
};



class RecursiveDescentParser {
private:
    Lexer& lexer;
    Token currentToken;
    RuntimeEnvironment env;
    bool hadError;

public:
    RecursiveDescentParser(Lexer& lexer) : lexer(lexer), hadError(false) {
        
        currentToken = lexer.getNextToken();
    }

    void parseProgram() {
    parseStatementList();

    if (!hadError) {
        match(END);
        std::cout << "Parsing successful!" << std::endl;
    } else {
        std::cout << "Parsing failed due to errors." << std::endl;
    }
}


    void evaluateProgram() {
        if (!hadError) {
            env.printAllVariables();
        }
    }

private:
    void parseStatementList() {
        while (currentToken.type != END && !hadError) {
            parseStatement();
            match(PUNCTUATION_SYMBOL, ";");
        }
    }

    void parseStatement() {
        if (hadError) return;  

        if (currentToken.type == IDENTIFIER) {
            parseAssignment();
        } else {
            reportError("Expected identifier for assignment", currentToken);
        }
    }

    void parseAssignment() {
        if (hadError) return;  

        std::string varName = currentToken.value;
        match(IDENTIFIER);

        if (currentToken.type == OPERATOR) {
            std::string op = currentToken.value;
            match(OPERATOR);
            float rhs = parseExpression();

            if (op == "=") {
                env.setVariable(varName, rhs);
            } else if (env.variableExists(varName)) {
                if (op == "+=") {
                    env.setVariable(varName, env.getVariable(varName) + rhs);
                } else if (op == "-=") {
                    env.setVariable(varName, env.getVariable(varName) - rhs);
                } else if (op == "*=") {
                    env.setVariable(varName, env.getVariable(varName) * rhs);
                } else if (op == "/=") {
                    if (rhs == 0) {
                        reportError("Division by zero", currentToken);
                    }
                    env.setVariable(varName, env.getVariable(varName) / rhs);
                } else {
                    reportError("Invalid assignment operator", currentToken);
                }
            } else {
                reportError("Variable not found", currentToken);
            }
        }
    }

    float parseExpression() {
        if (hadError) return 0;  

        float result = parseTerm();
        while (currentToken.type == OPERATOR && (currentToken.value == "+" || currentToken.value == "-")) {
            std::string op = currentToken.value;
            match(OPERATOR);
            float term = parseTerm();

            if (op == "+") {
                result += term;
            } else if (op == "-") {
                result -= term;
            }
        }
        return result;
    }

    float parseTerm() {
        if (hadError) return 0;  

        float result = parseFactor();
        while (currentToken.type == OPERATOR && (currentToken.value == "*" || currentToken.value == "/")) {
            std::string op = currentToken.value;
            match(OPERATOR);
            float factor = parseFactor();

            if (op == "*") {
                result *= factor;
            } else if (op == "/") {
                if (factor == 0) {
                    reportError("Division by zero", currentToken);
                }
                result /= factor;
            }
        }
        return result;
    }

    float parseFactor() {
        if (hadError) return 0;  

        if (currentToken.type == IDENTIFIER) {
            std::string varName = currentToken.value;
            match(IDENTIFIER);

            if (env.variableExists(varName)) {
                return env.getVariable(varName);
            } else {
                reportError("Variable not found", currentToken);
            }
        } else if (currentToken.type == CONSTANT) {
            float constantValue = std::stof(currentToken.value);
            match(CONSTANT);
            return constantValue;
        } else if (currentToken.type == SPECIAL_CHARACTER && currentToken.value == "(") {
            match(SPECIAL_CHARACTER);
            float expressionValue = parseExpression();
            match(SPECIAL_CHARACTER, ")");
            return expressionValue;
        } else {
            reportError("Expected identifier, constant, or '(' for factor", currentToken);
        }
        return 0;  
    }

    void match(TokenType expectedType, const std::string& expectedValue = "") {
        if (hadError) return;  

        if (currentToken.type == expectedType && (expectedValue.empty() || currentToken.value == expectedValue)) {
            std::cout << "Matched: " << getTokenTypeName(expectedType) << ", Value: " << currentToken.value << std::endl;
            currentToken = lexer.getNextToken();
        } else {
            reportError("Unexpected token", currentToken);
        }
    }

    void reportError(const std::string& message, const Token& token) {
        std::cerr << "Error: " << message << ". Found " << getTokenTypeName(token.type)
                  << " '" << token.value << "' at line " << token.lineNumber << std::endl;
        hadError = true;
        
    }
};




int main() {
    int choice;

    do {
        std::cout << "\n\n\t\t******WELCOME TO GROUP 3 PRINCIPLES OF COMPILER DESIGN PROJECT******\n";
        std::cout << "\tMENU\n";
        std::cout << "1. COMPILE A PROGRAM FROM TEXT FILE\n";
        std::cout << "2. CLEAR SCREEN\n";
        std::cout << "3. EXIT PROGRAM\n";
        std::cin >> choice;

        switch (choice) {
            case 1: {
                std::string filename;
                std::cout << "Enter the filename (without extension): ";
                std::cin >> filename;

                Lexer lexer(filename);

                if (lexer.hasFileOpenError()) {
                    std::cout << "File opening failed. Please check the filename and try again." << std::endl;
                    break;
                }

                RecursiveDescentParser parser(lexer);

                parser.parseProgram();
                lexer.printSymbolTable();
                parser.evaluateProgram();
                break;
            }
            case 2:
                std::system("cls");
                break;
            case 3:
                std::cout << "Exiting the program.\n";
                break;
            default:
                std::cout << "Invalid choice. Please try again.\n";
        }

    } while (choice != 3);

    return 0;
}
