from enum import Enum
from dataclasses import dataclass
from traceback import print_stack

from IPython.core.magic_arguments import argument

code = """
object ScalaExample {
  def main(args: Array[String]): Unit = {
    val name: String = "Alice"
    var age: Int = 25

    println(s"Hello, $name! You are $age years old.")
    }
}
"""

PRINT_AST=False # print the ASTs after parsing?
PRINT_ENV=False # print the environment stack during runtime?

TokenType = Enum('TokenType',
                 [   # Tokens that are unambiguous single characters - WILDCARD means underscore.
                     'SEMICOLON', 'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE', 'LEFT_BRACKET', 'RIGHT_BRACKET',
                     'PLUS', 'MINUS', 'STAR', 'SLASH', 'COMMA', 'COLON', 'WILDCARD', 'DOT',
                     # Tokens that require 1-character lookahead
                     'EQUAL', 'EQUAL_EQUAL',
                     'BANG', 'BANG_EQUAL',
                     'LESS', 'LESS_EQUAL',
                     'GREATER', 'GREATER_EQUAL',
                     'ARROW',
                     # keywords  - var (mutable VAR) / val (immutable VAR)
                     'DEF', 'IF', 'ELSE', 'WHILE', 'VAR', 'PRINT', 'TRUE', 'FALSE', 'RETURN',
                     'OBJECT', 'VAL', 'PRINTLN', 'CASE', 'MATCH', 'IMPORT', 'NEW',
                     'EXTENDS', 'WITH', 'CLASS', 'TRAIT', 'FOREACH',
                     #TYPE NAME - unit means void in C ('STRING', 'INT', 'FLOAT', 'DOUBLE', 'BOOLEAN', 'UNIT', 'ARRAY')
                     'TYPE_NAME',
                     # identifiers and literals
                     'IDENTIFIER', 'STRING', 'NUMBER',
                     # interpolator
                     'S_INTERPOLATOR', 'F_INTERPOLATOR','RAW_INTERPOLATOR',
                     # annotation
                     'ANNOTATION',
                 ])
    
@dataclass
class Token:
    tokenType: TokenType
    lexeme: str
    line_num: int

tokens = []

for line_num, line in enumerate(code.splitlines()):
    current = 0   # current index we're looking at
    start = 0     # start of the current token
    while current < len(line):
        def isEndOfLine():
            return current+1 >= len(line)
        def isPastEndOfLine():
            return current+1 > len(line)
        def peek(c):
            # is the character after next_char equal to c?
            if isEndOfLine(): # if we're at the end of the line, always false
                return False
            return line[current+1] == c

        # the next character to look at is line[current]
        next_char = line[current]
        # First look for the unambiguous 1-character tokens
        #    'SEMICOLON', 'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE',
        #    'PLUS', 'MINUS', 'STAR', 'SLASH',
        if next_char == ';':
            token = Token(TokenType.SEMICOLON, ';', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '_':
            token = Token(TokenType.WILDCARD, '_', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '(':
            token = Token(TokenType.LEFT_PAREN, '(', line_num)
            tokens.append(token)
            current += 1
        elif next_char == ')':
            token = Token(TokenType.RIGHT_PAREN, ')', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '{':
            token = Token(TokenType.LEFT_BRACE, '{', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '}':
            token = Token(TokenType.RIGHT_BRACE, '}', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '[':
            token = Token(TokenType.LEFT_BRACKET, '[', line_num)
            tokens.append(token)
            current += 1
        elif next_char == ']':
            token = Token(TokenType.RIGHT_BRACKET, ']', line_num)
            tokens.append(token)
            current += 1
        elif next_char == ':':
            token = Token(TokenType.COLON, ':', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '+':
            token = Token(TokenType.PLUS, '+', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '-':
            token = Token(TokenType.MINUS, '-', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '*':
            token = Token(TokenType.STAR, '*', line_num)
            tokens.append(token)
            current += 1
        elif next_char == '/':
            token = Token(TokenType.SLASH, '/', line_num)
            tokens.append(token)
            current += 1
        elif next_char == ',':
            token = Token(TokenType.COMMA, ',', line_num)
            tokens.append(token)
            current += 1
        elif next_char == ' ': # skip whitespace
            current += 1
        # Then look for tokens that require 1-character lookahead:
        #    'EQUAL', 'EQUAL_EQUAL',
        #    'BANG', 'BANG_EQUAL',
        #    'LESS', 'LESS_EQUAL',
        #    'GREATER', 'GREATER_EQUAL',
        elif next_char == '=':
            if peek('='):
                token = Token(TokenType.EQUAL_EQUAL, '==', line_num)
                tokens.append(token)
                current += 2
            elif peek('>'):
                token = Token(TokenType.ARROW, '=>', line_num)
                tokens.append(token)
                current += 2
            else:
                token = Token(TokenType.EQUAL, '=', line_num)
                tokens.append(token)
                current += 1
        elif next_char == '!':
            if peek('='):
                token = Token(TokenType.BANG_EQUAL, '!=', line_num)
                tokens.append(token)
                current += 2
            else:
                token = Token(TokenType.BANG, '!', line_num)
                tokens.append(token)
                current += 1
        elif next_char == '<':
            if peek('='):
                token = Token(TokenType.LESS_EQUAL, '<=', line_num)
                tokens.append(token)
                current += 2
            else:
                token = Token(TokenType.LESS, '<', line_num)
                tokens.append(token)
                current += 1
        elif next_char == '>':
            if peek('='):
                token = Token(TokenType.GREATER_EQUAL, '>=', line_num)
                tokens.append(token)
                current += 2
            else:
                token = Token(TokenType.GREATER, '>', line_num)
                tokens.append(token)
                current += 1
        # Look for string literals
        elif next_char == '"':
            start = current # keep track of start of string literal
            current += 1
            closed_string = False
            while not isPastEndOfLine() and not closed_string:
                if line[current] == '"':
                    closed_string = True
                    current += 1
                    token = Token(TokenType.STRING, line[start:current], line_num)
                    tokens.append(token)
                else:
                    current += 1
            if not closed_string:
                raise Exception(f"Unclosed string literal on line {line_num}")
        elif next_char == 's' and peek('"'):
            start = current
            current += 2
            closed_string = False
            while not isPastEndOfLine() and not closed_string:
                if line[current] == '"':
                    closed_string = True
                    current += 1
                    token = Token(TokenType.S_INTERPOLATOR, line[start:current], line_num)
                    tokens.append(token)
                else:
                    current += 1
            if not closed_string:
                raise Exception(f"Unclosed string literal on line {line_num}")
        elif next_char == 'f' and peek('"'):
            start = current
            current += 2
            closed_string = False
            while not isPastEndOfLine() and not closed_string:
                if line[current] == '"':
                    closed_string = True
                    current += 1
                    token = Token(TokenType.S_INTERPOLATOR, line[start:current], line_num)
                    tokens.append(token)
                else:
                    current += 1
            if not closed_string:
                raise Exception(f"Unclosed string literal on line {line_num}")
        elif next_char == 'r':
            if line[current+1] =='a' and line[current+2] == 'w' and line[current+3] == '"':
                start = current
                current += 4
                closed_string = False
                while not isPastEndOfLine() and not closed_string:
                    if line[current] == '"':
                        closed_string = True
                        current += 1
                        token = Token(TokenType.RAW_INTERPOLATOR, line[start:current], line_num)
                        tokens.append(token)
                    else:
                        current += 1
                if not closed_string:
                    raise Exception(f"Unclosed string literal on line {line_num}")
        # recall method
        elif next_char == '.':
            current += 1
            token = Token(TokenType.DOT, ".",line_num)
            tokens.append(token)
        # ANNOTATION
        elif next_char == '@':
            start = current
            current += 1
            closed_annotation = False
            while not isPastEndOfLine() and not closed_annotation:
                if line[current] == ' ':
                    closed_annotation = True
                    current += 1
                    token = Token(TokenType.ANNOTATION, line[start:current], line_num)
                    tokens.append(token)
                else:
                    current += 1
            if not closed_annotation:
                raise Exception(f"Unclosed annotation on line {line_num}")
        # Look for numeric literals
        elif next_char.isdigit():
            start = current
            current += 1
            has_decimal = False
            while not isPastEndOfLine() and\
                    (line[current].isdigit() or (line[current] == '.' and not has_decimal)):
                if line[current] == '.':
                    has_decimal = True    # only one decimal point allowed in a number
                current += 1
            token = Token(TokenType.NUMBER, line[start:current], line_num)
            tokens.append(token)
        elif next_char.isalpha():
            start = current
            current += 1
            while not isPastEndOfLine() and line[current].isalnum():
                current += 1
            # first check if it's a keyword: 'IF', 'ELSE', 'VAR', 'PRINT'
            lexeme = line[start:current]
            if lexeme == "def":
                token = Token(TokenType.DEF, lexeme, line_num)
            elif lexeme == "if":
                token = Token(TokenType.IF, lexeme, line_num)
            elif lexeme == "else":
                token = Token(TokenType.ELSE, lexeme, line_num)
            elif lexeme == "while":
                token = Token(TokenType.WHILE, lexeme, line_num)
            elif lexeme == "var":
                token = Token(TokenType.VAR, lexeme, line_num)
            elif lexeme == "print":
                token = Token(TokenType.PRINT, lexeme, line_num)
            elif lexeme == "val":
                token = Token(TokenType.VAL, lexeme, line_num)
            elif lexeme == "println":
                token = Token(TokenType.PRINTLN,lexeme, line_num)
            elif lexeme == "true":
                token = Token(TokenType.TRUE, lexeme, line_num)
            elif lexeme == "false":
                token = Token(TokenType.FALSE, lexeme, line_num)
            elif lexeme == "return":
                token = Token(TokenType.RETURN, lexeme, line_num)
            elif lexeme == "object":
                token = Token(TokenType.OBJECT, lexeme, line_num)
            elif lexeme == "case":
                token = Token(TokenType.CASE, lexeme, line_num)
            elif lexeme == "match":
                token = Token(TokenType.MATCH, lexeme, line_num)
            elif lexeme == "new":
                token = Token(TokenType.NEW, lexeme, line_num)
            elif lexeme == "import":
                token = Token(TokenType.IMPORT, lexeme, line_num)
            elif lexeme == "extends":
                token = Token(TokenType.EXTENDS,lexeme, line_num)
            elif lexeme == "with":
                token = Token(TokenType.WITH,lexeme, line_num)
            elif lexeme == "class":
                token = Token(TokenType.CLASS,lexeme, line_num)
            elif lexeme == "trait":
                token = Token(TokenType.TRAIT, lexeme, line_num)
            elif lexeme == "foreach":
                token = Token(TokenType.FOREACH, lexeme, line_num)
            ##Type name
            elif lexeme == "String" or lexeme == "Short" or lexeme == "Int" or \
                    lexeme == "Long" or lexeme == "Float" or lexeme == "Double" or \
                    lexeme == "Char" or lexeme == "Boolean" or lexeme == "Unit" or \
                    lexeme == "Array" or lexeme == "List" or lexeme == "Any" or \
                    lexeme == "T" or lexeme == "LIST":
                token = Token(TokenType.TYPE_NAME, lexeme, line_num)
            else: # otherwise it's an identifier
                token = Token(TokenType.IDENTIFIER, lexeme, line_num)
            tokens.append(token)
        else:
            raise Exception(f"Character {next_char} on line {line_num} not a valid start of token.")
    
print(tokens)

# Parser
# Input: tokens (list of tokens, each of class Token)
# Output: list of ASTs (each of one of the dataclasses below)
# ======

@dataclass
class decimalNumeral:
    name: Token     # IDENTIFIER token
    arguments: list # list of IDENTIFIER
    body: object    # body of the function (block)

@dataclass
class defDeclStmt:
    name: Token     # IDENTIFIER token
    genericType : Token # Generic Type (optional)
    arguments: list # variable_name, type_name of IDENTIFIER
    returnType : Token # returnType
    body: object    # body of the function (block)

@dataclass
class varDeclStmt:
    name: Token     # IDENTIFIER token
    varType : Token # variable type token
    initVal: object # expression or None

@dataclass
class valDeclStmt:  # val means immutable VAR
    name: Token     # IDENTIFIER token
    valType: Token  # variable type token
    initVal: object # expression or None

@dataclass
class assignStmt:
    name: Token     # IDENTIFIER token
    value: object   # expression

@dataclass
class block:
    statements: list #list of statements

@dataclass
class annoStmt:
    name: Token
    body: object  #body of the annotation

@dataclass
class traitStmt:
    name: Token
    body: object # body of the function (block)

@dataclass
class classStmt:
    name: Token
    traits: list
    body: object

@dataclass
class objectStmt:
    name: Token     # IDENTIFIER token
    body: object    # body of the object

@dataclass
class printStmt:
    expr: object

@dataclass
class printlnStmt:
    expr: object

@dataclass
class literalExpr:
    value: object   # float, string, or bool

@dataclass
class varRefExpr:
    name: str

@dataclass
class valRefExpr:
    name: str

@dataclass
class defCallExpr:
    function: object # expression that evaluates to a Callable
    arguments: list # list of expressions that will be the arguments

@dataclass
class binaryExpr:
    operator: Token
    lhs: object        # expression
    rhs: object        # expression

@dataclass
class unaryExpr:
    operator: Token
    rhs: object        # expression

@dataclass
class stringInterpolationExpr:
    interpolator: str
    parts: list

@dataclass
class memberAccessExpr:
    receiver : object # The object being accessed (e.g., 'dog')
    member : token    # The member being accessed (e.g., 'sound')

@dataclass
class foreachExpr:
    receiver : object #the object of collection on which a method is being called.
    operator : object #the action is performed on each element of the collection.

@dataclass
class matchCase:
    pattern: object # The condition you're matching against (e.g., a value, type, structure).
    result: object # What happens if the pattern matches (could be a value, expression, or action).

@dataclass
class matchExpr:
    expr: object
    cases: list

@dataclass
class wildcardPattern:
    value : object

@dataclass
class importStmt:
    path: str

@dataclass
class className:
    name: str

@dataclass
class traitDeclStmt:
    name: str
    body: object

@dataclass
class classDeclStmt:
    name: str
    params: list
    superClass : token
    traits: list
    body: object

@dataclass
class defCallExpr:
    name: str
    arguments: list

@dataclass
class curriedDefExpr:
    name : str
    arguments: list
    isCurried: bool

@dataclass
class annotationExpr:
    annot_name : str
    body : object

currentToken = 0

# some convenience functions for looking at and/or consuming the next token
def nextToken():
    "The next token to be parsed, or None if there are no tokens left"
    if currentToken >= len(tokens):
        return None
    return tokens[currentToken]
def nextNextToken():
    "Look ahead one token into the future, past the next one"
    if currentToken+1 >= len(tokens):
        return None
    return tokens[currentToken+1]
def nextTokenType():
    "The *type* of the next token to be parsed, or None if there are no tokens left"
    if currentToken >= len(tokens):
        return None
    return nextToken().tokenType
def nextNextTokenType():
    if currentToken+1 >= len(tokens):
        return None
    return nextNextToken().tokenType
def consume():
    "Consume the next token (i.e. we've finished parsing it)"
    global currentToken
    currentToken += 1
def expect(expectedType):
    "Consume the next token if it's of the expected type, otherwise error. Used when we 100% know what the next token should be."
    if nextTokenType() != expectedType:
        raise SyntaxError(f"Expected {expectedType} but found {nextTokenType()} ({nextToken().lexeme}) at line {nextToken().line_num}")
    consume()

def parseProgram():
    # grammar:
    #   program: statement+
    statements = []
    while nextToken() is not None:
        statement = parseStatement()
        statements.append(statement)
    return statements

def parseStatement():
    # grammar:
    #   statement: varDeclStmt | assignStmt | ifStmt | whileStmt | printStmt | block
    if nextTokenType() == TokenType.DEF:
        return parseDefDeclStmt()
    elif nextTokenType() == TokenType.PRINT:
        return parsePrintStmt()
    elif nextTokenType() == TokenType.PRINTLN:
        return parsePrintlnStmt()
    elif nextTokenType() == TokenType.IDENTIFIER and nextNextTokenType() == TokenType.EQUAL:
        return parseAssignStmt()
    elif nextTokenType() == TokenType.IDENTIFIER and nextNextTokenType() == TokenType.LEFT_PAREN:
        return parseDefCall()
    elif nextTokenType() == TokenType.LEFT_BRACE:
        return parseBlock()
    elif nextTokenType() == TokenType.OBJECT:
        return parseObjectStmt()
    elif nextTokenType() == TokenType.VAR:
        return parseVarDeclStmt()
    elif nextTokenType() == TokenType.VAL:
        return parseValDeclStmt()
    elif nextTokenType() == TokenType.CASE:
        return parseCaseStmt()
    elif nextTokenType() == TokenType.IMPORT:
        return parseImportStmt()
    elif nextTokenType() == TokenType.TRAIT:
        return parseTraitStmt()
    elif nextTokenType() == TokenType.CLASS:
        return parseClassStmt()
    elif nextTokenType() == TokenType.ANNOTATION:
        return parseAnnotationStmt()
    elif nextTokenType() == None:
        raise Exception("Unexpected end of file")
    return parseExpressionStmt()

def parseAnnotationStmt():
    # grammar
    ano_name = nextToken().lexeme
    expect(TokenType.ANNOTATION)
    body = parseStatement()
    return annotationExpr(ano_name, body)

def parseAssignStmt():
    # grammar
    # assign structure identifier equal value
    varName = nextToken()
    expect(TokenType.IDENTIFIER)
    expect(TokenType.EQUAL)
    expr = parseExpression()
    return assignStmt(varName, expr)

def parseClassStmt():
    # grammar
    # class structure: class name extends trait_name with trait_name {...}
    # variable
    params = []
    supperClass = None
    traits = []
    body = None

    expect(TokenType.CLASS)
    name = nextToken().lexeme
    expect(TokenType.IDENTIFIER)
    if nextTokenType() == TokenType.EXTENDS:
        expect(TokenType.EXTENDS)
        supperClass = nextToken()
        expect(TokenType.IDENTIFIER)
    while nextTokenType() == TokenType.WITH:
        expect(TokenType.WITH)
        trait = nextToken()
        expect(TokenType.IDENTIFIER)
        traits.append(trait)
    body = parseBlock()
    return classDeclStmt(name, params, supperClass, traits, body)

def parseTraitStmt():
    # grammar
    # trait structure : trait name {...}
    expect(TokenType.TRAIT)
    name = nextToken().lexeme
    expect(TokenType.IDENTIFIER)
    body = parseBlock()
    return traitDeclStmt(name, body)

def parseImportStmt():
    # grammar
    # import path
    expect(TokenType.IMPORT)
    path = ""
    path = nextToken().lexeme
    expect(TokenType.IDENTIFIER)
    path += nextToken().lexeme
    expect(TokenType.DOT)
    path += nextToken().lexeme
    expect(TokenType.IDENTIFIER)
    path += nextToken().lexeme
    expect(TokenType.DOT)
    path += nextToken().lexeme
    expect(TokenType.IDENTIFIER)

    return importStmt(path)

def parseCaseStmt():
    # grammar
    # case: case number => value
    expr = valRefExpr(tokens[currentToken-3].lexeme)
    cases = []
    while nextTokenType() == TokenType.CASE:
        consume()
        pattern = None
        if nextTokenType() == TokenType.NUMBER:
            pattern = literalExpr(nextToken().lexeme)
            expect(TokenType.NUMBER)
        else:
            pattern = wildcardPattern(nextToken().lexeme)
            expect(TokenType.WILDCARD)
        expect(TokenType.ARROW)
        result = literalExpr(nextToken().lexeme)
        consume()
        case = matchCase(pattern, result)
        cases.append(case)
    return matchExpr(expr, cases)

def parseValDeclStmt():
    # grammar
    # valDecl : val name : type_name equal value
    expect(TokenType.VAL)
    name = nextToken()
    expect(TokenType.IDENTIFIER)
    type_name = None
    if nextTokenType() == TokenType.COLON:
        expect(TokenType.COLON)
        type_name = nextToken()
        expect(TokenType.TYPE_NAME)
    expr = None
    if nextTokenType() == TokenType.EQUAL:
        expect(TokenType.EQUAL)
        expr = parseExpression()
    return valDeclStmt(name, type_name, expr)

def parseVarDeclStmt():
    # grammar
    # varDecl : var name : type_name equal value
    expect(TokenType.VAR)
    name = nextToken()
    expect(TokenType.IDENTIFIER)
    type_name = None
    if nextTokenType() == TokenType.COLON:
        expect(TokenType.COLON)
        type_name = nextToken()
        expect(TokenType.TYPE_NAME)
    expr = None
    if nextTokenType() == TokenType.EQUAL:
        expect(TokenType.EQUAL)
        expr = parseExpression()
    return varDeclStmt(name, type_name, expr)

def parseObjectStmt():
    # grammar
    # objectStmt: OBJECT expression
    expect(TokenType.OBJECT)
    objName = nextToken()
    expect(TokenType.IDENTIFIER)
    body = parseBlock()
    return objectStmt(objName, body)

def parseExpressionStmt():
    expr = parseExpression()
    return expr

def parsePrintStmt():
    # grammar
    # printStmt: PRINT expression
    expect(TokenType.PRINT)
    expr = parseExpression()
    return printStmt(expr)

def parsePrintlnStmt():
    # grammar
    # printlnStmt: println expression
    expect(TokenType.PRINTLN)
    expr = parseExpression()
    return printlnStmt(expr)

def parseExpression():
    # grammar:
    # expression: equality
    return parseEquality()

def parseEquality():
    # grammar:
    #    equality: comparison ( ( BANG_EQUAL | EQUAL_EQUAL ) comparison )*
    expr = parseComparison()
    while nextTokenType() in (TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
        operator = nextToken()
        consume()
        rhs = parseComparison()
        expr = binaryExpr(operator, expr, rhs)
    return expr

def parseTerm():
    # grammar:
    #    term: factor ( ( MINUS | PLUS ) factor )*
    expr = parseFactor()
    while nextTokenType() in (TokenType.MINUS, TokenType.PLUS):
        operator = nextToken()
        consume()
        rhs = parseFactor()
        expr = binaryExpr(operator, expr, rhs)
    return expr

def parseFactor():
    # grammar:
    #    factor: unary ( ( SLASH | STAR ) unary )*
    #           |funCall
    expr = parseUnary()
    while nextTokenType() in (TokenType.SLASH, TokenType.STAR):
        operator = nextToken()
        consume()
        rhs = parseUnary()
        expr = binaryExpr(operator, expr, rhs)
    return expr

def parseUnary():
    # grammar:
    #    unary: ( BANG | MINUS ) unary
    #           | primary
    if nextTokenType() in (TokenType.BANG, TokenType.MINUS):
        operator = nextToken()
        consume()
        rhs = parseUnary()
        return unaryExpr(operator, rhs)
    return parseDefCall()

def parseDefCall():
    # grammar
    # defCall: primary (LEFT_PAREN arguments? RIGHT_PAREN)?
    # arguments: expression (COMMA expression)*
    expr = parsePrimary()
    if nextTokenType() == TokenType.LEFT_PAREN:
        expect(TokenType.LEFT_PAREN)
        arguments = []
        while nextTokenType() != TokenType.RIGHT_PAREN:
            if len(arguments) > 0:
                expect(TokenType.COMMA)
            nextArgument = parseExpression()
            arguments.append(nextArgument)
        expect(TokenType.RIGHT_PAREN)
        expr = defCallExpr(expr,arguments)
    return expr

def parseComparison():
    # grammar:
    #    comparison: term ( ( GREATER | GREATER_EQUAL | LESS | LESS_EQUAL ) term )*
    expr = parseTerm()
    while nextTokenType() in (TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL):
        operator = nextToken()
        consume()
        rhs = parseTerm()
        expr = binaryExpr(operator, expr, rhs)
    return expr

def parseDefDeclStmt():
    # grammar
    # defDecl: DEF IDENTIFIER (LEFT_BRACKET TYPE_NAME RIGHT_BRACKET)-optional
    # LEFT_PAREN (PARAMS PARAMTYPE) RIGHT_PAREN COLON RETURNTYPE EQUAL LEFT_BRACE BODY RIGHT_BRACE
    # variables
    arguments = []
    generic_type = None
    returnType = None
    body = None

    # statements
    expect(TokenType.DEF)
    defName = nextToken()
    expect(TokenType.IDENTIFIER)

    if nextTokenType() == TokenType.LEFT_BRACKET:
        expect(TokenType.LEFT_BRACKET)
        generic_type = nextToken()
        expect(TokenType.TYPE_NAME)
        expect(TokenType.RIGHT_BRACKET)

    if nextTokenType() == TokenType.LEFT_PAREN:
        while nextTokenType() == TokenType.LEFT_PAREN:
            expect(TokenType.LEFT_PAREN)
            arguments = []
            # def example(a : Int, b: Int)
            while nextTokenType() == TokenType.IDENTIFIER:
                if len(arguments) > 0:
                    expect(TokenType.COMMA)
                nextArgName = nextToken().lexeme
                expect(TokenType.IDENTIFIER)
                expect(TokenType.COLON)
                nextArgType = nextToken().lexeme
                expect(TokenType.TYPE_NAME)
                nextArgType2 = None
                if nextTokenType() == TokenType.LEFT_BRACKET:
                    expect(TokenType.LEFT_BRACKET)
                    nextArgType2 = nextToken().lexeme
                    expect(TokenType.TYPE_NAME)
                    expect(TokenType.RIGHT_BRACKET)
                if nextArgType2 is not None:
                    arguments.append((nextArgName, (nextArgType, nextArgType2)))
                else:
                    arguments.append((nextArgName, nextArgType))
            expect(TokenType.RIGHT_PAREN)
        expect(TokenType.COLON)
        returnType = nextToken()
        expect(TokenType.TYPE_NAME)
        if nextTokenType() == TokenType.EQUAL:
            expect(TokenType.EQUAL)
            if nextTokenType() == TokenType.IDENTIFIER and nextNextTokenType() == TokenType.MATCH:
                expect(TokenType.IDENTIFIER)
                expect(TokenType.MATCH)
                body = parseBlock()
            elif nextTokenType() == TokenType.IDENTIFIER and (nextNextTokenType() == TokenType.PLUS or nextNextTokenType() == TokenType.MINUS or nextNextTokenType() == TokenType.STAR or nextNextTokenType() == TokenType.SLASH):
                a = nextToken().lexeme
                expect(TokenType.IDENTIFIER)
                op = nextToken()
                consume()
                b = nextToken().lexeme
                expect(TokenType.IDENTIFIER)
                body = binaryExpr(op, a, b)
            else:
                body =  parseBlock()
    elif nextTokenType() == TokenType.COLON:
        expect(TokenType.COLON)
        returnType = nextToken()
        expect(TokenType.TYPE_NAME)
        if nextTokenType() == TokenType.EQUAL:
            expect(TokenType.EQUAL)
            body = literalExpr(nextToken().lexeme)

    return defDeclStmt(defName, generic_type, arguments, returnType, body)

def parseBlock():
    #grammar
    # LEFT_BRACE statement* RIGHT_BRACE
    expect(TokenType.LEFT_BRACE)
    statements = []
    while nextTokenType() != TokenType.RIGHT_BRACE and nextTokenType() is not None:
        statement = parseStatement()
        statements.append(statement)
    expect(TokenType.RIGHT_BRACE)
    return block(statements)

def parsePrimary():
    # grammar:
    #    primary: IDENTIFIER | STRING | S_INTERPOLATOR | F_INTERPOLATOR | RAW_INTERPOLATOR | NUMBER | TRUE | FALSE | LEFT_PAREN expression RIGHT_PAREN
    if nextTokenType() == TokenType.IDENTIFIER:
        varName = nextToken().lexeme
        consume()
        if nextTokenType() == TokenType.DOT:
            expect(TokenType.DOT)
            if nextTokenType() == TokenType.IDENTIFIER:
                member = nextToken()
                expect(TokenType.IDENTIFIER)
                return memberAccessExpr(varName, member)
            elif nextTokenType() == TokenType.FOREACH:
                arguments = []
                expect(TokenType.FOREACH)
                expect(TokenType.LEFT_PAREN)
                def_name = nextToken().lexeme
                consume()
                if nextTokenType() == TokenType.LEFT_PAREN:
                    while nextTokenType() != TokenType.RIGHT_PAREN:
                        if len(arguments) > 0:
                            expect(TokenType.COMMA)
                        argument = nextToken().lexeme
                        arguments.append(literalExpr(argument))
                        expect(TokenType.IDENTIFIER)
                    expect(TokenType.RIGHT_PAREN)
                expect(TokenType.RIGHT_PAREN)
                return foreachExpr(varRefExpr(varName), defCallExpr(def_name, arguments))
            else:
                raise SyntaxError("The parser expected an IDENTIFIER token for a valid token in the access expression but found {nextTokenType()} ({nextToken().lexeme}). Please verify the syntax.")
        elif nextTokenType() == TokenType.LEFT_PAREN:
            expect(TokenType.LEFT_PAREN)
            arguments = []
            while nextTokenType() != TokenType.RIGHT_PAREN:
                if len(arguments) > 0:
                    expect(TokenType.COMMA)
                argument = nextToken().lexeme
                arguments.append(literalExpr(argument))
                consume()
                if nextTokenType() == TokenType.RIGHT_PAREN and nextNextTokenType() == TokenType.WILDCARD:
                    expect(TokenType.RIGHT_PAREN)
                    expect(TokenType.WILDCARD)
                    isCurried = True
                    return curriedDefExpr(varName, arguments, isCurried)
            expect(TokenType.RIGHT_PAREN)
            return defCallExpr(varName, arguments)
        elif nextNextTokenType() == TokenType.PLUS or nextNextTokenType() == TokenType.MINUS or nextNextTokenType() == TokenType.STAR or nextNextTokenType() == TokenType.SLASH:
            op = nextToken()
            consume()
            lhs = varName
            rhs = nextToken().lexeme
            return binaryExpr(op, lhs, rhs)
        else:
            return varRefExpr(varName)
    if nextTokenType() == TokenType.STRING:
        # the value of a string literal is everything inside the double quotes, but NOT including the double quotes themselves
        literalValue = nextToken().lexeme[1:-1]
        consume()
        return literalExpr(literalValue)

    if nextTokenType() == TokenType.TYPE_NAME:
        varName = nextToken().lexeme
        consume()
        return varRefExpr(varName)
    if nextTokenType() == TokenType.S_INTERPOLATOR:
        # the value of a string literal is everything inside the double quotes, but NOT including the double quotes themselves
        literalValue = nextToken().lexeme[1:-1]
        particles = []
        particle = ""
        faced_val = False # identify $val
        for letter in literalValue:
            if letter == '$':
                faced_val = True
                particles.append(literalExpr(particle))
                particle = ""
            elif faced_val == True and not letter.isalpha():
                faced_val = False
                particles.append(varRefExpr(particle))
                particle = ""
                particle += letter
            else:
                particle += letter
        if particle is not None and faced_val == True:
            particles.append(varRefExpr(particle))
        if particle is not None and faced_val == False:
            particles.append(literalExpr(particle))
        consume()
        return stringInterpolationExpr("s", particles)
    if nextTokenType() == TokenType.F_INTERPOLATOR:
        # the value of a string literal is everything inside the double quotes, but NOT including the double quotes themselves
        literalValue = nextToken().lexeme[1:-1]
        consume()
        return literalExpr(literalValue)
    if nextTokenType() == TokenType.RAW_INTERPOLATOR:
        # the value of a string literal is everything inside the double quotes, but NOT including the double quotes themselves
        literalValue = nextToken().lexeme[1:-1]
        consume()
        return literalExpr(literalValue)
    if nextTokenType() == TokenType.NUMBER:
        # Design decision here: What kinds of numbers do we support?
        # For now, we'll go with: the value of a numeric literal is whatever Python float() does
        literalValue = float(nextToken().lexeme)
        consume()
        return literalExpr(literalValue)
    if nextTokenType() == TokenType.TRUE:
        consume()
        return literalExpr(True)
    if nextTokenType() == TokenType.FALSE:
        consume()
        return literalExpr(False)
    if nextTokenType() == TokenType.LEFT_PAREN:
        consume()
        expr = parseExpression()
        expect(TokenType.RIGHT_PAREN)
        return expr
    if nextTokenType() == TokenType.NEW:
        consume()
        name = nextToken().lexeme
        consume()
        expr = className(name)
        return expr
    if nextTokenType() == TokenType.WILDCARD:
        lhs = valRefExpr(nextToken().lexeme)
        consume()
        operator = nextToken()
        consume()
        rhs = literalExpr(nextToken().lexeme)
        consume()
        return binaryExpr(operator, lhs, rhs)
    # if we get here, there was supposed to be an expression, but we didn't find one, so that's an error
    raise SyntaxError(f"Expected start of an expression, but found {nextTokenType()} ({nextToken().lexeme}) on line {nextToken().line_num}")


program = parseProgram()

for ast in program:
    print()
    print(ast)
print()

# Interpreter
# -----
# Input: list of ASTs
# Output: [none, just runs the program]

@dataclass
class Environment():
    variables: dict   # variables local to this environment
    parent: object    # parent environment, or None if this is the global environment
global_environment = Environment({}, None)
environment = global_environment

@dataclass
class Callable:
    arguments: list
    body: object

def runStmt(statement):
    # dispatch to the appropriate run function
    if isinstance(statement, objectStmt):
        runObjectStmt(statement)
    elif isinstance(statement, defDeclStmt):
        runDefDeclStmt(statement)
    elif isinstance(statement, valDeclStmt):
        runValDeclStmt(statement)
    elif isinstance(statement, varDeclStmt):
        runVarDeclStmt(statement)
    elif isinstance(statement, printStmt):
        runPrintStmt(statement)
    elif isinstance(statement, printlnStmt):
        runPrintlnStmt(statement)
    elif isinstance(statement, block):
        runBlock(statement)
    elif isinstance(statement, assignStmt):
        runAssignStmt(statement)
    else:
        raise RuntimeError(f"Unimplemented statement type {statement.__class__}")

def runAssignStmt(statement):
    cur_environment = environment
    varName = statement.name.lexeme
    while varName not in cur_environment.variables:
        if cur_environment.parent is not None:
            cur_environment = cur_environment.parent
        else:
            raise RuntimeError(f"Attempt to assign to undeclared variable {varName} on line {statement.name.line_num}")
    value = evalExpr(statement.value)
    cur_environment.variables[varName] = value

def evalStringInterExpr(statement):
    interpolation_name = statement.interpolator
    parts = statement.parts
    print_statement = ""
    for part in parts:
        print_part = evalExpr(part)
        print_statement = print_statement + str(print_part)
    return print_statement

def runBlock(statement):
    # semantics of running a block:
    #   create a new environment, child of existing 'environment'
    #   run the statements in that environment
    #   destroy environment, restoring previous environment
    global environment
    environment = Environment({}, environment)
    if PRINT_ENV:
        cur_environment = environment
        print(f"STACK (entering block): ")
        while cur_environment is not None:
            print(f"   {cur_environment.variables}")
            cur_environment = cur_environment.parent
    for s in statement.statements:
        runStmt(s)
        # debug code start
    cur_environment = environment
    if PRINT_ENV:
        print(f"STACK (exit block): ")
        while cur_environment is not None:
            print(f"   {cur_environment.variables}")
            cur_environment = cur_environment.parent

    environment = environment.parent

def runPrintStmt(statement):
    expr = evalExpr(statement.expr)
    print(expr)

def runPrintlnStmt(statement):
    expr = evalExpr(statement.expr)
    print(expr, "\n")

def runVarDeclStmt(statement):
    # grammar
    # varDecl : var name : type_name equal value
    varName = statement.name.lexeme
    if varName in environment.variables:
        # design decision: redeclaring a variable in the same scope is an error
        raise RuntimeError(f"Attempt to redeclare existing variable {varName} on line {statement.name.line_num}")
    if statement.initVal is not None:
        initVal = evalExpr(statement.initVal)
        environment.variables[varName] = initVal
    else:
        # design decision: variables with no initial value get a placeholder None value,
        # and it's an error to reference them without first assigning a value
        environment.variables[varName] = None

def runValDeclStmt(statement):
    # grammar
    # valDecl : val name : type_name equal value
    valName = statement.name.lexeme
    if valName in environment.variables:
        # design decision: redeclaring a variable in the same scope is an error
        raise RuntimeError(f"Attempt to redeclare existing variable {valName} on line {statement.name.line_num}")
    if statement.initVal is not None:
        initVal = evalExpr(statement.initVal)
        environment.variables[valName] = initVal
    else:
        # design decision: variables with no initial value get a placeholder None value,
        # and it's an error to reference them without first assigning a value
        environment.variables[valName] = None

def runDefDeclStmt(statement):
    # grammar
    # defDecl: DEF IDENTIFIER (LEFT_BRACKET TYPE_NAME RIGHT_BRACKET)-optional
    defName = statement.name.lexeme
    if defName in environment.variables:
        raise RuntimeError(f"Attempt to define function with existing name {defName}")
    environment.variables[defName] = Callable(statement.arguments, statement.body)
    if defName == "main":
        runStmt(statement.body)

def evalExpr(expr):
    if isinstance(expr, literalExpr):
        return evalLiteralExpr(expr)
    if isinstance(expr, unaryExpr):
        return evalUnaryExpr(expr)
    if isinstance(expr, binaryExpr):
        return evalBinaryExpr(expr)
    if isinstance(expr, varRefExpr):
        return evalVarRefExpr(expr)
    if isinstance(expr, defCallExpr):
        return evalDefCallExpr(expr)
    if isinstance(expr, stringInterpolationExpr):
        return evalStringInterExpr(expr)
    else:
        raise RuntimeError(f"Unimplemented expression type {expr.__class__}")

def evalVarRefExpr(expr):
    # two types of errors in variable references: undeclared and uninitialized
    # (study question: what's the difference?)
    cur_environment = environment
    while expr.name not in cur_environment.variables:
        if cur_environment.parent is not None:
            cur_environment = cur_environment.parent
        else:
            raise RuntimeError(f"Attempt to reference undeclared variable {expr.name}")
    if cur_environment.variables[expr.name] is None:
        raise RuntimeError(f"Attempt to reference uninitialized variable {expr.name}")
    return cur_environment.variables[expr.name]

def runBlock(statement):
    # semantics of running a block:
    #   create a new environment, child of existing 'environment'
    #   run the statements in that environment
    #   destroy environment, restoring previous environment
    global environment
    environment = Environment({}, environment)
    if PRINT_ENV:
        cur_environment = environment
        print(f"STACK (entering block): ")
        while cur_environment is not None:
            print(f"   {cur_environment.variables}")
            cur_environment = cur_environment.parent
    for s in statement.statements:
        runStmt(s)
        # debug code start
    cur_environment = environment
    if PRINT_ENV:
        print(f"STACK (exit block): ")
        while cur_environment is not None:
            print(f"   {cur_environment.variables}")
            cur_environment = cur_environment.parent

def evalDefCallExpr(expr):
    # first evaluate both the function expression and argument expressions to values
    callable = evalExpr(expr.function)
    if not isinstance(callable, Callable):
        raise RuntimeError(f"Tried to call non-callable '{callable}'")
    arguments = [evalExpr(arg) for arg in expr.arguments]
    # set up a new environment to run the function in
    # with the function parameters bound to the passed values
    global environment
    old_environment = environment
    environment = Environment({}, global_environment)  # Note: lexical scope!
    for param, val in zip(callable.arguments, arguments):
        environment.variables[param] = val
    # run the body of the function, and catch any return
    returnVal = None
    try:
        runBlock(callable.body)
    except ReturnException as r:
        returnVal = r.args[0]
    environment = old_environment # restore previous environment
    return returnVal

class ReturnException(Exception):
    pass

def runReturnStmt(statement):
    returnVal = evalExpr(statement.expr)
    raise ReturnException(returnVal)

def evalLiteralExpr(expr):
    return expr.value

def evalUnaryExpr(expr):
    rhsVal = evalExpr(expr.rhs)
    if expr.operator.tokenType == TokenType.MINUS:
        if not isinstance(rhsVal, float):
            raise RuntimeError(f"Applied unary '-' to a non-number on line {expr.operator.line_num}")
        return -rhsVal
    if expr.operator.tokenType == TokenType.BANG:
        if not isinstance(rhsVal, bool):
            raise RuntimeError(f"Applied unary '!' to a non-boolean on line {expr.operator.line_num}")
        return not rhsVal
    raise RuntimeError(f"Unrecognized unary operator: {expr.operator.tokenType} on line {expr.operator.line_num}")

def evalBinaryExpr(expr):
    lhsVal = evalExpr(expr.lhs)
    rhsVal = evalExpr(expr.rhs)
    # TODO: should check types before applying operator, like in evalUnaryExpr() above
    # arithmetic operators
    if expr.operator.tokenType == TokenType.PLUS:
        return lhsVal + rhsVal
    if expr.operator.tokenType == TokenType.MINUS:
        return lhsVal - rhsVal
    if expr.operator.tokenType == TokenType.STAR:
        return lhsVal * rhsVal
    if expr.operator.tokenType == TokenType.SLASH:
        return lhsVal / rhsVal
    # comparison operators
    if expr.operator.tokenType == TokenType.EQUAL_EQUAL:
        return lhsVal == rhsVal
    if expr.operator.tokenType == TokenType.BANG_EQUAL:
        return lhsVal != rhsVal
    if expr.operator.tokenType == TokenType.GREATER:
        return lhsVal > rhsVal
    if expr.operator.tokenType == TokenType.GREATER_EQUAL:
        return lhsVal >= rhsVal
    if expr.operator.tokenType == TokenType.LESS:
        return lhsVal < rhsVal
    if expr.operator.tokenType == TokenType.LESS_EQUAL:
        return lhsVal <= rhsVal
    raise RuntimeError(f"Unrecognized binary operator: {expr.op.tokenType} on line {expr.op.line_num}")

def runObjectStmt(statement):
    name_object = statement.name

    if name_object is None:
        raise RuntimeError(f"No object named {statement.name}")
    else:
        runStmt(statement.body)

# actually run the program!
for ast in program:
    if PRINT_ENV:
        print(f"STACK: ")
        cur_environment = environment
        while cur_environment is not None:
            print(f"   {cur_environment.variables}")
            cur_environment = cur_environment.parent
    runStmt(ast)
if PRINT_ENV:
    print(f"STACK: ")
    cur_environment = environment
    while cur_environment is not None:
        print(f"   {cur_environment.variables}")
        cur_environment = cur_environment.parent
