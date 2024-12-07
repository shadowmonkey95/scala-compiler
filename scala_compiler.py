from enum import Enum
from dataclasses import dataclass
from typing import Optional

code = """
object ScalaExample {
  def main(args: Array[String]): Unit = {
    val name: String = "Alice"
    var age: Int = 25

    println(s"Hello, $name! You are $age years old.")
    }
}
"""

PRINT_AST=False
PRINT_ENV=False

TokenType = Enum('TokenType',
                 [   # Tokens that are unambiguous single characters - WILDCARD means underscore.
                     'SEMICOLON', 'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE', 'LEFT_BRACKET', 'RIGHT_BRACKET',
                     'PLUS', 'MINUS', 'STAR', 'SLASH', 'COMMA', 'COLON', 'WILDCARD',
                     # Tokens that require 1-character lookahead
                     'EQUAL', 'EQUAL_EQUAL',
                     'BANG', 'BANG_EQUAL',
                     'LESS', 'LESS_EQUAL',
                     'GREATER', 'GREATER_EQUAL',
                     'ARROW',
                     # keywords  - var (mutable VAR) / val (immutable VAR)
                     'DEF', 'IF', 'ELSE', 'WHILE', 'VAR', 'PRINT', 'TRUE', 'FALSE', 'RETURN',
                     'OBJECT', 'VAL', 'PRINTLN', 'CASE', 'MATCH', 'ARGS',
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
        elif next_char == 's':
            if peek('"'):
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
        elif next_char == 'f':
            if peek('"'):
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
            ##Type name
            elif lexeme == "String" or lexeme == "Short" or lexeme == "Int" or \
                    lexeme == "Long" or lexeme == "Float" or lexeme == "Double" or \
                    lexeme == "Char" or lexeme == "Boolean" or lexeme == "Unit" or \
                    lexeme == "Array" or lexeme == "List" or lexeme == "trait" or \
                    lexeme == "Any":
                token = Token(TokenType.TYPE_NAME, lexeme, line_num)
            else: # otherwise it's an identifier
                token = Token(TokenType.IDENTIFIER, lexeme, line_num)
            tokens.append(token)
        else:
            raise Exception(f"Character {next_char} on line {line_num} not a valid start of token.")
    
for token in tokens:
    print(token)
# Parser
# Input: tokens (list of tokens, each of class Token)
# Output: list of ASTs (each of one of the dataclasses below)
# ======

code = """
object ScalaExample {
  def main(args: Array[String]): Unit = {
    val name: String = "Alice"
    var age: Int = 25

    println(s"Hello, $name! You are $age years old.")
    }
}
"""

@dataclass
class block:
    statements: list   # list of statements
    
@dataclass
class TmplDef:
    templateName: Token     # IDENTIFIER token
    arguments: list # list of IDENTIFIER
    templateBody: object    # body of the template (block)
    
@dataclass
class Def:
    name: Token     # IDENTIFIER token
    arguments: dict # dictionary of IDENTIFIER with TYPE
    returnType: Token # TYPE token
    body: object    # body of the function (block)

@dataclass
class FunParamClause:
    params: dict # dictionary of IDENTIFIER with TYPE

@dataclass
class TypedFunParam:
    name: Token     # IDENTIFIER token
    type: Token     # TYPE token
    subType: Optional[Token] = None
# @dataclass
# class TypedFunParam:
#     name: Token     # IDENTIFIER token
#     type: Token     # TYPE token
    
@dataclass
class ifStmt:
    condition: object  # expression
    runIfTrue: block
    runIfFalse: object # block or None

@dataclass
class whileStmt:
    condition: object  # expression
    body: block

@dataclass
class printStmt:
    expr: object    # expression

@dataclass
class returnStmt:
    expr: object    # expression to return

@dataclass
class expressionStmt:
    expr: object       # the expression

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
class funCallExpr:
    function: object   # expression that evaluates to a Callable
    arguments: list    # list of expressions that will be the arguments

@dataclass
class literalExpr:
    value: object      # float, string, or bool

@dataclass
class varRefExpr:
    name: str
    
def parseTmplDef():
    # grammar:
    #   ::= [‘case’] ‘object’ ObjectDef
    #       -> ObjectDef         ::=  id [Template]
    #          -> Template          ::=  InheritClauses [TemplateBody]
    #             -> InheritClauses    ::=  NO IMPLEMENT CASE
    #             -> TemplateBody -> TemplateStat  ::=  Def
    expect(TokenType.OBJECT)
    templateName = nextToken()
    expect(TokenType.IDENTIFIER)
    expect(TokenType.LEFT_BRACE)
    arguments = []
    # def example(a, b, c)
    while nextTokenType() != TokenType.RIGHT_BRACE:
        templateBody = parseDef()
    expect(TokenType.RIGHT_BRACE)
    return TmplDef(templateName, arguments, templateBody)

def parseDef():
    # grammar:
    #  ::= 'def' id [TypeParamClause] FunParamClause ':' Type '=' Block
    #  ex: def main(args: Array[String]): Unit = {
    expect(TokenType.DEF)
    name = nextToken()
    expect(TokenType.IDENTIFIER)
    arguments = parseFunParamClause()
    expect(TokenType.COLON)
    returnType = nextToken()
    expect(TokenType.TYPE_NAME)
    expect(TokenType.EQUAL)
    body = parseBlock()
    return Def(name, arguments, returnType, body)

def parseFunParamClause():
    # (args: Array[String])
    expect(TokenType.LEFT_PAREN)
    params = []
    while nextTokenType() != TokenType.RIGHT_PAREN and nextTokenType() is not None:
        if nextTokenType() == TokenType.IDENTIFIER:
            obj = parseTypedFunParam()
        
        
        
        if nextTokenType() == TokenType.IDENTIFIER:
            name = nextToken().lexeme
        elif nextTokenType() == TokenType.ARGS:
            name = 'args'    
        # params[name] = nextToken()
        consume()
        expect(TokenType.COLON)
        if nextTokenType() == TokenType.TYPE_NAME and nextNextTokenType() == TokenType.LEFT_BRACKET:
            typeName = nextToken().lexeme
            expect(TokenType.TYPE_NAME)
            typeName = typeName + "["
            expect(TokenType.LEFT_BRACKET)
            typeName = typeName + nextToken().lexeme
            expect(TokenType.TYPE_NAME)
            typeName = typeName + "["
            expect(TokenType.RIGHT_BRACKET)
        else:
            typeName = nextToken().lexeme
            expect(TokenType.TYPE_NAME)
        params[name] = typeName
        # params[name] = nextToken()
        # print(f"params = {params}")
        # expect(TokenType.TYPE_NAME)
        if nextTokenType() == TokenType.COMMA:
            consume()
    expect(TokenType.RIGHT_PAREN)
    return FunParamClause(params)

def parseTypedFunParam():
    # grammar:
    #   TypedFunParam     ::=  id ‘:’ Type
    if nextTokenType() == TokenType.IDENTIFIER or nextTokenType() == TokenType.ARGS:
        name = nextToken()
    expect(TokenType.COLON)
    
    
        
    # name = nextToken()
    # expect(TokenType.IDENTIFIER)
    # expect(TokenType.COLON)
    # type = nextToken()
    # expect(TokenType.TYPE_NAME)
    # return TypedFunParam(name, type)

def parseBlock():
    # grammar:
    #    LEFT_BRACE statement* RIGHT_BRACE
    expect(TokenType.LEFT_BRACE)
    statements = []
    while nextTokenType() != TokenType.RIGHT_BRACE and nextTokenType() is not None:
        statement = parseStatement()
        statements.append(statement)
    expect(TokenType.RIGHT_BRACE)
    return block(statements)

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
    # if nextTokenType() == TokenType.FUN:
    #     return parseFunDeclStmt()
    # elif nextTokenType() == TokenType.VAR:
    #     return parseVarDeclStmt()
    # elif nextTokenType() == TokenType.IDENTIFIER and nextNextTokenType() == TokenType.EQUAL:
    #     return parseAssignStmt()
    # elif nextTokenType() == TokenType.IF:
    #     return parseIfStmt()
    # elif nextTokenType() == TokenType.WHILE:
    #     return parseWhileStmt()
    # elif nextTokenType() == TokenType.PRINT:
    #     return parsePrintStmt()
    # elif nextTokenType() == TokenType.RETURN:
    #     return parseReturnStmt()
    if nextTokenType() == TokenType.OBJECT:
        return parseTmplDef()
    elif nextTokenType() == TokenType.LEFT_BRACE:
        return parseBlock()
    elif nextTokenType() == None:
        raise Exception("Unexpected end of file")
    # if we got this far, either it's an error, or an expression statement
    return parseExpressionStmt()

def parseExpressionStmt():
    expr = parseExpression()
    if nextTokenType() == TokenType.SEMICOLON:
        consume()
    return expressionStmt(expr)

def parseExpression():
    # grammar:
    #    expression: equality
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
    #           | funCall
    if nextTokenType() in (TokenType.BANG, TokenType.MINUS):
        operator = nextToken()
        consume()
        rhs = parseUnary()
        return unaryExpr(operator, rhs)
    return parseFunCall()

def parseFunCall():
    # grammar:
    #   funCall: primary (LEFT_PAREN arguments? RIGHT_PAREN)?
    #   arguments: expression ( COMMA expression )*
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
        expr = funCallExpr(expr, arguments)
    return expr

def parsePrimary():
    # grammar:
    #    primary: IDENTIFIER | STRING | NUMBER | TRUE | FALSE | LEFT_PAREN expression RIGHT_PAREN
    if nextTokenType() == TokenType.IDENTIFIER:
        varName = nextToken().lexeme
        consume()
        return varRefExpr(varName)
    if nextTokenType() == TokenType.STRING:
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
    # if we get here, there was supposed to be an expression, but we didn't find one, so that's an error
    checkCur(1)
    raise SyntaxError(f"Expected start of an expression, but found {nextTokenType()} ({nextToken().lexeme}) on line {nextToken().line_num}")
currentToken = 0
# some convenience functions for looking at and/or consuming the next token
def checkCur(a):
    print(f"Debug {a}: {tokens[currentToken]}")
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
    
program = parseProgram()

if PRINT_AST:
    for ast in program:
        print()
        print(ast)
    print()

# Interpreter
# ---
# Input: list of ASTs
# Output: [none, just runs the program]