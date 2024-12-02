# Lox implementation
# Fall 2024

from enum import Enum
from dataclasses import dataclass

code = """
fun square(x)
{
    return x*x;
}
print square(5);
"""

PRINT_AST=False  # print the ASTs after parsing?
PRINT_ENV=False  # print the environment stack during runtime?

# Scanner
# Input: code (string containing source code)
# Output: tokens (a list of tokens)
# =======

TokenType = Enum('TokenType',
                 [   # Tokens that are unambiguous single characters
                     'SEMICOLON', 'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE',
                     'PLUS', 'MINUS', 'STAR', 'SLASH', 'COMMA',
                     # Tokens that require 1-character lookahead
                     'EQUAL', 'EQUAL_EQUAL',
                     'BANG', 'BANG_EQUAL',
                     'LESS', 'LESS_EQUAL',
                     'GREATER', 'GREATER_EQUAL',
                     # keywords
                     'FUN', 'IF', 'ELSE', 'WHILE', 'VAR', 'PRINT', 'TRUE', 'FALSE', 'RETURN',
                     # identifiers and literals
                     'IDENTIFIER', 'STRING', 'NUMBER',
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
            if lexeme == "fun":
                token = Token(TokenType.FUN, lexeme, line_num)
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
            elif lexeme == "true":
                token = Token(TokenType.TRUE, lexeme, line_num)
            elif lexeme == "false":
                token = Token(TokenType.FALSE, lexeme, line_num)
            elif lexeme == "return":
                token = Token(TokenType.RETURN, lexeme, line_num)
            else: # otherwise it's an identifier
                token = Token(TokenType.IDENTIFIER, lexeme, line_num)
            tokens.append(token)
        else:
            raise Exception(f"Character {next_char} on line {line_num} not a valid start of token.")

#print(tokens)

# Parser
# Input: tokens (list of tokens, each of class Token)
# Output: list of ASTs (each of one of the dataclasses below)
# ======

@dataclass
class funDeclStmt:
    name: Token     # IDENTIFIER token
    arguments: list # list of IDENTIFIER
    body: object    # body of the function (block)

@dataclass
class varDeclStmt:
    name: Token     # IDENTIFIER token
    initVal: object # expression or None

@dataclass
class assignStmt:
    name: Token     # IDENTIFIER token
    value: object   # expression

@dataclass
class block:
    statements: list   # list of statements

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
    if nextTokenType() == TokenType.FUN:
        return parseFunDeclStmt()
    elif nextTokenType() == TokenType.VAR:
        return parseVarDeclStmt()
    elif nextTokenType() == TokenType.IDENTIFIER and nextNextTokenType() == TokenType.EQUAL:
        return parseAssignStmt()
    elif nextTokenType() == TokenType.IF:
        return parseIfStmt()
    elif nextTokenType() == TokenType.WHILE:
        return parseWhileStmt()
    elif nextTokenType() == TokenType.PRINT:
        return parsePrintStmt()
    elif nextTokenType() == TokenType.RETURN:
        return parseReturnStmt()
    elif nextTokenType() == TokenType.LEFT_BRACE:
        return parseBlock()
    elif nextTokenType() == None:
        raise Exception("Unexpected end of file")
    # if we got this far, either it's an error, or an expression statement
    return parseExpressionStmt()

def parseExpressionStmt():
    expr = parseExpression()
    expect(TokenType.SEMICOLON)
    return expressionStmt(expr)

def parseFunDeclStmt():
    # grammar:
    #   funcDecl: FUN IDENTIFIER LEFT_PAREN parameters? RIGHT_PAREN block
    #   parameters: IDENTIFIER
    #             | IDENTIFIER [COMMA parameters]
    expect(TokenType.FUN)
    funName = nextToken()
    expect(TokenType.IDENTIFIER)
    expect(TokenType.LEFT_PAREN)
    arguments = []
    # def example(a, b, c)
    while nextTokenType() != TokenType.RIGHT_PAREN:
        if len(arguments) > 0:
            expect(TokenType.COMMA)
        nextArgName = nextToken().lexeme
        expect(TokenType.IDENTIFIER)
        arguments.append(nextArgName)
    expect(TokenType.RIGHT_PAREN)
    body = parseBlock()
    return funDeclStmt(funName, arguments, body)

def parseVarDeclStmt():
    # grammar:
    #   varDecl: VAR IDENTIFIER [EQUAL expression] SEMICOLON
    expect(TokenType.VAR)
    varName = nextToken()
    expect(TokenType.IDENTIFIER)
    expr = None
    if nextTokenType() == TokenType.EQUAL:
        expect(TokenType.EQUAL)
        expr = parseExpression()
    expect(TokenType.SEMICOLON)
    return varDeclStmt(varName, expr)

def parseAssignStmt():
    # grammar:
    #    assign: IDENTIFIER EQUAL expression SEMICOLON
    varName = nextToken()
    expect(TokenType.IDENTIFIER)
    expect(TokenType.EQUAL)
    expr = parseExpression()
    expect(TokenType.SEMICOLON)
    return assignStmt(varName, expr)

def parseIfStmt():
    # grammar:
    #   ifStmt: IF LEFT_PAREN expression RIGHT_PAREN block [ELSE block]
    expect(TokenType.IF)
    expect(TokenType.LEFT_PAREN)
    condition = parseExpression()
    expect(TokenType.RIGHT_PAREN)
    runIfTrue = parseBlock()
    runIfFalse = None
    if nextTokenType == TokenType.ELSE:
        expect(TokenType.ELSE)
        runIfFalse = parseBlock()
    return ifStmt(condition, runIfTrue, runIfFalse)

def parseWhileStmt():
    # grammar:
    #   whileStmt: WHILE LEFT_PAREN expression RIGHT_PAREN block
    expect(TokenType.WHILE)
    expect(TokenType.LEFT_PAREN)
    condition = parseExpression()
    expect(TokenType.RIGHT_PAREN)
    body = parseBlock()
    return whileStmt(condition, body)

def parsePrintStmt():
    # grammar:
    #   printStmt: PRINT expression SEMICOLON
    expect(TokenType.PRINT)
    expr = parseExpression()
    expect(TokenType.SEMICOLON)
    return printStmt(expr)

def parseReturnStmt():
    # grammar:
    #    RETURN expression SEMICOLON
    expect(TokenType.RETURN)
    expr = parseExpression()
    expect(TokenType.SEMICOLON)
    return returnStmt(expr)

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
    raise SyntaxError(f"Expected start of an expression, but found {nextTokenType()} ({nextToken().lexeme}) on line {nextToken().line_num}")

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
    if isinstance(statement, funDeclStmt):
        runFunDeclStmt(statement)
    elif isinstance(statement, varDeclStmt):
        runVarDeclStmt(statement)
    elif isinstance(statement, assignStmt):
        runAssignStmt(statement)
    elif isinstance(statement, block):
        runBlock(statement)
    elif isinstance(statement, ifStmt):
        runIfStmt(statement)
    elif isinstance(statement, whileStmt):
        runWhileStmt(statement)
    elif isinstance(statement, printStmt):
        runPrint(statement)
    elif isinstance(statement, expressionStmt):
        runExpressionStmt(statement)
    elif isinstance(statement, returnStmt):
        runReturnStmt(statement)
    else:
        raise RuntimeError(f"Unimplemented statement type {statement.__class__}")

def runIfStmt(statement):
    conditionVal = evalExpr(statement.condition)
    # design decision: condition has to be True or False
    if conditionVal != True and conditionVal != False:
        raise RuntimeError(f"Expected condition of if to be boolean, got {conditionVal} instead")
    if conditionVal:
        runStmt(statement.runIfTrue)
    elif statement.runIfFalse is not None:
        runStmt(statement.runIfFalse)

def runWhileStmt(statement):
    # Note: may be instructive to compare and constrast this to runIfStmt above
    conditionVal = evalExpr(statement.condition)
    # design decision: condition has to be True or False
    if conditionVal != True and conditionVal != False:
        raise RuntimeError(f"Expected condition of while to be boolean, got {conditionVal} instead")
    while conditionVal:
        runStmt(statement.body)
        conditionVal = evalExpr(statement.condition)
        if conditionVal != True and conditionVal != False:
            raise RuntimeError(f"Expected condition of while to be boolean, got {conditionVal} instead")

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

def runPrint(statement):
    expr = evalExpr(statement.expr)
    print(expr)

def runExpressionStmt(statement):
    evalExpr(statement.expr)

class ReturnException(Exception):
    pass

def runReturnStmt(statement):
    returnVal = evalExpr(statement.expr)
    raise ReturnException(returnVal)

def runFunDeclStmt(statement):
    funName = statement.name.lexeme
    if funName in environment.variables:
        raise RuntimeError(f"Attempt to define function with existing name {funName}")
    environment.variables[funName] = Callable(statement.arguments, statement.body)

def runVarDeclStmt(statement):
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

def evalExpr(expr):
    if isinstance(expr, literalExpr):
        return evalLiteralExpr(expr)
    if isinstance(expr, unaryExpr):
        return evalUnaryExpr(expr)
    if isinstance(expr, binaryExpr):
        return evalBinaryExpr(expr)
    if isinstance(expr, varRefExpr):
        return evalVarRefExpr(expr)
    if isinstance(expr, funCallExpr):
        return evalFunCallExpr(expr)
    else:
        raise RuntimeError(f"Unimplemented expression type {expr.__class__}")

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

def evalFunCallExpr(expr):
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
