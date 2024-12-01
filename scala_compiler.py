from enum import Enum
from dataclasses import dataclass

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