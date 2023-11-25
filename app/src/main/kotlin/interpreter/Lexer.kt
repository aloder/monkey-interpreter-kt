package interpreter

public class Lexer {

  val input: String
  var position: Int = 0
  var readPosition: Int = 0
  var ch: Char = 0.toChar()
  public constructor(input: String) {
    this.input = input
    readChar()
  }
  public fun nextToken(): Token {
    skipWhitespace()

    val result =
        when (ch) {
          '=' -> {
            if (peekChar() == '=') {
              readChar()
              Token(TokenType.EQ, "==")
            } else {
              Token(TokenType.ASSIGN, "=")
            }
          }
          ';' -> Token(TokenType.SEMICOLON, ";")
          '(' -> Token(TokenType.LPAREN, "(")
          ')' -> Token(TokenType.RPAREN, ")")
          ',' -> Token(TokenType.COMMA, ",")
          '+' -> Token(TokenType.PLUS, "+")
          '{' -> Token(TokenType.LBRACE, "{")
          '}' -> Token(TokenType.RBRACE, "}")
          '-' -> Token(TokenType.MINUS, "-")
          '!' -> {
            if (peekChar() == '=') {
              readChar()
              Token(TokenType.NOT_EQ, "!=")
            } else {
              Token(TokenType.BANG, "!")
            }
          }
          '*' -> Token(TokenType.ASTERISK, "*")
          '/' -> Token(TokenType.SLASH, "/")
          '<' -> Token(TokenType.LT, "<")
          '>' -> Token(TokenType.GT, ">")
          0.toChar() -> Token(TokenType.EOF, "")
          else -> {
            if (isLetter(ch)) {
              val literal = readIdentifier()
              return Token(lookupIdent(literal), literal)
            } else if (isDigit(ch)) {
              return Token(TokenType.INT, readNumber())
            } else {
              Token(TokenType.ILLEGAL, ch.toString())
            }
          }
        }
    readChar()
    return result
  }

  val keywords =
      mapOf(
          "fn" to TokenType.FUNCTION,
          "let" to TokenType.LET,
          "true" to TokenType.TRUE,
          "false" to TokenType.FALSE,
          "if" to TokenType.IF,
          "else" to TokenType.ELSE,
          "return" to TokenType.RETURN
      )

  private fun lookupIdent(ident: String): TokenType {
    return keywords.getOrDefault(ident, TokenType.IDENT)
  }

  private fun readIdentifier(): String {
    val position = position
    while (isLetter(ch)) {
      readChar()
    }
    return input.substring(position, this.position)
  }

  private fun isLetter(ch: Char): Boolean {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
  }

  private fun isDigit(ch: Char): Boolean {
    return '0' <= ch && ch <= '9'
  }

  private fun readNumber(): String {
    val position = position
    while (isDigit(ch)) {
      readChar()
    }
    return input.substring(position, this.position)
  }

  private fun skipWhitespace() {
    while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
      readChar()
    }
  }

  private fun peekChar(): Char {
    if (readPosition >= input.length) {
      return 0.toChar()
    } else {
      return input[readPosition]
    }
  }
  private fun readChar() {
    if (readPosition >= input.length) {
      ch = 0.toChar()
    } else {
      ch = input[readPosition]
    }
    position = readPosition
    readPosition += 1
  }
}
