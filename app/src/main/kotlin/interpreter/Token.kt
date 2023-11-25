package interpreter

public class Token(public val type: TokenType, public val literal: String) {
  override fun toString(): String {
    return "Token($type, $literal)"
  }
}

public enum class TokenType(val literal: String) {
  ILLEGAL("ILLEGAL"),
  EOF("EOF"),
  // Identifiers + literals
  IDENT("IDENT"), // add, foobar, x, y, ...
  INT("INT"), // 1343456
  // Operators
  ASSIGN("="),
  PLUS("+"),
  MINUS("-"),
  BANG("!"),
  ASTERISK("*"),
  SLASH("/"),
  LT("<"),
  GT(">"),
  EQ("=="),
  NOT_EQ("!="),
  // Delimiters
  COMMA(","),
  SEMICOLON(";"),
  LPAREN("("),
  RPAREN(")"),
  LBRACE("{"),
  RBRACE("}"),
  // Keywords
  FUNCTION("FUNCTION"),
  LET("LET"),
  TRUE("TRUE"),
  FALSE("FALSE"),
  IF("IF"),
  ELSE("ELSE"),
  RETURN("RETURN")
}
