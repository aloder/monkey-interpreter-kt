package interpreter

import kotlin.test.Test
import kotlin.test.assertEquals

internal class LexerTest {

  @Test
  fun firstTest() {
    val input = "=+(){},;"
    assertEquals("", "")
    val expected =
        listOf(
            Pair(TokenType.ASSIGN, "="),
            Pair(TokenType.PLUS, "+"),
            Pair(TokenType.LPAREN, "("),
            Pair(TokenType.RPAREN, ")"),
            Pair(TokenType.LBRACE, "{"),
            Pair(TokenType.RBRACE, "}"),
            Pair(TokenType.COMMA, ","),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.EOF, ""),
        )
    val lexer = Lexer(input)
    val actual = mutableListOf<Pair<TokenType, String>>()
    while (true) {
      val token = lexer.nextToken()
      actual.add(Pair(token.type, token.literal))
      if (token.type == TokenType.EOF) {
        break
      }
    }
    assertEquals(expected, actual)
  }

  @Test
  fun testLets() {
    val input =
        """ let five = 5;
      let ten = 10;
      let add = fn(x, y){
      x + y;
      };
      let result = add(five, ten);
      if (5 < 10) { return true; } else { return false; }
      10 == 10; 
      10 != 9;
    """

    val expected =
        listOf(
            Pair(TokenType.LET, "let"),
            Pair(TokenType.IDENT, "five"),
            Pair(TokenType.ASSIGN, "="),
            Pair(TokenType.INT, "5"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.LET, "let"),
            Pair(TokenType.IDENT, "ten"),
            Pair(TokenType.ASSIGN, "="),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.LET, "let"),
            Pair(TokenType.IDENT, "add"),
            Pair(TokenType.ASSIGN, "="),
            Pair(TokenType.FUNCTION, "fn"),
            Pair(TokenType.LPAREN, "("),
            Pair(TokenType.IDENT, "x"),
            Pair(TokenType.COMMA, ","),
            Pair(TokenType.IDENT, "y"),
            Pair(TokenType.RPAREN, ")"),
            Pair(TokenType.LBRACE, "{"),
            Pair(TokenType.IDENT, "x"),
            Pair(TokenType.PLUS, "+"),
            Pair(TokenType.IDENT, "y"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.RBRACE, "}"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.LET, "let"),
            Pair(TokenType.IDENT, "result"),
            Pair(TokenType.ASSIGN, "="),
            Pair(TokenType.IDENT, "add"),
            Pair(TokenType.LPAREN, "("),
            Pair(TokenType.IDENT, "five"),
            Pair(TokenType.COMMA, ","),
            Pair(TokenType.IDENT, "ten"),
            Pair(TokenType.RPAREN, ")"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.IF, "if"),
            Pair(TokenType.LPAREN, "("),
            Pair(TokenType.INT, "5"),
            Pair(TokenType.LT, "<"),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.RPAREN, ")"),
            Pair(TokenType.LBRACE, "{"),
            Pair(TokenType.RETURN, "return"),
            Pair(TokenType.TRUE, "true"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.RBRACE, "}"),
            Pair(TokenType.ELSE, "else"),
            Pair(TokenType.LBRACE, "{"),
            Pair(TokenType.RETURN, "return"),
            Pair(TokenType.FALSE, "false"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.RBRACE, "}"),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.EQ, "=="),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.NOT_EQ, "!="),
            Pair(TokenType.INT, "9"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.EOF, "")
        )
    val lexer = Lexer(input)
    val actual = mutableListOf<Pair<TokenType, String>>()
    while (true) {
      val token = lexer.nextToken()
      actual.add(Pair(token.type, token.literal))
      if (token.type == TokenType.EOF) break
    }
    assertEquals(expected, actual)
  }
  @Test
  fun ch1_4_testLets() {
    val input = """!-/*5; 5 < 10 > 5;"""

    val expected =
        listOf(
            Pair(TokenType.BANG, "!"),
            Pair(TokenType.MINUS, "-"),
            Pair(TokenType.SLASH, "/"),
            Pair(TokenType.ASTERISK, "*"),
            Pair(TokenType.INT, "5"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.INT, "5"),
            Pair(TokenType.LT, "<"),
            Pair(TokenType.INT, "10"),
            Pair(TokenType.GT, ">"),
            Pair(TokenType.INT, "5"),
            Pair(TokenType.SEMICOLON, ";"),
            Pair(TokenType.EOF, "")
        )
    val lexer = Lexer(input)
    val actual = mutableListOf<Pair<TokenType, String>>()
    while (true) {
      val token = lexer.nextToken()
      actual.add(Pair(token.type, token.literal))
      if (token.type == TokenType.EOF) break
    }
    assertEquals(expected, actual)
  }
}
