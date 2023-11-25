package interpreter.ast

import interpreter.Token
import interpreter.TokenType
import kotlin.test.Test
import kotlin.test.assertEquals

internal class AstTest {

  @Test
  fun testString() {
    val program =
        Program(
            arrayListOf(
                LetStatement(
                    Token(TokenType.LET, "let"),
                    Identifier(Token(TokenType.IDENT, "myVar"), "myVar"),
                    Identifier(Token(TokenType.IDENT, "anotherVar"), "anotherVar")
                ),
            )
        )
    assertEquals("let myVar = anotherVar;", program.toString())
  }
}
