package interpreter.parser

import interpreter.Lexer
import interpreter.ast.LetStatement
import interpreter.ast.Statement
import kotlin.test.Test
import kotlin.test.assertEquals

internal class ParserTest {

  @Test
  fun testLetStatements() {
    val input = """
    let x = 5;
    let y = 10;
    let foobar = 838383;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    checkParserErrors(parser)
    assertEquals(program.statements.size, 3)
    assertLetStatement(program.statements[0], "x")
    assertLetStatement(program.statements[1], "y")
    assertLetStatement(program.statements[2], "foobar")
  }

  @Test
  fun testReturnStatements() {
    val input = """
    return 5;
    return 10;
    return 993322;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    checkParserErrors(parser)
    assertEquals(3, program.statements.size)
    assertReturnStatement(program.statements[0])
    assertReturnStatement(program.statements[1])
    assertReturnStatement(program.statements[2])
  }
  fun assertReturnStatement(statement: Statement) {
    assertEquals(statement.tokenLiteral(), "return")
  }
  fun assertLetStatement(statement: Statement, name: String) {
    assertEquals(statement.tokenLiteral(), "let")
    val letStatement = statement as LetStatement
    assertEquals(letStatement.name.value, name)
    assertEquals(letStatement.name.tokenLiteral(), name)
  }

  fun checkParserErrors(parser: Parser) {
    val errors = parser.getErrors()
    if (errors.size == 0) {
      return
    }
    println("parser has ${errors.size} errors")
    for (error in errors) {
      println("parser error: $error")
    }
    assertEquals(errors.size, 0)
  }
}
