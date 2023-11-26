package interpreter.parser

import interpreter.Lexer
import interpreter.ast.BooleanLiteral
import interpreter.ast.Expression
import interpreter.ast.ExpressionStatement
import interpreter.ast.Identifier
import interpreter.ast.IfExpression
import interpreter.ast.InfixExpression
import interpreter.ast.IntegerLiteral
import interpreter.ast.LetStatement
import interpreter.ast.PrefixExpression
import interpreter.ast.Statement
import kotlin.collections.listOf
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

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
  fun testIdentifierExpression() {
    val input = """
    foobar;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    checkParserErrors(parser)
    assertEquals(1, program.statements.size)
    assertIdentiferExpressionStatement(program.statements[0], "foobar")
  }

  @Test
  fun testIntegerLiteral() {
    val input = """
    5;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    checkParserErrors(parser)
    assertEquals(1, program.statements.size)
    val statement = program.statements[0]
    assertEquals(true, statement is ExpressionStatement)
    val expressionStatement = statement as ExpressionStatement
    assertEquals(true, expressionStatement.expression is IntegerLiteral)
    val integerLiteral = expressionStatement.expression as IntegerLiteral
    assertEquals(5, integerLiteral.value)
  }
  @Test
  fun testPrefixLiteral() {
    val input = """
    !5;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()

    checkParserErrors(parser)
    assertEquals(1, program.statements.size)
    val statement = program.statements[0]
    assertEquals(true, statement is ExpressionStatement)
    val expressionStatement = statement as ExpressionStatement
    val exp = expressionStatement.expression as PrefixExpression
    assertEquals("!", exp.operator)
    assertEquals(true, exp.right is IntegerLiteral, "Expression is of type Integer Literal")
    val lit = exp.right as IntegerLiteral
    assertEquals(5L, lit.value)
  }

  @Test
  fun testInfixLiteral() {
    val input = """
    5*5;
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()

    checkParserErrors(parser)
    assertEquals(1, program.statements.size)
    val statement = program.statements[0]
    assertTrue(statement is ExpressionStatement)
    val expressionStatement = statement as ExpressionStatement
    assertInfixExpression(expressionStatement.expression, 5, "*", 5)
  }
  @Test
  fun testPriorityStatements() {
    data class TestPriority(val input: String, val expected: String)
    val inputs: List<TestPriority> =
        listOf(
            TestPriority("-a * b", "((-a) * b)"),
            TestPriority("a * b", "(a * b)"),
            TestPriority("!-a", "(!(-a))"),
            TestPriority("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            TestPriority("true", "true"),
            TestPriority("false", "false"),
            TestPriority("3 > 5 == false", "((3 > 5) == false)"),
            TestPriority("a + b - c", "((a + b) - c)"),
            TestPriority("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            TestPriority("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        )

    for (input in inputs) {
      val lexer = Lexer(input.input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)
      assertEquals(input.expected, program.toString())
    }
  }
  @Test
  fun testParsingInfixExpressions() {
    data class TestInfix(
        val input: String,
        val leftValue: Any,
        val operator: String,
        val rightValue: Any
    )
    val inputs: List<TestInfix> =
        listOf(
            TestInfix("true == true", true, "==", true),
            TestInfix("true != false", true, "!=", false),
            TestInfix("false == false", false, "==", false),
        )

    for (input in inputs) {
      val lexer = Lexer(input.input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)
      assertEquals(1, program.statements.size)
      assertTrue(program.statements[0] is ExpressionStatement)
      val expState = program.statements[0] as ExpressionStatement
      assertInfixExpression(expState.expression, input.leftValue, input.operator, input.rightValue)
    }
  }
  @Test
  fun testParsingPrefixExpressions() {
    data class TestInfix(val input: String, val operator: String, val rightValue: Any)
    val inputs: List<TestInfix> =
        listOf(
            TestInfix("!true;", "!", true),
            TestInfix("!false;", "!", false),
        )

    for (input in inputs) {
      val lexer = Lexer(input.input)
      val parser = Parser(lexer)
      val program = parser.parseProgram()
      checkParserErrors(parser)
      assertEquals(1, program.statements.size)
      val statement = program.statements[0]
      assertTrue(statement is ExpressionStatement)
      assertTrue(statement.expression is PrefixExpression)
      assertPrefixExpression(statement.expression, input.operator, input.rightValue)
    }
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
  @Test
  fun testIfExpression() {
    val input = """
    if (x < y) { x } else { y }
    """
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    checkParserErrors(parser)
    assertEquals(1, program.statements.size)
    val statement = program.statements[0]
    assertTrue(statement is ExpressionStatement)
    assertTrue(statement.expression is IfExpression)
    val ifExp = statement.expression as IfExpression
    assertInfixExpression(ifExp.condition, "x", "<", "y")
    assertEquals(1, ifExp.concequence.statements.size)
    val statementConc = ifExp.concequence.statements[0]
    assertTrue(statementConc is ExpressionStatement)
    assertIdentifer(statementConc.expression, "x")
  }

  fun assertPrefixExpression(expression: Expression, operator: String, right: Any) {
    assertTrue(expression is PrefixExpression)
    val inf = expression as PrefixExpression
    assertEquals(operator, inf.operator)
    assertLiteralExpression(inf.right, right)
  }

  fun assertInfixExpression(expression: Expression, left: Any, operator: String, right: Any) {
    assertTrue(expression is InfixExpression)
    val inf = expression as InfixExpression
    assertLiteralExpression(inf.left, left)
    assertEquals(operator, inf.operator)
    assertLiteralExpression(inf.right, right)
  }
  fun assertLiteralExpression(expression: Expression, expected: Any) {
    val a =
        when (expected) {
          is Int -> assertIntegerLiteral(expression, (expected).toLong())
          is Long -> assertIntegerLiteral(expression, expected)
          is String -> assertIdentifer(expression, expected)
          is Boolean -> assertBooleanLiteral(expression, expected)
          else -> throw AssertionError("Could not find expression for $expression")
        }
    return a
  }

  fun assertIdentiferExpressionStatement(statement: Statement, name: String) {
    assertTrue(statement is ExpressionStatement)
    val expressionStatement = statement as ExpressionStatement
    assertIdentifer(expressionStatement.expression, name)
  }

  fun assertIntegerLiteral(expression: Expression, value: Long) {
    assertTrue(expression is IntegerLiteral)
    assertEquals(value, expression.value)
  }

  fun assertBooleanLiteral(expression: Expression, value: Boolean) {
    assertTrue(expression is BooleanLiteral)
    assertEquals(value, expression.value)
  }
  fun assertIdentifer(expression: Expression, value: String) {
    assertTrue(expression is Identifier)
    assertEquals(value, expression.value)
    assertEquals(value, expression.tokenLiteral())
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
    var builder = StringBuilder()
    for (error in errors) {
      builder.append("$error\n")
    }
    throw AssertionError("parser has ${errors.size} errors: ${builder.toString()}")
    assertEquals(errors.size, 0)
  }
}
