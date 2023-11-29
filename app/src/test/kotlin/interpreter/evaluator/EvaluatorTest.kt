package interpreter.evaluator

import interpreter.Lexer
import interpreter.obj.BooleanObj
import interpreter.obj.IntegerObj
import interpreter.obj.MonkeyObject
import interpreter.parser.Parser
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

internal class EvaluatorTest {

  @Test
  fun testEvalIntegerLiteral() {
    data class TestStructure(val input: String, val expected: Long)
    val tests = listOf(TestStructure("5", 5), TestStructure("10", 10))

    for (test in tests) {
      val obj = testEval(test.input)
      assertNotNull(obj)
      assertIntegerObject(obj, test.expected)
    }
  }

  @Test
  fun testBooleanExpression() {
    data class TestStructure(val input: String, val expected: Boolean)
    val tests = listOf(TestStructure("true", true), TestStructure("false", false))

    for (test in tests) {
      val obj = testEval(test.input)
      assertNotNull(obj)
      assertBooleanObject(obj, test.expected)
    }
  }
  fun testEval(input: String): interpreter.obj.MonkeyObject? {
    val lexer = Lexer(input)
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    return Eval(program)
  }
  fun assertIntegerObject(obj: MonkeyObject, expected: Long) {
    assertTrue(obj is IntegerObj)
    assertEquals(expected, obj.value)
  }
  fun assertBooleanObject(obj: MonkeyObject, expected: Boolean) {
    assertTrue(obj is BooleanObj):q
    assertEquals(expected, obj.value)
  }
}
