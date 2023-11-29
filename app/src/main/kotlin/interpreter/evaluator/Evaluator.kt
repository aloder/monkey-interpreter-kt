package interpreter.evaluator

import interpreter.ast.BooleanLiteral
import interpreter.ast.ExpressionStatement
import interpreter.ast.InfixExpression
import interpreter.ast.IntegerLiteral
import interpreter.ast.Node
import interpreter.ast.PrefixExpression
import interpreter.ast.Program
import interpreter.ast.Statement
import interpreter.obj.BooleanObj
import interpreter.obj.IntegerObj
import interpreter.obj.MonkeyObject
import interpreter.obj.NullObj

private val TRUE = BooleanObj(true)
private val FALSE = BooleanObj(false)
private val NULL = NullObj()

public fun Eval(node: Node): MonkeyObject? {
  val v =
      when (node) {
        is Program -> EvalStatements(node.statements)
        is ExpressionStatement -> Eval(node.expression)
        is IntegerLiteral -> IntegerObj(node.value)
        is BooleanLiteral -> if (node.value) TRUE else FALSE
        is PrefixExpression -> {
          val right = Eval(node.right)
          if (right == null) {
            return NULL
          }
          evalPrefixExpression(node.operator, right)
        }
        is InfixExpression -> {
          val left = Eval(node.left)
          val right = Eval(node.right)
          evalInfixExpression(node.operator, left, right)
        }
        else -> null
      }

  return v
}

public fun EvalStatements(stmts: List<Statement>): MonkeyObject? {
  return stmts.map { Eval(it) }.last()
}

private fun evalPrefixExpression(operator: String, right: MonkeyObject): MonkeyObject {
  return when (operator) {
    "!" -> evalBangOperatorExpression(right)
    "-" -> evalMinusPrefixOperatorExpression(right)
    else -> NULL
  }
}

private fun evalBangOperatorExpression(right: MonkeyObject): MonkeyObject {
  return when (right) {
    TRUE -> FALSE
    FALSE -> TRUE
    NULL -> TRUE
    else -> FALSE
  }
}

private fun evalMinusPrefixOperatorExpression(right: MonkeyObject): MonkeyObject {
  if (!(right is IntegerObj)) {
    return NULL
  }
  val value = right.value
  return IntegerObj(-value)
}

private fun evalInfixExpression(
    operator: String,
    left: MonkeyObject?,
    right: MonkeyObject?
): MonkeyObject {
  if (left == null || right == null) {
    return NULL
  }
  if (left is IntegerObj && right is IntegerObj) {
    return evalIntegerInfixExpression(operator, left, right)
  }
  return NULL
}

private fun evalIntegerInfixExpression(
    operator: String,
    left: MonkeyObject,
    right: MonkeyObject
): MonkeyObject {
  if (!(left is IntegerObj && right is IntegerObj)) {
    return NULL
  }
  val leftVal = left.value
  val rightVal = right.value
  return when (operator) {
    "+" -> IntegerObj(leftVal + rightVal)
    "-" -> IntegerObj(leftVal - rightVal)
    "*" -> IntegerObj(leftVal * rightVal)
    "/" -> IntegerObj(leftVal / rightVal)
    else -> NULL
  }
}
