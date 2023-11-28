package interpreter.evaluator

import interpreter.ast.ExpressionStatement
import interpreter.ast.IntegerLiteral
import interpreter.ast.Node
import interpreter.ast.Program
import interpreter.ast.Statement
import interpreter.obj.IntegerObj
import interpreter.obj.MonkeyObject

public fun Eval(node: Node): MonkeyObject? {
  val v =
      when (node) {
        is Program -> EvalStatements(node.statements)
        is ExpressionStatement -> Eval(node.expression)
        is IntegerLiteral -> IntegerObj(node.value)
        else -> null
      }

  return v
}

public fun EvalStatements(stmts: List<Statement>): MonkeyObject? {
  return stmts.map { Eval(it) }.last()
}
