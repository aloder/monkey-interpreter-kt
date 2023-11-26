package interpreter.ast

import interpreter.Token

interface Node {
  fun tokenLiteral(): String
  override fun toString(): String
}

public class Program(public val statements: ArrayList<Statement> = arrayListOf()) : Node {

  override fun tokenLiteral(): String {
    if (statements.size > 0) {
      return statements[0].tokenLiteral()
    } else {
      return ""
    }
  }

  override fun toString(): String {
    val builder = StringBuilder()
    statements.forEach { builder.append(it.toString()) }
    return builder.toString()
  }
}

// [[ Statements ]]
interface Statement : Node {}

public class LetStatement(
    public val token: Token,
    public val name: Identifier,
    public val value: Expression
) : Statement {
  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return "${token.literal} ${name.toString()} = ${value.toString()};"
  }
}

public class ReturnStatement(public val token: Token, public val returnValue: Expression) :
    Statement {
  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return "${tokenLiteral()} ${returnValue.toString()};"
  }
}

public class ExpressionStatement(public val token: Token, public val expression: Expression) :
    Statement {
  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return "${expression.toString()}"
  }
}

// [[ Expressions ]]
interface Expression : Node {}

public class PrefixExpression(
    public val token: Token,
    public val operator: String,
    public val right: Expression
) : Expression {

  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return "($operator$right)"
  }
}

public class InfixExpression(
    public val token: Token,
    public val left: Expression,
    public val operator: String,
    public val right: Expression
) : Expression {

  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return "($left $operator $right)"
  }
}

public class Identifier(public val token: Token, public val value: String) : Expression {
  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return value
  }
}

// [[ Literals ]]
public class IntegerLiteral(public val token: Token, public val value: Long) : Expression {

  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return token.literal
  }
}

public class BooleanLiteral(public val token: Token, public val value: Boolean) : Expression {
  override fun tokenLiteral(): String {
    return token.literal
  }

  override fun toString(): String {
    return token.literal
  }
}
