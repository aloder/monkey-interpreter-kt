package interpreter.parser

import interpreter.Lexer
import interpreter.Token
import interpreter.TokenType
import interpreter.ast.BlockStatement
import interpreter.ast.BooleanLiteral
import interpreter.ast.CallExpression
import interpreter.ast.Expression
import interpreter.ast.ExpressionStatement
import interpreter.ast.FunctionLiteral
import interpreter.ast.Identifier
import interpreter.ast.IfExpression
import interpreter.ast.InfixExpression
import interpreter.ast.IntegerLiteral
import interpreter.ast.LetStatement
import interpreter.ast.PrefixExpression
import interpreter.ast.Program
import interpreter.ast.Statement
import kotlin.collections.ArrayList
import kotlin.collections.mutableListOf

enum class PRIORITY(val order: Int) {
  LOWEST(1),
  EQUALS(2),
  LESSGREATER(3),
  SUM(4),
  PRODUCT(5),
  PREFIX(6),
  CALL(7)
}

public class Parser(val lexer: Lexer) {
  var curToken: Token = Token(TokenType.ILLEGAL, "")
  var peekToken: Token = Token(TokenType.ILLEGAL, "")
  private val errors = arrayListOf<String>()
  private val prefixParseFns: Map<TokenType, () -> Expression?> =
      mapOf(
          Pair(TokenType.IDENT, this::parseIdentifier),
          Pair(TokenType.IF, this::parseIfExpression),
          Pair(TokenType.INT, ::parseIntegerLiteral),
          Pair(TokenType.BANG, ::parsePrefixExpression),
          Pair(TokenType.TRUE, ::parseBoolean),
          Pair(TokenType.FALSE, ::parseBoolean),
          Pair(TokenType.LPAREN, ::parseGroupedExpression),
          Pair(TokenType.MINUS, ::parsePrefixExpression),
          Pair(TokenType.FUNCTION, ::parseFunctionLiteral),
      )
  private val precedences: Map<TokenType, PRIORITY> =
      mapOf(
          Pair(TokenType.EQ, PRIORITY.EQUALS),
          Pair(TokenType.NOT_EQ, PRIORITY.EQUALS),
          Pair(TokenType.LT, PRIORITY.LESSGREATER),
          Pair(TokenType.GT, PRIORITY.LESSGREATER),
          Pair(TokenType.PLUS, PRIORITY.SUM),
          Pair(TokenType.MINUS, PRIORITY.SUM),
          Pair(TokenType.SLASH, PRIORITY.PRODUCT),
          Pair(TokenType.LPAREN, PRIORITY.CALL),
          Pair(TokenType.ASTERISK, PRIORITY.PRODUCT),
      )
  private val infixParseFns: Map<TokenType, (Expression) -> Expression?> =
      mapOf(
          Pair(TokenType.PLUS, ::parseInfixExpression),
          Pair(TokenType.MINUS, ::parseInfixExpression),
          Pair(TokenType.SLASH, ::parseInfixExpression),
          Pair(TokenType.ASTERISK, ::parseInfixExpression),
          Pair(TokenType.EQ, ::parseInfixExpression),
          Pair(TokenType.NOT_EQ, ::parseInfixExpression),
          Pair(TokenType.LT, ::parseInfixExpression),
          Pair(TokenType.LPAREN, ::parseCallExpression),
          Pair(TokenType.GT, ::parseInfixExpression),
      )

  init {
    // iterate so both the curToken and the peek token are populated
    nextToken()
    nextToken()
  }

  fun nextToken() {
    curToken = peekToken
    peekToken = lexer.nextToken()
  }

  public fun parseProgram(): interpreter.ast.Program {
    val program = Program()
    while (curToken.type != TokenType.EOF) {
      val statement = parseStatment()
      if (statement != null) {
        program.statements.add(statement)
      }
      nextToken()
    }

    return program
  }
  public fun getErrors(): ArrayList<String> {
    return errors
  }
  private fun parseStatment(): interpreter.ast.Statement? {
    return when (curToken.type) {
      TokenType.LET -> parseLetStatement()
      TokenType.RETURN -> parseReturnStatement()
      else -> parseExpressionStatement()
    }
  }
  private fun parseReturnStatement(): interpreter.ast.ReturnStatement? {
    val token = curToken
    nextToken()
    val value = parseExpression(PRIORITY.LOWEST)
    if (value == null) {
      return null
    }
    while (curToken.type != TokenType.SEMICOLON) {
      nextToken()
    }
    return interpreter.ast.ReturnStatement(token, value)
  }

  private fun parseLetStatement(): LetStatement? {
    val token: Token = curToken
    if (!expectPeek(TokenType.IDENT)) return null

    val identifier = parseIdentifier()

    if (!expectPeek(TokenType.ASSIGN)) return null

    nextToken()

    val value = parseExpression(PRIORITY.LOWEST)
    if (value == null) {
      return null
    }

    while (curToken.type != TokenType.SEMICOLON) {
      nextToken()
    }
    return LetStatement(token, identifier, value)
  }

  private fun parseExpressionStatement(): ExpressionStatement? {
    val localCurToken = curToken

    val expression = parseExpression(PRIORITY.LOWEST)
    if (expression == null) {
      return null
    }

    val ret = ExpressionStatement(localCurToken, expression)
    if (peekTokenIs(TokenType.SEMICOLON)) {
      nextToken()
    }
    return ret
  }

  private fun parseIntegerLiteral(): Expression? {
    val localCurToken = curToken
    val value = localCurToken.literal.toLongOrNull()
    if (value == null) {
      errors.add("Could not parse ${localCurToken.literal} as integer")
      return null
    }

    return IntegerLiteral(localCurToken, value)
  }

  private fun parseBoolean(): Expression {
    return BooleanLiteral(curToken, curTokenIs(TokenType.TRUE))
  }

  private fun parseInfixExpression(left: Expression): Expression? {
    val localCurToken = curToken
    val localPrecedence = curPrecedence()

    nextToken()
    val right = parseExpression(localPrecedence)
    if (right == null) {
      return null
    }
    return InfixExpression(localCurToken, left, localCurToken.literal, right)
  }

  private fun parsePrefixExpression(): Expression? {
    val localCurToken = curToken
    nextToken()
    val expression = parseExpression(PRIORITY.PREFIX)
    if (expression == null) {
      return null
    }
    return PrefixExpression(localCurToken, localCurToken.literal, expression)
  }

  private fun parseIfExpression(): Expression? {
    val localCurToken = curToken

    if (!expectPeek(TokenType.LPAREN)) {
      return null
    }

    nextToken()
    val expression = parseExpression(PRIORITY.LOWEST)

    if (expression == null) {
      return null
    }

    if (!expectPeek(TokenType.RPAREN)) {
      return null
    }

    if (!expectPeek(TokenType.LBRACE)) {
      return null
    }
    val consequence = parseBlockStatement()
    var alt: BlockStatement? = null
    if (peekTokenIs(TokenType.ELSE)) {
      nextToken()

      if (!expectPeek(TokenType.LBRACE)) {
        return null
      }
      alt = parseBlockStatement()
    }
    return IfExpression(localCurToken, expression, consequence, alt)
  }

  private fun parseBlockStatement(): BlockStatement {
    val localToken = curToken
    nextToken()
    val lst: MutableList<Statement> = mutableListOf()
    while (!curTokenIs(TokenType.RBRACE) && !curTokenIs(TokenType.EOF)) {
      val stmt = parseStatment()
      if (stmt != null) {
        lst.add(stmt)
      }
      nextToken()
    }
    return BlockStatement(localToken, lst)
  }

  private fun parseCallExpression(function: Expression): Expression {
    val localToken = curToken
    return CallExpression(localToken, function, parseCallArguments())
  }

  private fun parseCallArguments(): List<Expression> {
    val a: MutableList<Expression> = mutableListOf()
    if (peekTokenIs(TokenType.RPAREN)) {
      nextToken()
      return a
    }
    nextToken()
    val exp = parseExpression(PRIORITY.LOWEST)
    if (exp != null) {
      a.add(exp)
    }

    while (peekTokenIs(TokenType.COMMA)) {
      nextToken()
      nextToken()
      val exp2 = parseExpression(PRIORITY.LOWEST)
      if (exp2 != null) {
        a.add(exp2)
      }
    }
    if (expectPeek(TokenType.RPAREN)) {
      return a
    }
    return a
  }
  private fun parseGroupedExpression(): Expression? {
    nextToken()
    val exp = parseExpression(PRIORITY.LOWEST)

    if (!expectPeek(TokenType.RPAREN)) {
      return null
    }
    return exp
  }

  private fun parseExpression(precidence: PRIORITY): Expression? {
    val prefix = prefixParseFns.get(curToken.type)
    if (prefix == null) {
      noPrefixParseFnError(curToken.type)
      return null
    }
    var leftExp = prefix.invoke()
    while (!peekTokenIs(TokenType.SEMICOLON) && precidence.order < peekPrecedence().order) {
      val infix = infixParseFns.get(peekToken.type)
      if (infix == null) {
        return leftExp
      }

      nextToken()
      if (leftExp == null) {
        return null
      }
      val res = infix.invoke(leftExp)
      if (res == null) {
        return leftExp
      }
      leftExp = res
    }
    return leftExp
  }

  private fun parseFunctionLiteral(): Expression? {
    val localCurToken = curToken

    if (!expectPeek(TokenType.LPAREN)) {
      return null
    }
    val parameters = parseFunctionParameters()

    if (!expectPeek(TokenType.LBRACE)) {
      return null
    }

    val body = parseBlockStatement()
    return FunctionLiteral(localCurToken, parameters, body)
  }
  private fun parseFunctionParameters(): List<Identifier> {
    val list: MutableList<Identifier> = mutableListOf()

    if (peekTokenIs(TokenType.RPAREN)) {
      nextToken()
      return list
    }
    nextToken()
    list.add(parseIdentifier())
    while (peekTokenIs(TokenType.COMMA)) {
      nextToken()
      nextToken()
      list.add(parseIdentifier())
    }
    if (!expectPeek(TokenType.RPAREN)) {
      return list
    }

    return list
  }

  fun noPrefixParseFnError(type: TokenType) {
    errors.add("There is no functin prefix for $type")
    return
  }

  fun parseIdentifier(): Identifier {
    return Identifier(curToken, curToken.literal)
  }
  private fun peekPrecedence(): PRIORITY {
    return precedences.getOrDefault(peekToken.type, PRIORITY.LOWEST)
  }
  private fun curPrecedence(): PRIORITY {
    return precedences.getOrDefault(curToken.type, PRIORITY.LOWEST)
  }
  private fun expectPeek(expected: TokenType): Boolean {
    if (peekTokenIs(expected)) {
      nextToken()
      return true
    } else {
      peekError(expected)
      return false
    }
  }
  private fun peekError(tokenType: TokenType) {
    errors.add("expected next token to be $tokenType, got ${peekToken.type} instead")
  }
  private fun peekTokenIs(tokenType: TokenType): Boolean {
    if (peekToken.type == tokenType) {
      return true
    } else {
      return false
    }
  }
  private fun curTokenIs(tokenType: TokenType): Boolean {
    return curToken.type == tokenType
  }
}
