package interpreter.parser

import interpreter.Lexer
import interpreter.Token
import interpreter.TokenType
import interpreter.ast.Identifier
import interpreter.ast.LetStatement
import interpreter.ast.Program
import kotlin.collections.ArrayList

public class Parser(val lexer: Lexer) {
  var curToken: Token = Token(TokenType.ILLEGAL, "")
  var peekToken: Token = Token(TokenType.ILLEGAL, "")
  private val errors = arrayListOf<String>()

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
      else -> null
    }
  }
  private fun parseReturnStatement(): interpreter.ast.ReturnStatement? {
    val token = curToken
    nextToken()
    // TODO: skipping expressions for now
    while (curToken.type != TokenType.SEMICOLON) {
      nextToken()
    }
    return interpreter.ast.ReturnStatement(token, Identifier(token, token.literal))
  }

  private fun parseLetStatement(): LetStatement? {
    val token: Token = curToken
    if (!expectPeek(TokenType.IDENT)) return null

    val identifier = parseIdentifier()

    if (!expectPeek(TokenType.ASSIGN)) return null

    // TODO: skipping expressions for now
    while (curToken.type != TokenType.SEMICOLON) {
      nextToken()
    }
    return LetStatement(token, identifier, identifier)
  }

  fun parseIdentifier(): Identifier {
    return Identifier(curToken, curToken.literal)
  }
  private fun expectPeek(expected: TokenType): Boolean {
    if (peekTokenIs(expected)) {
      nextToken()
      return true
    } else {
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
      peekError(tokenType)
      return false
    }
  }
  private fun curTokenIs(tokenType: TokenType): Boolean {
    return curToken.type == tokenType
  }
}
