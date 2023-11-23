package interpreter.repl

import interpreter.Lexer
import interpreter.TokenType
import java.util.Scanner

val PROMPT = ">> "

public fun startRepl(scanner: Scanner) {
  while (true) {
    print(PROMPT)
    while (!scanner.hasNextLine()) {
      Thread.sleep(100)
    }
    var line = scanner.nextLine()
    if (line == null) {
      println()
      break
    }
    val lexer = Lexer(line)
    while (true) {
      val token = lexer.nextToken()
      if (token.type == TokenType.EOF) {
        break
      }
      println(token)
    }
  }
}
