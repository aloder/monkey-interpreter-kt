package interpreter.repl

import interpreter.Lexer
import interpreter.TokenType
import java.io.PrintStream
import java.util.Scanner

val PROMPT = ">> "

public fun startRepl(scanner: Scanner, out: PrintStream) {
  while (true) {
    out.print(PROMPT)
    while (!scanner.hasNextLine()) {
      Thread.sleep(100)
    }
    var line = scanner.nextLine()
    if (line == null) {
      out.println()
      break
    }
    val lexer = Lexer(line)
    while (true) {
      val token = lexer.nextToken()
      if (token.type == TokenType.EOF) {
        break
      }
      out.println(token)
    }
  }
}
