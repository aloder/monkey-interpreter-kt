package interpreter.repl

import interpreter.Lexer
import interpreter.evaluator.Eval
import interpreter.parser.Parser
import java.io.PrintStream
import java.util.Scanner
import kotlin.io.println

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
    val parser = Parser(lexer)
    val program = parser.parseProgram()
    if (parser.getErrors().size > 0) {
      printParserErrors(out, parser.getErrors())
      continue
    }
    val evaluated = Eval(program)
    out.println(evaluated?.inspect())
  }
}

private fun printParserErrors(out: PrintStream, errors: List<String>) {
  out.println("Woops! We ran into some monkey business here!")
  out.println(" parser errors: ")
  errors.forEach { out.println(it) }
}
