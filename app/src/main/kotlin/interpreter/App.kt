package interpreter

import interpreter.repl.startRepl
import java.io.PrintStream
import java.util.Scanner

fun main() {
  startRepl(Scanner(System.`in`), PrintStream(System.out))
}
