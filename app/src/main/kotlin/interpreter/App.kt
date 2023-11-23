package interpreter

import interpreter.repl.startRepl
import java.io.PrintStream
import java.util.Scanner

fun main(args: Array<String>) {
  startRepl(Scanner(System.`in`), PrintStream(System.out))
}
