package interpreter

import interpreter.repl.startRepl
import java.util.Scanner

fun main(args: Array<String>) {
  startRepl(Scanner(System.`in`))
}
