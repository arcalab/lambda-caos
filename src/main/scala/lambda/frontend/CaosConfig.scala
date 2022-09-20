package lambda.frontend

import caos.common.Example
import caos.frontend.Configurator
import caos.frontend.Configurator.*
import caos.frontend.widgets.WidgetInfo
import caos.view.{Mermaid, Text, View, Code}
import lambda.backend.*
import lambda.syntax.Program.Term
import lambda.syntax.{Program, Show}

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Term]:
  val name = "Animator of a simple lambda calculus language"
  override val languageName: String = "Lambda Calculus with addition"

  val parser: String=>Term =
    lambda.syntax.Parser.parseProgram

  val examples = List(
    "succ" ->
      "(\\x -> x + 1) 2" ->
      "Adds 1 to number 2",
    "omega" ->
      "(\\x -> x x) (\\y -> y y)" -> "infinite beta reductions",
    "non-determ" ->
      "((\\x -> x + 1) 2) \n+ \n((\\y -> y + 1) 3)" -> "non-determinism when evaluating term",
    "if0" ->
      "(\\n -> if0 n 1\n   ((\\x -> (x x)) (\\x -> (x x)))\n) (2+2)" ->
      "Example with infinite beta reductions inside a reduceable term, borrowed from https://redex.racket-lang.org/lam-v.html.",
    "triangle" ->
      "((\\le -> \n   ((\\f -> (le (\\x -> ((f f) x))))\n    (\\f -> (le (\\x -> ((f f) x))))))\n (\\triangle ->\n   (\\x ->\n     (if0 x\n          0\n          (x + (triangle (x + (-1))))))))" ->
      "Triangle example taken from the SEwPR book - https://redex.racket-lang.org/lam-v.html"
  )

  val widgets = List(
    "View parsed data" -> view(_.toString , Text),
    "View pretty data" -> view(Show.apply , Text),
    "Run semantics" -> steps(
      e=>e, Semantics, e => Show(e), Text),
    "Build LTS" -> lts(
      e=>e, Semantics, x=>Show(x)),
    "Build LTS - Lazy Evaluation" -> lts(
      e => e, LazySemantics, x => Show(x)),
    "Build LTS - Strict Evaluation" -> lts(
      e => e, StrictSemantics, x => Show(x)),

  )