package lambda.frontend

import caos.frontend.Configurator
import Configurator.*
import caos.view.{Code, Mermaid, Text}
import lambda.backend.*
import lambda.syntax.{Program, Show}
import Program.Term

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[Term]:
  val name = "Animator of a simple lambda calculus language"
  override val languageName: String = "Lambda Calculus with addition"

  val parser =
    lambda.syntax.Parser.parseProgram

  val examples = List(
    "succ" ->
      "(\\x -> x + 1) 2" ->
      "Adds 1 to number 2",
    "inf-strict" ->
      "((\\x -> \\y -> y)\\n  ((\\x -> x x) (\\x -> x x)))\n  1" ->
      "Example that never terminates with a strict evaluation, but terminates with lazy evaluation.",
    "omega" ->
      "(\\x -> x x) (\\y -> y y)" -> "infinite beta reductions",
    "non-determ" ->
      "((\\x -> x + 1) 2) \n+ \n((\\y -> y + 1) 3)" -> "non-determinism when evaluating term",
    "if0" ->
      "(\\n -> if0 n 1\n   ((\\x -> (x x)) (\\x -> (x x)))\n) (2+2)" ->
      "Example with infinite beta reductions inside a reduceable term, borrowed from https://redex.racket-lang.org/lam-v.html.",
    "triangle" ->
      "((\\le -> \n   ((\\f -> (le (\\x -> ((f f) x))))\n    (\\f -> (le (\\x -> ((f f) x))))))\n (\\triangle ->\n   (\\x ->\n     (if0 x\n          0\n          (x + (triangle (x + (-1))))))))" ->
      "Triangle example taken from the SEwPR book - https://redex.racket-lang.org/lam-v.html",
    "find-bisim" ->
      "((\\y->y y) ((\\x -> x + 1) 2))\n// is bisimlar to\n((\\y->y y) ((\\x -> 5 + x) 27))" ->
      "Simple example to verify if two terms are bisimilar"
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
    "View parsed data" -> view(_.toString, Text).moveTo(1),
    "View pretty data" -> view(Show(_), Code("haskell")).moveTo(1),
    "Diagram of the structure" -> view(Show.mermaid, Mermaid).moveTo(1),
    "Run semantics" -> steps(e=>e, Semantics, Show(_), Text),
    "Run semantics (with diagrams)" -> steps(e=>e, Semantics, Show.mermaid, Mermaid),
    "Build LTS" -> lts(e=>e, Semantics, Show(_)),
    "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, Show(_)),
    "Build LTS - Lazy Evaluation" -> lts(e=>e, LazySemantics, Show(_)),
    "Build LTS - Strict Evaluation" -> lts(e=>e, StrictSemantics, Show(_)),
    "Find bisimulation: given 'A B', check if 'A ~ B'" ->
      compareBranchBisim(Semantics,Semantics,getApp(_).e1,getApp(_).e2,Show(_),Show(_)),
  )

  /** Auxiliar function that casts a generic term into a Term.App */
  def getApp(t: Term): Term.App = t match
    case a: Term.App => a
    case _ => sys.error("Input must be an application \"A B\" to compare \"A\" and \"B\".")

  /** Custom footer message */
  override val footer =
    """Source code at: <a href="https://github.com/arcalab/lambda-caos" target="#">https://github.com/arcalab/lambda-caos</a>.
      |This is minimal example to illustrate how to use the
      |<a target="_blank" href="https://github.com/arcalab/CAOS">CAOS</a>
      |libraries to generate a web frontend.""".stripMargin

  override val documentation = List(
    languageName
      -> "Press to read more about the input language." ->
      """The input language is a simple lambda term, borrowed from
        |<a target="_blank" href="https://redex.racket-lang.org/lam-v.html">
        |https://redex.racket-lang.org/lam-v.html</a> for Racket,
        |given by the following grammar:
        |<pre>
        |  term ::= variable
        |         | term term
        |         | \ variable -> term
        |         | integer
        |         | term + term
        |         | if0 term term term
        |</pre>""".stripMargin,
    "Run semantics"
      -> "Press to read more about the semantics of the language." ->
      """<p>The semantics of this lambda calculus is the traditional one,
        | allowing alpha-reductions and beta-reductions to any of its sub-terms,
        | enriched with rules to reduce the sum of two integers and to check if
        | a given term is the integer value 0.</p>
        |<p>Semantics borrowed from
        |<a target="_blank" href="https://redex.racket-lang.org/lam-v.html">
        |https://redex.racket-lang.org/lam-v.html</a> for Racket</p>""".stripMargin,
  )
