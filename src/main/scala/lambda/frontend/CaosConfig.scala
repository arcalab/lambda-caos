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


  val widgets = List(
    "View parsed data" -> view(_.toString, Text).moveTo(1),
    "View pretty data" -> view(Show.apply, Code("haskell")).moveTo(1),
    "Diagram of the structure" -> view(Show.mermaid, Mermaid).moveTo(1),
    "Run semantics" -> steps(e=>e, Semantics, e=>Show(e), Text),
    "Run semantics (with diagrams)" -> steps(e=>e, Semantics, Show.mermaid, Mermaid),
    "Build LTS" -> lts(e=>e, Semantics, x=>Show(x)),
    "Build LTS - Lazy Evaluation" -> lts(e=>e, LazySemantics, x=>Show(x)),
    "Build LTS - Strict Evaluation" -> lts(e=>e, StrictSemantics, x=>Show(x)),
    "Find bisimulation: given 'A B', check if 'A ~ B'" ->
      compareBranchBisim(Semantics,Semantics,getPair(_)._1,getPair(_)._2,show1=Show.apply,show2=Show.apply),
  )

  def getPair(t:Term): (Term,Term) = t match
    case Term.App(t1,t2) => (t1,t2)
    case _ => sys.error("Input must be an application \"A B\" to compare \"A\" and \"B\".")