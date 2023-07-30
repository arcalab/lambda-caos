package lambda.syntax

import lambda.syntax.Program
import Program.Term
import Term.*



/**
 * List of functions to produce textual representations of commands
 */
object Show:
  /** Pretty expression */
  def apply(e: Term): String = e match
    case Var(x) => x
    case App(e1, e2) => s"${applyP(e1)} ${applyP(e2)}"
    case Lam(x, e) => s"\\$x -> ${apply(e)}"
    case Val(n) => n.toString
    case Add(e1, e2) => s"${applyP(e1)} + ${applyP(e2)}"
    case If0(e1,e2,e3) => s"if ${applyP(e1)}==0 then ${applyP(e2)} else ${applyP(e3)}"

  private def applyP(e: Term): String = e match
    case _:(Var|Val) => apply(e)
    case _ => s"(${apply(e)})"

  /** Converts a lambda term into a mermaid diagram reflecting its structure. */
  def mermaid(e: Term): String = "graph TD\n" + term2merm(e).mkString("\n")
  /** Builds nodes and arcs, using a set structure to avoid repetition. */
  private def term2merm(e: Term): Set[String] = e match
    case Var(x) => Set(s"  ${e.hashCode()}([\"$x\"])")
    case App(e1, e2) =>
      Set(s"  ${e.hashCode()}([\"${Show(e)}\"])") ++
      term2merm(e1) ++
      term2merm(e2) ++
      Set(s"  ${e.hashCode()} -->|app-left| ${e1.hashCode()}",
          s"  ${e.hashCode()} -->|app-right| ${e2.hashCode()}")
    case Lam(x, e1) =>
      Set(s"  ${e.hashCode()}([\"${Show(e)}\"])") ++
      term2merm(Var(x)) ++
      term2merm(e1) ++
      Set(s"  ${e.hashCode()} -->|lam-var| ${Var(x).hashCode()}",
          s"  ${e.hashCode()} -->|lam-term| ${e1.hashCode()}")
    case Val(n) => Set(s"  ${e.hashCode()}([\"$n\"])")
    case Add(e1, e2) =>
      Set(s"  ${e.hashCode()}([\"${Show(e)}\"])") ++
      term2merm(e1) ++
      term2merm(e2) ++
      Set(s"  ${e.hashCode()} -->|+left| ${e1.hashCode()}",
          s"  ${e.hashCode()} -->|+right| ${e2.hashCode()}")
    case If0(e1, e2, e3) =>
      Set(s"  ${e.hashCode()}([\"${Show(e)}\"])") ++
      term2merm(e1) ++ term2merm(e2) ++ term2merm(e3) ++
      Set(s"  ${e.hashCode()} -->|if-cond| ${e1.hashCode()}",
          s"  ${e.hashCode()} -->|if-true| ${e2.hashCode()}",
          s"  ${e.hashCode()} -->|if-false| ${e3.hashCode()}")

/*
graph TD
    A([Christmas]) -->|Get money| B(Go shopping)
    B --> C{Let me think}
    C -->|One| D[Laptop]
    C -->|Two| E[iPhone]
    C -->|Three| F[fa:fa-car Car]
*/