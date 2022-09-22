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
    case App(e1, e2) => s"(${apply(e1)}) (${apply(e2)})"
    case Lam(x, e) => s"\\$x -> ${apply(e)}"
    case Val(n) => n.toString
    case Add(e1, e2) => s"(${apply(e1)}) + (${apply(e2)})"
    case If0(e1,e2,e3) => s"if (${apply(e1)})==0 then (${apply(e2)}) else (${apply(e3)})"

  /** Converts a lambda term into a mermaid diagram reflecting its structure. */
  def mermaid(e:Term): String = "graph TD\n"+term2merm(e)
  private def term2merm(e: Term): String = e match
    case Var(x) => s"  ${e.hashCode()}([\"$x\"])"
    case App(e1, e2) =>
      s"  ${e.hashCode()}([\"${Show(e)}\"])  \n${
        term2merm(e1)}\n${term2merm(e2)}\n  ${
        e.hashCode()} -->|app-left| ${e1.hashCode()}\n  ${
        e.hashCode()} -->|app-right| ${e2.hashCode()}"
    case Lam(x, e1) =>
      s"  ${e.hashCode()}([\"${Show(e)}\"])  \n${
        term2merm(Var(x))}\n${term2merm(e1)}\n  ${
        e.hashCode()} -->|lam-var| ${Var(x).hashCode()}\n  ${
        e.hashCode()} -->|lam-term| ${e1.hashCode()}"
    case Val(n) =>  s"  ${e.hashCode()}([\"$n\"])"
    case Add(e1, e2) =>
      s"  ${e.hashCode()}([\"${Show(e)}\"])  \n${
        term2merm(e1)}\n${term2merm(e2)}\n  ${
        e.hashCode()} -->|+left| ${e1.hashCode()}\n  ${
        e.hashCode()} -->|+right| ${e2.hashCode()}"
    case If0(e1, e2, e3) =>
      s"  ${e.hashCode()}([\"${Show(e)}\"])  \n${
        term2merm(e1)}\n${term2merm(e2)}\n${term2merm(e3)}\n  ${
        e.hashCode()} -->|if-cond| ${e1.hashCode()}\n  ${
        e.hashCode()} -->|if-true| ${e2.hashCode()}\n  ${
        e.hashCode()} -->|if-false| ${e3.hashCode()}"


/*
graph TD
    A([Christmas]) -->|Get money| B(Go shopping)
    B --> C{Let me think}
    C -->|One| D[Laptop]
    C -->|Two| E[iPhone]
    C -->|Three| F[fa:fa-car Car]
*/