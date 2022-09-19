package lambda.syntax

import lambda.syntax.Program
import Program.Expression
import Expression.*



/**
 * List of functions to produce textual representations of commands
 */
object Show:
  /** Pretty expression */
  def apply(e: Expression): String = e match
    case Var(x) => x
    case App(e1, e2) => s"(${apply(e1)}) (${apply(e2)})"
    case Lam(x, e) => s"\\$x -> ${apply(e)}"
    case Val(n) => n.toString
    case Add(e1, e2) => s"(${apply(e1)}) + (${apply(e2)})"
    case If0(e1,e2,e3) => s"if (${apply(e1)})==0 then (${apply(e2)}) else (${apply(e3)})"