package lambda.backend

import caos.sos.SOS
import lambda.backend.Semantics.St
import lambda.syntax.Program
import lambda.syntax.Program.Term
import Term.*

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[String,St]:

  type St = Term

  /** When is a state terminal: there are no states marked as terminal */
  override def accepting(s: St): Boolean = false

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:String](st: St): Set[(A, St)] = st match
    case Var(_) => Set() // cannot evolve
    case App(e1, e2) => // evolve by e1, by e2, or by a beta reduction
      (for (by, to) <- next(e1) yield by -> App(to, e2)) ++
      (for (by, to) <- next(e2) yield by -> App(e1, to)) ++
      (e1 match
        case Lam(x,e) => Set(s"beta-$x" -> subst(e,x,e2))
        case _ => Set()
      )
    case Lam(x, e) => // evolve by e
      for (by, to) <- next(e) yield by -> Lam(x, to)
    case Val(_) => Set()
    case Add(e1, e2) => // evolve by e1, by e2, or by a sum
      (for (by, to) <- next(e1) yield by -> Add(to, e2)) ++
      (for (by, to) <- next(e2) yield by -> Add(e1, to)) ++
      ((e1,e2) match
        case (Val(n1),Val(n2)) => Set("+" -> Val(n1+n2))
        case _ => Set()
      )
    case If0(e1,e2,e3) =>
      (for (by, to) <- next(e1) yield by -> If0(to, e2, e3)) ++
      (for (by, to) <- next(e2) yield by -> If0(e1, to, e3)) ++
      (for (by, to) <- next(e3) yield by -> If0(e1, e2, to)) ++
      (e1 match
        case Val(0) => Set("if0-true" -> e2)
        case Val(_) => Set("if0-false" -> e3)
        case _ => Set()
        )


  def subst(e:Term, x:String, by:Term): Term = e match
      case Var(x2) => if x==x2 then by else e
      case App(e1, e2) => App(subst(e1,x,by),subst(e2,x,by))
      case Lam(x2, e1) => if x==x2 then e else Lam(x2,subst(e1,x,by))
      case Val(_) => e
      case Add(e1, e2) => Add(subst(e1,x,by),subst(e2,x,by))
      case If0(e1,e2,e3) => If0(subst(e1,x,by),subst(e2,x,by),subst(e3,x,by))



