package lambda.backend

import caos.sos.SOS
import lambda.backend.LazySemantics.St
import lambda.syntax.Program.Term
import lambda.syntax.Program.Term.*
import lambda.syntax.{Program, Show}

/** Strict evaluation of Lambda terms.  */
object StrictSemantics extends SOS[String,St]:

  type St = Term

  /** When is a state terminal: there are no states marked as terminal */
  override def accepting(s: St): Boolean = false

  /** What are the set of possible evolutions (label and new state) */
  override def next[A>:String](st: St): Set[(A, St)] = st match
    case Var(_) => Set() // cannot evolve
    case App(e1, e2) => // evolve by e2, if fail try to evolve by e1, if fail, check for a beta reduction
      val e2N = next(e2)
      if e2N.nonEmpty
      then Set(e2N.head._1 -> App(e1,e2N.head._2))
      else
        val e1N = next(e1)
        if e1N.nonEmpty
        then Set(e1N.head._1 -> App(e1N.head._2,e2))
        else e1 match
          case Lam(x,e3) => Set(s"beta-$x" -> Semantics.subst(e3,x,e2))
          case _ => Set()
    case Lam(x, e) => // evolve by e
      for (by, to) <- next(e) yield by -> Lam(x, to)
    case Val(_) => Set()
    case Add(Val(n1),Val(n2)) => Set("+" -> Val(n1+n2))
    case Add(e1, e2) => // evolve by e1, if fail try by e2
      val e2N = next(e2)
      if e2N.nonEmpty
      then Set(e2N.head._1 -> Add(e1, e2N.head._2))
      else for (by, to) <- next(e1) yield by -> Add(to, e2)
    case If0(e1,e2,e3) => // evolve by e2, if fail try by e3, if fail try by e1, otherwise check if e1 is a value
      val e2N = next(e2)
      if e2N.nonEmpty
      then Set(e2N.head._1 -> If0(e1,e2N.head._2,e3))
      else
        val e3N = next(e3)
        if e3N.nonEmpty
        then Set(e3N.head._1 -> If0(e1,e2,e3N.head._2))
        else
          val e1N = next(e1)
          if e1N.nonEmpty
          then Set(e1N.head._1 -> If0(e1N.head._2, e2, e3))
          else e1 match
            case Val(0) => Set("if0-true" -> e2)
            case Val(_) => Set("if0-false" -> e3)
            case _ => Set()



