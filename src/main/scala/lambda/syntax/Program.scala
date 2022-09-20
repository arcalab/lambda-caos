package lambda.syntax

/**
 * Internal structure to represent commands in a simple while language
 * @author José Proença
 */

object Program:

  /** A lambda expression */
  enum Term:
    case Var(x:String)
    case App(e1:Term, e2:Term)
    case Lam(x:String, e:Term)
    ///
    case Val(n:Int)
    case Add(e1:Term, e2:Term)
    case If0(e1:Term, e2:Term, e3:Term)



  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////

  object Examples:
    import Program.Term._

    val p1: Term =
      App( Lam("x",App(Var("x"),Var("x"))) , Lam("y",Var("y")) )

