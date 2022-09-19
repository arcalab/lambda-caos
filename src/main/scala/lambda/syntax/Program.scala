package lambda.syntax

/**
 * Internal structure to represent commands in a simple while language
 * @author José Proença
 */

object Program:

  /** A lambda expression */
  enum Expression:
    case Var(x:String)
    case App(e1:Expression, e2:Expression)
    case Lam(x:String, e:Expression)
    ///
    case Val(n:Int)
    case Add(e1:Expression, e2:Expression)
    case If0(e1:Expression, e2:Expression, e3:Expression)



  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////

  object Examples:
    import Program.Expression._

    val p1: Expression =
      App( Lam("x",App(Var("x"),Var("x"))) , Lam("y",Var("y")) )

