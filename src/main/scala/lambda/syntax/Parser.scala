package lambda.syntax

import cats.parse.{LocationMap, Parser as P, Parser0 as P0}
import cats.parse.Numbers.*
import cats.syntax.all.*
import P.*
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp
import lambda.syntax.Program.Term
import lambda.syntax.Program.Term.*

import scala.sys.error

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):Term =
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A],str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  def prettyError(str:String,err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-" * (y+1))+"^\n"}""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  def symbols: P[String] = // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as sybmols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string


  /** A program is a command with possible spaces or comments around. */
  def program: P[Term] = expression.surroundedBy(sps)

  /** (Recursive) Parser for a command in the while language */
  def expression: P[Term] = P.recursive(exprRec =>
    def lit:P[Term] = P.recursive(litRec =>// lambda, int, or var
      ((char('\\')~sps)*>varName~(string("->").surroundedBy(sps)*>exprRec))
        .map(x => Lam(x._1,x._2)) |
      (char('-')*>digits).map(x=>Val(0-x.toInt)) |
      digits.map(x=>Val(x.toInt)) |
      (char('(')*>exprRec.surroundedBy(sps)<*char(')')) |
      ((string("if0")~sps)*>(litRec<*sps)~(litRec<*sps)~litRec)
        .map(x => If0(x._1._1,x._1._2,x._2)) |
      varName.map(Var.apply)
    )
    def rest:P[Term => Term] =
      ((char('+')~sps)*>exprRec)
        .map(e2 => (e1 => Add(e1,e2))) |
      exprRec
        .map(e2 => (e1 => App(e1,e2)))

    (lit ~ (sps*>rest.?))
        .map(x => x._2.getOrElse(y=>y)(x._1))

  )


  /// Auxiliary parser combinators

  /** Non-empty list of elements with a binary operator */
  def listSep[A](elem:P[A],op:P[(A,A)=>A]): P[A] =
    (elem ~ (op.surroundedBy(sps).backtrack~elem).rep0)
      .map(x=>
        val pairlist = x._2
        val first = x._1;
        pairlist.foldLeft(first)((rest,pair) => pair._1(rest,pair._2))
      )

  /** Pair of elements with a separator */
  def binary[A,B](p1:P[A],op:String,p2:P[B]): P[(A,B)] =
    (p1 ~ string(op).surroundedBy(sps) ~ p2).map(x=>(x._1._1,x._2))


  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////
  object Examples:
    val ex1 =
      """x:=28; while(x>1) do x:=x-1"""
