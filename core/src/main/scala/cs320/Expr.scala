package cs320

import scala.util.parsing.combinator._

sealed trait Expr

// variable
case class Id(name: String) extends Expr // done
// integer
case class IntE(value: BigInt) extends Expr // done
// boolean
case class BooleanE(value: Boolean) extends Expr // done
// addition
case class Add(left: Expr, right: Expr) extends Expr // done
// multiplication
case class Mul(left: Expr, right: Expr) extends Expr // done
// division
case class Div(left: Expr, right: Expr) extends Expr // done
// modulo
case class Mod(left: Expr, right: Expr) extends Expr // done
// equal-to
case class Eq(left: Expr, right: Expr) extends Expr // done
// less-then
case class Lt(left: Expr, right: Expr) extends Expr // done
// conditional
case class If(condition: Expr, trueBranch: Expr, falseBranch: Expr)
    extends Expr // done
// tuple
case class TupleE(expressions: List[Expr]) extends Expr // done
// projection
case class Proj(expression: Expr, index: Int) extends Expr // done
// nil
case object NilE extends Expr // done
// cons
case class ConsE(head: Expr, tail: Expr) extends Expr // done
// is-empty
case class Empty(expression: Expr) extends Expr // done
// head
case class Head(expression: Expr) extends Expr // done
// tail
case class Tail(expression: Expr) extends Expr // done
// local variable
case class Val(name: String, expression: Expr, body: Expr) extends Expr // done
// anonymous function
case class Fun(parameters: List[String], body: Expr) extends Expr // done
// recursive function
case class RecFuns(functions: List[FunDef], body: Expr) extends Expr // not done
// function application
case class App(function: Expr, arguments: List[Expr]) extends Expr // done
// type test
case class Test(expression: Expr, typ: Type) extends Expr // not done

case class FunDef(name: String, parameters: List[String], body: Expr)

sealed trait Type
case object IntT extends Type
case object BooleanT extends Type
case object TupleT extends Type
case object ListT extends Type
case object FunctionT extends Type

case class ParsingError(msg: String) extends Exception

object Expr extends RegexParsers {

  private def error(msg: String): Nothing = throw ParsingError(msg)

  private def wrapR[T](e: Parser[T]): Parser[T] = "(" ~> e <~ ")"
  private def wrapC[T](e: Parser[T]): Parser[T] = "{" ~> e <~ "}"
  private def wrapS[T](e: Parser[T]): Parser[T] = "[" ~> e <~ "]"

  val keywords = Set("true", "false", "val", "def", "Nil", "if", "else")

  private lazy val n: Parser[BigInt] =
    "-?[0-9]+".r ^^ BigInt.apply

  private lazy val i: Parser[Int] =
    "_[1-9][0-9]*".r ^^ (_.substring(1).toInt)

  private lazy val b: Parser[Boolean] =
    "true" ^^^ true | "false" ^^^ false

  private lazy val x: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))

  private lazy val e: Parser[Expr] =
    (x <~ "=>") ~ e ^^ { case p ~ b => Fun(List(p), b) } |
      (wrapR(repsep(x, ",")) <~ "=>") ~ e ^^ { case ps ~ b =>
        if (dupCheck(ps))
          error(s"Duplicated parameters: ${ps.mkString(", ")}")
        Fun(ps, b)
      } | e0

  private lazy val e0: Parser[Expr] =
    rep1sep(e1, "::") ^^ (_.reduceRight(ConsE))

  private lazy val e1: Parser[Expr] =
    rep1sep(e2, "||") ^^ (_.reduceLeft(Or))

  private lazy val e2: Parser[Expr] =
    rep1sep(e3, "&&") ^^ (_.reduceLeft(And))

  private lazy val e3: Parser[Expr] =
    e4 ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ e4) ^^ { case e ~ es =>
      es.foldLeft(e) {
        case (l, "==" ~ r) => Eq(l, r)
        case (l, "!=" ~ r) => Neq(l, r)
        case (l, "<" ~ r)  => Lt(l, r)
        case (l, "<=" ~ r) => Lte(l, r)
        case (l, ">" ~ r)  => Gt(l, r)
        case (l, _ ~ r)    => Gte(l, r)
      }
    }

  private lazy val e4: Parser[Expr] =
    e5 ~ rep(("+" | "-") ~ e5) ^^ { case e ~ es =>
      es.foldLeft(e) {
        case (l, "+" ~ r) => Add(l, r)
        case (l, _ ~ r)   => Sub(l, r)
      }
    }

  private lazy val e5: Parser[Expr] =
    e6 ~ rep(("*" | "/" | "%") ~ e6) ^^ { case e ~ es =>
      es.foldLeft(e) {
        case (l, "*" ~ r) => Mul(l, r)
        case (l, "/" ~ r) => Div(l, r)
        case (l, _ ~ r)   => Mod(l, r)
      }
    }

  private lazy val e6: Parser[Expr] =
    "-" ~> e6 ^^ Neg | "!" ~> e6 ^^ Not | e7

  private lazy val e7: Parser[Expr] =
    e8 ~ rep(
      wrapR(repsep(e, ",")) ^^ AppP |
        "." ~> i ^^ ProjP |
        "." ~> "isEmpty" ^^^ EmptyP |
        "." ~> "nonEmpty" ^^^ NonEmptyP |
        "." ~> "head" ^^^ HeadP |
        "." ~> "tail" ^^^ TailP |
        "." ~> "isInstanceOf" ~> wrapS(t) ^^ TestP
    ) ^^ { case e ~ es =>
      es.foldLeft(e) {
        case (f, AppP(as))  => App(f, as)
        case (t, ProjP(i))  => Proj(t, i)
        case (l, EmptyP)    => Empty(l)
        case (l, NonEmptyP) => NonEmpty(l)
        case (l, HeadP)     => Head(l)
        case (l, TailP)     => Tail(l)
        case (e, TestP(t))  => Test(e, t)
      }
    }

  private lazy val e8: Parser[Expr] =
    x ^^ Id | n ^^ IntE | b ^^ BooleanE | "Nil" ^^^ NilE |
      wrapR(rep1sep(e, ",")) ^^ {
        case e :: Nil => e
        case es       => TupleE(es)
      } |
      ("if" ~> wrapR(e)) ~ e ~ ("else" ~> e) ^^ { case c ~ t ~ f =>
        If(c, t, f)
      } |
      ("val" ~> x <~ "=") ~ e ~ (";" ~> e) ^^ { case x ~ e ~ b =>
        Val(x, e, b)
      } |
      ("val" ~> wrapR(
        (x <~ ",") ~ rep1sep(x, ",")
      ) <~ "=") ~ e ~ (";" ~> e) ^^ { case x ~ xs ~ e ~ b =>
        val xs1 = x :: xs
        if (dupCheck(xs1))
          error(s"Duplicated variables: ${xs1.mkString(", ")}")
        TupleVal(x :: xs, e, b)
      } |
      rep1(d) ~ e ^^ { case ds ~ b =>
        val names = ds.map(_.name)
        if (dupCheck(names))
          error(s"Duplicated function names: ${names.mkString(", ")}")
        RecFuns(ds, b)
      } |
      wrapC(e)

  private lazy val d: Parser[FunDef] =
    ("def" ~> x) ~ wrapR(repsep(x, ",")) ~ ("=" ~> e <~ ";") ^^ {
      case n ~ ps ~ b =>
        if (dupCheck(ps))
          error(s"Duplicated parameters: ${ps.mkString(", ")}")
        FunDef(n, ps, b)
    }

  private lazy val t: Parser[Type] =
    "Int" ^^^ IntT |
      "Boolean" ^^^ BooleanT |
      "Tuple" ^^^ TupleT |
      "List" ^^^ ListT |
      "Function" ^^^ FunctionT

  private sealed trait E6P
  private case class AppP(as: List[Expr]) extends E6P
  private case class ProjP(i: Int) extends E6P
  private case object EmptyP extends E6P
  private case object NonEmptyP extends E6P
  private case object HeadP extends E6P
  private case object TailP extends E6P
  private case class TestP(t: Type) extends E6P

  // Desugaring
  private val T = BooleanE(true)
  private val F = BooleanE(false)
  private def Neg(e: Expr): Expr = Mul(e, IntE(-1))
  private def Not(e: Expr): Expr = If(e, F, T)
  private def Sub(l: Expr, r: Expr): Expr = Add(l, Neg(r))
  private def Neq(l: Expr, r: Expr): Expr = Not(Eq(l, r))
  private def Lte(l: Expr, r: Expr): Expr = {
    val lv, rv = fresh()
    Val(lv, l, Val(rv, r, Or(Eq(Id(lv), Id(rv)), Lt(Id(lv), Id(rv)))))
  }
  private def Gt(l: Expr, r: Expr): Expr = Not(Lte(l, r))
  private def Gte(l: Expr, r: Expr): Expr = Not(Lt(l, r))
  private def And(l: Expr, r: Expr): Expr = If(l, r, F)
  private def Or(l: Expr, r: Expr): Expr = If(l, T, r)
  private def NonEmpty(l: Expr): Expr = Not(Empty(l))
  private def TupleVal(xs: List[String], e: Expr, b: Expr): Expr = {
    require(xs.length > 1)
    val t = fresh()
    Val(
      t,
      e,
      xs.zipWithIndex.foldRight(b) { case ((x, i), b) =>
        Val(x, Proj(Id(t), i + 1), b)
      }
    )
  }

  private var id = -1
  private def fresh(): String = {
    id += 1
    s"$$x$id"
  }

  private def dupCheck(ss: List[String]): Boolean =
    ss.distinct.length != ss.length

  def apply(str: String): Expr = parseAll(e, str).getOrElse(error(""))
}
