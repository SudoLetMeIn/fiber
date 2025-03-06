package cs320

import Value._

object Implementation extends Template {
  type Env = Map[String, Value]
  def interp(expr: Expr): Value = interpreter(expr, Map())
  def interpreter(expr: Expr, env: Env): Value = expr match {
    case Id(x) =>
      env.get(x) match {
        case Some(v) => v
        case None    => error()
      }
    case IntE(value) => IntV(value)
    case Add(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(lv), IntV(rv)) => IntV(lv + rv)
        case _                    => error()
      }
    case Mul(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(lv), IntV(rv)) => IntV(lv * rv)
        case _                    => error()
      }
    case Div(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(lv), IntV(rv)) if (rv != 0) => IntV(lv / rv)
        case _                                 => error()
      }
    case Mod(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(lv), IntV(rv)) if (rv != 0) => IntV(lv % rv)
        case _                                 => error()
      }
    case BooleanE(b) => BooleanV(b)
    case Eq(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(x), IntV(y)) => BooleanV(x == y)
        case _                  => error()
      }
    case Lt(l, r) =>
      (interpreter(l, env), interpreter(r, env)) match {
        case (IntV(lv), IntV(rv)) => BooleanV(lv < rv)
        case _                    => error()
      }
    case If(condition, trueBranch, falseBranch) =>
      interpreter(condition, env) match {
        case BooleanV(true)  => interpreter(trueBranch, env)
        case BooleanV(false) => interpreter(falseBranch, env)
        case _               => error()
      }
    case TupleE(expressions) =>
      TupleV(for (expr <- expressions) yield interpreter(expr, env))
    case Proj(expression, index) =>
      interpreter(expression, env) match {
        case TupleV(values) if (values.length >= index && index >= 1) =>
          def return_index(l: List[Value], i: Int): Value = l match {
            case h :: _ if i == 1 => h
            case _ :: t           => return_index(t, i - 1)
            case Nil              => error()
          }
          return_index(values, index)
        case _ => error()
      }
    case NilE => NilV
    case ConsE(head, tail) =>
      val v1 = interpreter(head, env)
      interpreter(tail, env) match {
        case NilV          => ConsV(v1, NilV)
        case ConsV(v2, v3) => ConsV(v1, ConsV(v2, v3))
        case _             => error()
      }
    case Head(expression) =>
      interpreter(expression, env) match {
        case ConsV(head, _) => head
        case _              => error()
      }
    case Tail(expression) =>
      interpreter(expression, env) match {
        case ConsV(_, tail) => tail
        case _              => error()
      }
    case Val(name, expression, body) =>
      interpreter(body, env + (name -> interpreter(expression, env)))
    case Fun(parameters, body) => CloV(parameters, body, env)
    case Empty(expression) =>
      interpreter(expression, env) match {
        case ConsV(_, _) => BooleanV(false)
        case NilV        => BooleanV(true)
        case _           => error()
      }

    case App(func, arguments) => {
      interpreter(func, env) match {
        case CloV(parameters, body, nenv) => {
          val args = for (e <- arguments) yield interpreter(e, env)
          def pack(
              params: List[String],
              args: List[Value]
          ): Env = (params, args) match {
            case (h1 :: t1, h2 :: t2) => pack(t1, t2) + (h1 -> h2)
            case (Nil, Nil)           => Map()
            case _                    => error()
          }

          if (parameters.length == arguments.length) {
            val newEnv = nenv ++ pack(parameters, args)
            interpreter(body, newEnv)
          } else error()
        }

        case _ => error()
      }
    }

    case RecFuns(functions, body) =>
      val names = for (func <- functions) yield (func match {
        case FunDef(name, _, _) => name
        case _                  => error()
      })
      val closures =
        for (func <- functions)
          yield (func match {
            case FunDef(_, parameters, body) => CloV(parameters, body, env)
            case _                           => error()
          })
      def pack(
          names: List[String],
          closures: List[Value],
          environment: Env
      ): Env = (names, closures) match {
        case (h1 :: t1, h2 :: t2) => pack(t1, t2, environment) + (h1 -> h2)
        case (Nil, Nil)           => environment
        case _                    => error()
      }
      val nenv = pack(names, closures, env)
      for (cloV <- closures) cloV.env = nenv
      interpreter(body, nenv)

    case Test(expression, typ) =>
      (interpreter(expression, env), typ) match {
        case (IntV(_), IntT) | (BooleanV(_), BooleanT) | (TupleV(_), TupleT) |
            (ConsV(_, _), ListT) | (NilV, ListT) | (CloV(_, _, _), FunctionT) =>
          BooleanV(true)
        case _ => BooleanV(false)
      }
  }

}
