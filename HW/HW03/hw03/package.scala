package cs320

package object hw03 extends Homework03 {
  
  trait MRFWAEValue
  case class NumV(n: Int) extends MRFWAEValue
  case class CloV(params: List[String], body: MRFWAE, fenv: Env) extends MRFWAEValue
  case class RecV(rec: Env) extends MRFWAEValue
  type Env = Map[String, MRFWAEValue]
  
  def numVAdd(left: MRFWAEValue, right: MRFWAEValue): MRFWAEValue = left match {
    case NumV(l) => right match {
      case NumV(r) => NumV(l + r)
      case v => error(s"not a number: $v")
    }
    case v => error(s"not a number: $v")
  }
  def numVSub(left: MRFWAEValue, right: MRFWAEValue): MRFWAEValue = left match {
    case NumV(l) => right match {
      case NumV(r) => NumV(l - r)
      case v => error(s"not a number: $v")
    }
    case v => error(s"not a number: $v")
  }
  
  
  def run(str: String): String = {
    def interp(e: MRFWAE, env: Env): MRFWAEValue = e match {
      case Num(num) => NumV(num)
      case Add(left, right) => numVAdd(interp(left, env), interp(right, env))
      case Sub(left, right) => numVSub(interp(left, env), interp(right, env))
      case With(name, value, body) => interp(body, env + (name -> interp(value, env)))
      case Id(name) => env.getOrElse(name, error(s"free identifier: $name"))
      case App(func, args) => interp(func, env) match {
        case CloV(params, body, fenv) =>
          if(params.length==args.length) interp(body, fenv ++ params.zip(args.map(x => interp(x, env))).toMap)
          else error("wrong arity")
        case v => error(s"not a closure: $v")
      }
      case Fun(params, body) => CloV(params, body, env)
      case Rec(rec) => RecV(rec.map{case (k, v) => (k, interp(v, env))})
      case Acc(expr, name) => interp(expr, env) match {
        case RecV(rec) => rec.getOrElse(name, error(s"no such field"))
        case v => error(s"not a record: $v")
      }
    }
    
    interp(MRFWAE(str), Map()) match {
      case NumV(n) => n.toString
      case CloV(params, body, fenv) => "function"
      case RecV(rec) => "record"
    }
  }

  def tests: Unit = {
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    testExc(run("x"), "free identifier")
    testExc(run("{1}"), "not a closure")
    testExc(run("{+ 1 {fun {} 3}}"), "not a number")
    testExc(run("{+ 1 {record {x 1}}}"), "not a number")
    test(run("{+ 1 {{fun {} 3}}}"), "4")
    test(run("{+ 1 {access {record {x 1}} x}}"), "2")
    test(run("{fun {} {+ x 1}}"), "function")
    test(run("{fun {x y} {+ x y}}"), "function")
    test(run("{fun {x y z} 1}"), "function")
    test(run("{with {x 3} {{fun {} {+ 5 x}}}}"), "8")
    test(run("{with {x 3} {fun {} {+ 5 x}}}"), "function")
    test(run("{with {x 3} {{fun {y} {+ x y}} 2}}"), "5")
    testExc(run("{{fun {} {+ x 4}}}"), "free identifier")
    testExc(run("{{fun {x y} {+ x y}}}"), "wrong arity")
    testExc(run("{{fun {x y} {+ x y}} 1 2 3}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} y}"), "2")
    test(run("{access {record {x 1} {y 2} {f {fun {x} x}}} f}"), "function")
    test(run("{{access {record {x 1} {y 2} {f {fun {x} x}}} f} 7}"), "7")
    testExc(run("{{access {record {x 1} {y 2} {f {fun {x} {x}}}} fun} 7}"), "no such field")
    test(run("{record {x {access {record {y 1}} y}}}"), "record")
    testExc(run("{{record {x {access {record {y 1}} y}}} x}"), "not a closure")
    test(run("{access {record {x {access {record {y 1}} y}}} x}"), "1")
    test(run("{access {record {x {record {y 1}}}} x}"), "record")
  }
}
