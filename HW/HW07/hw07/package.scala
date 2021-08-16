package cs320

package object hw07 extends Homework07 {
  
  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(params: List[String], body: KXCFAE, fenv: Env) extends KXCFAEValue
  case class ContV(proc: Cont) extends KXCFAEValue
  case object ThrowV extends KXCFAEValue
  type Env = Map[String, KXCFAEValue]
  type Cont = KXCFAEValue => KXCFAEValue
  
  def numVOp(left: KXCFAEValue, right: KXCFAEValue, op: (Int, Int) => Int): KXCFAEValue = (left, right) match {
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case _ => error(s"not both numbers: $left, $right")
  }
  
  def run(str: String): String = {
    def argRed(fv: KXCFAEValue, args: List[KXCFAE], argvs: List[KXCFAEValue], env: Env, k: Cont): KXCFAEValue = args match {
      case Nil => fv match {
        case CloV(params, body, fenv) =>
          if(params.length == argvs.length) interp(body, fenv ++ params.zip(argvs).toMap, k)
          else error("wrong arity")
        case ContV(kv) =>
          if(argvs.length == 1) kv(argvs(0))
          else error("wrong arity")
        case v => error(s"not a closure: $v")
      }
      case h::t =>
        interp(h, env, {
          case ThrowV => k(ThrowV)
          case v => argRed(fv, t, argvs :+ v, env, k)
        })
    }
    def interp(e: KXCFAE, env: Env, k: Cont): KXCFAEValue = e match {
      case Num(num) => k(NumV(num))
      case Add(left, right) =>
        interp(left, env, {
          case ThrowV => k(ThrowV)
          case lv =>
            interp(right, env, {
              case ThrowV => k(ThrowV)
              case rv => k(numVOp(lv, rv, _ + _))
            })
        })
      case Sub(left, right) =>
        interp(left, env, {
          case ThrowV => k(ThrowV)
          case lv =>
            interp(right, env, {
              case ThrowV => k(ThrowV)
              case rv => k(numVOp(lv, rv, _ - _))
            })
        })
      case Id(name) => k(env.getOrElse(name, error(s"free identifier: $name")))
      case Fun(params, body) => k(CloV(params, body, env))
      case App(func, args) =>
        interp(func, env, {
          case ThrowV => k(ThrowV)
          case f => argRed(f, args, List[KXCFAEValue](), env, k)
        })
      case If0(cond: KXCFAE, thenE: KXCFAE, elseE: KXCFAE) =>
        interp(cond, env, {
          case NumV(0) => interp(thenE, env, k)
          case ThrowV => k(ThrowV)
          case _ => interp(elseE, env, k)
        })
      case Withcc(name: String, body: KXCFAE) => interp(body, env + (name -> ContV(k)), k)
      case Try(tryE: KXCFAE, catchE: KXCFAE) =>
        interp(tryE, env, {
          case ThrowV => interp(catchE, env, k)
          case v => k(v)
        })
      case Throw => k(ThrowV)
    }
    
    interp(KXCFAE(str), Map(), x => x) match {
      case NumV(n) => n.toString
      case CloV(_, _, _) => "function"
      case ContV(_) => "continuation"
      case ThrowV => error("no enclosing try-catch")
    }
  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    testExc(run("{throw}"), "no enclosing try-catch")
    
    /* Write your own tests */
    test(run("{withcc cont {try {cont {throw}} catch {cont cont}}}"), "continuation")
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    testExc(run("x"), "free identifier")
    testExc(run("{1}"), "not a closure")
    testExc(run("{+ 1 {fun {} 3}}"), "not both numbers")
    test(run("{+ 1 {{fun {} 3}}}"), "4")
    test(run("{fun {} {+ x 1}}"), "function")
    test(run("{fun {x y} {+ x y}}"), "function")
    test(run("{fun {x y z} 1}"), "function")
    testExc(run("{{fun {} {+ x 4}}}"), "free identifier")
    testExc(run("{{fun {x y} {+ x y}}}"), "wrong arity")
    testExc(run("{{fun {x y} {+ x y}} 1 2 3}"), "wrong arity")
    testExc(run("{try {throw} catch {throw}}"), "no enclosing try-catch")
    test(run("{withcc esc esc}"), "continuation")
    testExc(run("{withcc esc {esc}}"), "wrong arity")
    test(run("{withcc esc {{fun {x} {+ 1 x}} {esc 3}}}"), "3")
    test(run("{withcc done {{withcc esc {done {+ 1 {withcc k {esc k}}}}} 3}}"), "4")
    testExc(run("{{fun {x y} {+ x y}} {1 2}}"), "not a closure")
    test(run("{try {{fun {x y} x} 1 {throw}} catch 2}"), "2")
    test(run("{+ 1 {withcc k {{fun {x y} {+ x y}} {k 2} 4}}}"), "3")
    testExc(run("{{fun {x y} y} 1 {esc 1}}"), "free identifier")
    test(run("{withcc esc {{fun {x y} y} 1 {esc 1}}}"), "1")
    testExc(run("{withcc esc {esc 3 5}}"), "wrong arity")
    test(run("{try {{fun {x} x} 1 2 3 4 {throw}} catch 1000}"), "1000")
    test(run("{if0 1 {throw} 2}"), "2")
    testExc(run("{if0 {throw} 1 2}"), "no enclosing try-catch")
    test(run("{withcc k {+ 1 {k 2}}}"), "2")
    test(run("{- 3 {withcc mine {+ 4 2}}}"), "-3")
    test(run("{- 3 {withcc mine {mine 42}}}"), "-39")
    test(run("{+ 1 {withcc x {- {x 2} 42}}}"), "3")
    test(run("{try {try {throw} catch 1} catch 2}"), "1")
    test(run("{try {try {throw} catch {throw}} catch 2}"), "2")
    testExc(run("{try {try {throw} catch {throw}} catch {throw}}"), "no enclosing try-catch")
    testExc(run("{{throw} 1}"), "no enclosing try-catch")
  }
}
