package cs320

package object hw05 extends Homework05 {
  
  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String, body: SRBFAE, fenv: Env) extends SRBFAEValue
  case class RecV(rec: Rec) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  
  type Env  = Map[String, SRBFAEValue]
  type Addr = Int
  type Rec  = Map[String, Addr]
  type Sto  = Map[Addr, SRBFAEValue]
  
  def numVAdd(left: SRBFAEValue, right: SRBFAEValue): SRBFAEValue = left match {
    case NumV(l) => right match {
      case NumV(r) => NumV(l + r)
      case v => error(s"not a number: $v")
    }
    case v => error(s"not a number: $v")
  }
  def numVSub(left: SRBFAEValue, right: SRBFAEValue): SRBFAEValue = left match {
    case NumV(l) => right match {
      case NumV(r) => NumV(l - r)
      case v => error(s"not a number: $v")
    }
    case v => error(s"not a number: $v")
  }
  
  def lookup(name: String, env: Env): SRBFAEValue = env.getOrElse(name, error(s"free identifier: $name"))
  def recordLookup(name: String, rec: Rec): Addr = rec.getOrElse(name, error(s"no such field"))
  def storeLookup(addr: Addr, sto: Sto): SRBFAEValue = sto.getOrElse(addr, error(s"bad address: $addr"))
  
  def malloc(sto: Sto): Addr =
    sto.foldLeft(0) {
      case (max, (addr, _)) => math.max(max, addr)
    } + 1
  
  def run(str: String): String = {
    def interp(e: SRBFAE, env: Env, sto: Sto): (SRBFAEValue, Sto) = e match {
      case Num(num) => (NumV(num), sto)
      case Add(left, right) =>
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (numVAdd(lv, rv), rs)
      case Sub(left, right) =>
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (numVSub(lv, rv), rs)
      case Id(name) => (lookup(name, env), sto)
      case Fun(param, body) => (CloV(param, body, env), sto)
      case App(fun, arg) => {
        val (fv, fs) = interp(fun, env, sto)
        val (av, as) = interp(arg, env, fs)
        fv match {
          case CloV(param, body, fenv) => interp(body, fenv + (param -> av), as)
          case _ => error(s"not a closure: $fv")
        }
      }
      case NewBox(expr) =>
        val (v, s) = interp(expr, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))
      case SetBox(box, expr) =>
        val (bv, bs) = interp(box, env, sto)
        val (v, s) = interp(expr, env, bs)
        bv match {
          case BoxV(addr) => (v, s + (addr -> v))
          case _ => error(s"not a box: $bv")
        }
      case OpenBox(box) =>
        val (bv, bs) = interp(box, env, sto)
        bv match {
          case BoxV(addr) => (storeLookup(addr, bs), bs)
          case _ => error(s"not a box: $bv")
        }
      case Seqn(left, right) =>
        right.foldLeft(interp(left, env, sto)) {
          case ((_, ls), r) => interp(r, env, ls)
        }
      case Rec(fields) =>
        val (recm, recs) = fields.foldLeft(Map[String, Addr](), sto) {
          case ((lm, ls), (id, expr)) =>
            val (v, s) = interp(expr, env, ls)
            val addr = malloc(s)
            (lm + (id -> addr), s + (addr -> v))
        }
        (RecV(recm), recs)
      case Get(record, field) =>
        val (r, s) = interp(record, env, sto)
        r match {
          case RecV(rec) =>
            val addr = recordLookup(field, rec)
            (storeLookup(addr, s), s)
          case _ => error(s"not a record: $r")
        }
      case Set(record, field, expr) =>
        val (rv, rs) = interp(record, env, sto)
        val (v, s) = interp(expr, env, rs)
        rv match {
          case RecV(rec) =>
            val addr = recordLookup(field, rec)
            (v, s + (addr -> v))
          case _ => error(s"not a record: $rv")
        }
    }
  
    val (v, s) = interp(SRBFAE(str), Map(), Map())
    v match {
      case NumV(n) => n.toString
      case CloV(_, _, _) => "function"
      case RecV(_) => "record"
      case BoxV(_) => "box"
    }
  }

  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")

    /* Write your own tests */
    test(run("{+ 1 2}"), "3")
    test(run("{- 5 3}"), "2")
    testExc(run("x"), "free identifier")
    test(run("{fun {x} y}"), "function")
    testExc(run("{{fun {x} y} 1}"), "free identifier")
    test(run("{{fun {x} {+ x 2}} 1}"), "3")
    testExc(run("{newbox x}"), "free identifier")
    test(run("{newbox {fun {x} x}}"), "box")
    test(run("{newbox {newbox 1}}"), "box")
    test(run("{setbox {newbox 1} {+ 1 2}}"), "3")
    test(run("{setbox {newbox 1} {fun {x} y}}"), "function")
    test(run("{{setbox {newbox 1} {fun {x} x}} 2}"), "2")
    test(run("{setbox {newbox 1} {newbox 1}}"), "box")
    test(run("{openbox {newbox 1}}"), "1")
    test(run("{openbox {newbox {fun {x} x}}}"), "function")
    test(run("{{openbox {newbox {fun {x} x}}} 2}"), "2")
    test(run("{openbox {newbox {setbox {newbox 1} 2}}}"), "2")
    test(run("{openbox {setbox {newbox 1} {newbox 2}}}"), "2")
    test(run("{openbox {newbox {newbox 1}}}"), "box")
    test(run("{openbox {openbox {newbox {newbox 1}}}}"), "1")
    test(run("""{{fun {x} {{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox x}}} 5}"""), "14")
    test(run("{{fun {f} {{fun {a} {seqn {setbox a {+ 1 {openbox a}}} {f a} {openbox a}}} {newbox 1}}} {fun {x} {setbox x 0}}}"), "0")
    test(run("{{fun {f} {{fun {a} {seqn {f a} {setbox a {+ 1 {openbox a}}} {openbox a}}} {newbox 1}}} {fun {x} {setbox x 0}}}"), "1")
    test(run("{{fun {f} {{fun {a} {seqn {setbox a {+ 1 {openbox a}}} {openbox a}}} {newbox 1}}} {fun {x} {setbox x 0}}}"), "2")
    
    test(run("{get {rec {x 1} {y 2}} x}"), "1")
    testExc(run("{get {rec {x 1} {y 2}} z}"), "no such field")
    testExc(run("{rec {x {get {rec {y 1}} z}}}"), "no such field")
    test(run("{rec {x 1}}"), "record")
    testExc(run("{+ 1 {fun {x} 3}}"), "not a number")
    testExc(run("{+ 1 {rec {x 1}}}"), "not a number")
    test(run("{+ 1 {get {rec {x 1}} x}}"), "2")
    test(run("{get {rec {x 1} {y 2}} y}"), "2")
    test(run("{get {rec {x 1} {y 2} {f {fun {x} x}}} f}"), "function")
    test(run("{{get {rec {x 1} {y 2} {f {fun {x} x}}} f} 7}"), "7")
    testExc(run("{{get {rec {x 1} {y 2} {f {fun {x} x}}} fun} 7}"), "no such field")
    test(run("{rec {x {get {rec {y 1}} y}}}"), "record")
    test(run("{get {rec {x {get {rec {y 1}} y}}} x}"), "1")
    test(run("{get {rec {x {rec {y 1}}}} x}"), "record")
    test(run("{get {rec {x {rec {y 1}}}} x}"), "record")
    testExc(run("{get {rec} x}"),"no such field")
    testExc(run("{set {rec} x 1}"),"no such field")
    test(run("{seqn {{fun {r} {{{get r f} {get {get r a} x}} {get {get r a} y}}} {rec {a {rec {x 1} {y 2}}} {f {fun {x} {fun {y} {+ x y}}}}}}}"), "3")
  }
}
