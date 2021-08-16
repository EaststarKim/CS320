package cs320

package object hw09 extends Homework09 {
  
  trait CORELValue
  case class NumV(n: Int) extends CORELValue
  case class BoolV(b: Boolean) extends CORELValue
  case class ArrowV(param: String, body: COREL, var fenv: Env) extends CORELValue
  case class VariantV(name: String, value: CORELValue) extends CORELValue
  case class ConstructorV(name: String) extends CORELValue
  
  type Env = Map[String, CORELValue]
  case class TypeEnv(vars: Map[String, Type] = Map(), tbinds: Map[String, Map[String, Type]] = Map()) {
    def addVar(x: String, t: Type): TypeEnv = copy(vars = vars + (x -> t))
    def addTBind(x: String, cs: Map[String, Type]): TypeEnv = copy(tbinds = tbinds + (x -> cs))
  }
  
  def mustSame(l: Type, r: Type): Type = if (same(l, r)) l else notype(s"$l is not equal to $r")
  def same(l: Type, r: Type): Boolean = (l,r) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (ArrowT(p1, r1), ArrowT(p2, r2)) => same(p1, p2) && same(r1, r2)
    case (IdT(x1), IdT(x2)) => x1 == x2
    case _ => false
  }
  def notype(msg: Any): Nothing = error(s"no type: $msg")
  def validType(t: Type, tenv: TypeEnv): Type = t match {
    case NumT => t
    case BoolT => t
    case ArrowT(p, r) => ArrowT(validType(p, tenv), validType(r, tenv))
    case IdT(x) => if (tenv.tbinds.contains(x)) t else notype(s"$x is a free type")
  }
  def has(x: Type, t: Type): Type = x match {
    case NumT => x
    case BoolT => x
    case ArrowT(p, r) => ArrowT(has(p, t), has(r, t))
    case IdT(x1) => t match {
      case IdT(x2) => if (x1 != x2) x else notype(s"$t is type escaping")}
  }
  
  def typeCheck(str: String): Type = {
    def check(e: COREL, tenv: TypeEnv): Type = e match {
      case Num(_) => NumT
      case Bool(_) => BoolT
      case Add(l, r) =>
        mustSame(check(l, tenv), NumT)
        mustSame(check(r, tenv), NumT)
        NumT
      case Sub(l, r) =>
        mustSame(check(l, tenv), NumT)
        mustSame(check(r, tenv), NumT)
        NumT
      case Equ(l, r) =>
        mustSame(check(l, tenv), NumT)
        mustSame(check(r, tenv), NumT)
        BoolT
      case With(x, t, e, b) =>
        validType(t, tenv)
        check(b, tenv.addVar(x, mustSame(check(e, tenv), t)))
      case Id(x) => tenv.vars.getOrElse(x, notype(s"$x is a free identifier"))
      case Fun(x, t, b) =>
        validType(t, tenv)
        ArrowT(t, check(b, tenv.addVar(x, t)))
      case App(f, a) =>
        val funT = check(f, tenv)
        val argT = check(a, tenv)
        funT match {
          case ArrowT(p, r)
            if same(argT, p) => r
          case _ => notype(s"apply $argT to $funT")
        }
      case IfThenElse(c, t, f) =>
        mustSame(check(c, tenv), BoolT)
        mustSame(check(t, tenv), check(f, tenv))
      case Rec(f, ft, x, xt, b) =>
        validType(ft, tenv)
        validType(xt, tenv)
        mustSame(ft, ArrowT(xt, check(b, tenv.addVar(f, ft).addVar(x, xt))))
      case WithType(tn, cs, b) =>
        val tenvT = tenv.addTBind(tn, cs)
        val tenvV = cs.foldLeft(tenvT) {case (env, (vn, vt)) => env.addVar(vn, ArrowT(vt, IdT(tn)))}
        cs.foldLeft(validType(NumT, tenvT)) {case (_, (_, vt)) => validType(vt, tenvT)}
        has(check(b, tenvV), IdT(tn))
      case Cases(tn, e, s) =>
        val cs = tenv.tbinds.getOrElse(tn, notype(s"$tn is a free type"))
        if (cs.size != s.size) error("not all cases")
        mustSame(check(e, tenv), IdT(tn))
        s.toList.head match {
          case (vfn, (bfn, rfe)) =>
            s.foldLeft(check(rfe, tenv.addVar(bfn, cs.getOrElse(vfn, notype(s"$vfn is free"))))) {
              case (t, (vn, (bn, re))) => mustSame(t, check(re, tenv.addVar(bn, cs.getOrElse(vn, notype(s"$vn is free")))))}
        }
    }
    check(COREL(str), TypeEnv())
  }
  
  def numVOp(left: CORELValue, right: CORELValue, op: (Int, Int) => Int): CORELValue = (left, right) match {
    case (NumV(l), NumV(r)) => NumV(op(l, r))
    case _ => error("NaN")
  }
  
  def interp(str: String): String = {
    def eval(e: COREL, env: Env): CORELValue = e match {
      case Num(n) => NumV(n)
      case Bool(b) => BoolV(b)
      case Add(l, r) => numVOp(eval(l, env), eval(r, env), _ + _)
      case Sub(l, r) => numVOp(eval(l, env), eval(r, env), _ - _)
      case Equ(l, r) => (eval(l, env), eval(r, env)) match {
        case (NumV(l), NumV(r)) => BoolV(l==r)
        case _ => error(s"NaN")}
      case With(x, _, e, b) => eval(b, env + (x -> eval(e, env)))
      case Id(x) => env.getOrElse(x, error(s"free identifier: $x"))
      case Fun(x, t, b) => ArrowV(x, b, env)
      case App(f, a) => eval(f, env) match {
        case ArrowV(x, b, fenv) => eval(b, fenv + (x -> eval(a, env)))
        case ConstructorV(x) => VariantV(x, eval(a, env))}
      case IfThenElse(c, t, f) => eval(c, env) match {
        case BoolV(true) => eval(t, env)
        case _ => eval(f, env)}
      case Rec(f, _, x, _, b) =>
        val arrowV = ArrowV(x, b, env)
        arrowV.fenv = env + (f -> arrowV)
        arrowV
      case WithType(_, cs, b) => eval(b, cs.foldLeft(env){case (env, (vn, _)) => env + (vn -> ConstructorV(vn))})
      case Cases(_, e, s) => eval(e, env) match {
        case VariantV(x, av) =>
          val t = s.getOrElse(x, error(s"$x is a free constructor"))
          eval(t._2, env + (t._1 -> av))
      }
    }
    eval(COREL(str), Map()) match {
      case NumV(n) => n.toString
      case BoolV(b) => b.toString
      case ArrowV(_, _, _) => "function"
    }
  }

  def tests: Unit = {
    test(run("42"), "42")
    test(run("true"), "true")
    test(run("{+ 1 2}"), "3")
    test(run("{- 2 1}"), "1")
    test(run("{= 1 0}"), "false")
    testExc(run("{= true 0}"), "no type")
    test(run("{with {x : num 1} x}"), "1")
    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    test(run("""
      {{recfun {f: {num -> num} x: num}
               {if {= x 0} 0 {+ {f {- x 1}} x}}}
       10}"""), "55")
    testExc(run("{if 1 2 3}"), "no type")
    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}
               {banana {y} y}}}"""), "1")
    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}}}"""), "not all cases")

    /* Write your own tests */
    test(run("{fun {x : num} x}"), "function")
    test(run("false"), "false")
    testExc(run("{+ {fun {x : num} x} 2}"), "no type")
    test(run("{- {{fun {x : num} x} 2} 1}"), "1")
    testExc(run("{= x x}"), "no type")
    test(run("{= 0 0}"), "true")
    test(run("{with {f : {num -> num} {fun {x : num} x}} {f 1}}"), "1")
    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    test(run("{if true 2 3}"), "2")
    test(run("""
      {withtype
        {BOOL {number num}
               {boolean bool}}
        {cases BOOL {number 1}
               {number {x} {if {= x 0} false true}}
               {boolean {y} y}}}"""), "true")
    test(run("""
      {withtype
        {BOOL {number num}
               {boolean bool}}
        {cases BOOL {number 0}
               {number {x} {if {= x 0} false true}}
               {boolean {y} y}}}"""), "false")
    test(run("""
      {withtype
        {BOOL {number num}
               {boolean bool}}
        {cases BOOL {boolean true}
               {number {x} {if {= x 0} false true}}
               {boolean {y} y}}}"""), "true")
    test(run("""
      {withtype
        {fruit {apple num}}
        {cases fruit {apple 1}
               {apple {x} x}}}"""), "1")
    testExc(run("""
      {{withtype {foo {a num} {b num}}
                 {fun {x : foo}
                      {cases foo x
                        {a {n} {+ n 3}}
												{b {n} {+ n 4}}}}}
						{withtype {foo {c {num -> num}} {d num}}
			                {c {fun {y : num} y}}}}"""), "no type")
  }
}
