package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l,r)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def run(str: String): List[Int] = {
    def f(wae: MUWAE, env: Map[String, List[Int]]): List[Int] = wae match {
      case Num(nums) => nums
      case Add(left, right) => binOp(_ + _, f(left, env), f(right, env))
      case Sub(left, right) => binOp(_ - _, f(left, env), f(right, env))
      case With(name, expr, body) => f(body, env + (name -> f(expr, env)))
      case Id(id) => env.getOrElse(id, error(s"free identifier: $id"))
      case Min(left, mid, right) => binOp(Math.min, f(left, env), binOp(Math.min, f(mid, env), f(right, env)))
      case Max(left, mid, right) => binOp(Math.max, f(left, env), binOp(Math.max, f(mid, env), f(right, env)))
    }
    f(MUWAE(str), Map())
  }

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
    test(run("{1}"), List(1))
    test(run("{1 2 3}"), List(1, 2, 3))
    test(run("{}"), List())
    test(run("{+ {} {1 2}}"), List())
    test(run("{- {} {1 2}}"), List())
    test(run("{min {} {1 2} {3 4}}"), List())
    test(run("{min {1 2} {} {3 4}}"), List())
    test(run("{min {1 2} {3 4} {}}"), List())
    
    test(run("{+ {1 3} 7}"), List(8, 10))
    test(run("{- 10 {-3 -5}}"), List(13, 15))
    test(run("{with {x {2 3}} {+ x x}}"), List(4, 5, 5, 6))
    test(run("{with {x {1 2 3}} {+ x 10}}"), List(11, 12, 13))
    test(run("{with {x {1 2 3}} {+ x {0 10}}}"), List(1, 11, 2, 12, 3, 13))
    test(run("{min {3 4 5} {4 3 2} {3 4}}"), List(3, 3, 3, 3, 2, 2, 3, 4, 3, 3, 2, 2, 3, 4, 3, 3, 2, 2))
    test(run("{max {+ 1 2} {2 5} {with {x {2 3}} {+ x x}}}"), List(4, 5, 5, 6, 5, 5, 5, 6))
    test(run("{with {x {1 2}} {min x x x}}"), List(1, 1, 1, 1, 1, 1, 1, 2))
    test(run("{with {x {1 2}} {max x x x}}"), List(1, 2, 2, 2, 2, 2, 2, 2))
    test(run("{with {x {3 7}} {with {x {- x 2}} x}}"), List(1, 5))
    test(run("{with {x {3 7}} {with {x {- x 2}} {min x x {2 3}}}}"), List(1, 1, 1, 1, 1, 1, 2, 3))
    test(run("{with {x {with {y {2 3}} {- y {1 2}}}} {+ {0 10} x}}"), List(1, 0, 2, 1, 11, 10, 12, 11))
    test(run("{with {x {with {y {2 3}} {- y {1 2}}}} {with {y {+ 2 {1 3}}} {- y x}}}"), List(2, 3, 1, 2, 4, 5, 3, 4))
    
    testExc(run("{+ x 1}"), "free identifier: x")
    testExc(run("{with {x 1} {+ x y}}"), "free identifier: y")
  }
}