package cs320

package object hw01 extends Homework01 {
  // Problem 1
  def dollar2won(dollar: Int): Int = 1100*dollar
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c
  def isEven(num: Int): Boolean = num%2==0
  def isOdd(num: Int): Boolean = num%2!=0
  def gcd(a: Int, b: Int): Int = Math.abs(if(b==0)a else gcd(b,a%b))
  def lcm(a: Int, b: Int): Int = Math.abs(if(gcd(a,b)==0)0 else a*b/gcd(a,b))

  // Problem 2
  def numOfHomework(course: COURSE): Int = course match{
    case CS320(quiz,homework) => homework
    case CS311(homework) => homework
    case CS330(projects,homework) => homework
  }
  def hasProjects(course: COURSE): Boolean = course match{
    case CS320(quiz,homework) => false
    case CS311(homework) => false
    case CS330(projects,homework) => projects>1
  }

  // Problem 3
  def namePets(pets: List[String]): List[String] = {
    pets.map((i: String) => if(i=="dog")"happy" else if(i=="cat")"smart" else if(i=="pig")"pinky" else i)
  }
  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    pets: List[String] => pets.map((i:String) => if(i==oldName)newName else i)
  }

  def tests: Unit = {
    test(dollar2won(1), 1100)
    test(volumeOfCuboid(1, 2, 3), 6)
    test(isEven(10), true)
    test(isOdd(10), false)
    test(gcd(123, 245), 1)
    test(lcm(123, 245), 30135)
    test(numOfHomework(CS320(quiz = 4, homework = 3)), 3)
    test(hasProjects(CS320(quiz = 3, homework = 9)), false)
    test(namePets(List("dog", "cat", "pig")), List("happy", "smart", "pinky"))
    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))

    /* Write your own tests */
    test(dollar2won(9), 9900)
    test(volumeOfCuboid(7, 8, 9), 504)
    test(isEven(7), false)
    test(isOdd(7), true)
    test(isEven(-9), false)
    test(isOdd(-9), true)
    test(gcd(237, 790), 79)
    test(lcm(237, 790), 2370)
    test(lcm(0,0),0)
    test(numOfHomework(CS311(homework = 2)), 2)
    test(hasProjects(CS311(homework = 2)), false)
    test(numOfHomework(CS330(projects=1,homework = 5)), 5)
    test(hasProjects(CS330(projects=1,homework = 5)), false)
    test(hasProjects(CS330(projects=2,homework = 5)), true)
    test(namePets(List("dog", "tiger", "cat")), List("happy", "tiger", "smart"))
    test(namePets(List("dog", "pig", "dog")), List("happy", "pinky", "happy"))
    test(giveName("tiger", "tigger")(List("pig", "tiger", "bear")), List("pig", "tigger", "bear"))
    test(giveName("kangaroo", "roo")(List("kangaroo", "bear", "pig")), List("roo", "bear", "pig"))
  }
}