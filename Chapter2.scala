/**
  * Created with IntelliJ IDEA.
  * User: layne
  * Date: 2017. 12. 14.
  * Time: PM 10:07
  * Copyright Coupang. All rights reserved.
  */
object Chapter2 {
  // 연습문제 2.1
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // 연습문제 2.2
  def isSorted[A](as: Array[A], f: (A, A) => Boolean): Boolean = {
    def loop(a: Int, b:Int): Boolean = {
      if (b == as.length)
        true
      else if (f(as(a), as(b)))
        loop(a + 1, b + 1)
      else
        false
    }

    loop(0, 1)
  }
  // 연습문제 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
  // 연습문제 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b) // !!!!!!!
  }
  // 연습문제 2.5
  def compose[A,B,C] (f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go (n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go (n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {

    def go (n: Int, a: Int, b: Int): Int = {
      if (n <= 1) a
      else
        go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

  private def intOrdered(a: Int, b: Int): Boolean = {
    if (a <= b)
      true
    else
      false
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

    val sortMsg = "%s is sorted? -> %s";
    println(sortMsg.format(Array(1,3,5,6).mkString(","), isSorted(Array(1,3,5,6), intOrdered)))

    println("%s".format(curry((a: Int, b: String) => Array(a, b))));
//    println("%s".format(uncurry((a: Int) => (b: String) => Array(a, b))));
  }
}
