/**
  * Created with IntelliJ IDEA.
  * User: layne
  * Date: 2017. 12. 19.
  * Time: PM 11:38
  * Copyright Coupang. All rights reserved.
  */
import fpinscala.datastructures._

object Chapter3 {

  def ex3_1():Int = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons (3, Cons(4, _)))) => x + y
      case _ => 101
    }
    x
  }

  def ex3_2():List[Int] = {
    List.tail(List(1,2,3,4,5,6))
  }

  def ex3_3():List[Int] = {
    List.setHead(10, List(1,2,3))
  }

  def ex3_4():List[Int] = {
    List.drop(List(1,2,3,4,5,6), 3)
  }

  def ex3_5():List[Int] = {
    List.dropWhile(List(1,2,3,4,5,6), (a:Int) => { a % 2 == 0 } )
  }

  def append():List[Int] = {
    List.append(List(1,2,3), List(10, 11, 12))
  }

  def ex3_6():List[Int] = {
    List.init(List(1,2,3))
  }

  def ex3_9(): Int = {
    val x = (1 to 1000000000).toList
    List.length(List(x))
  }


  def main(args: Array[String]): Unit = {

    println("---------------------------------------------------------------")
    print("ex3_2 : ")
    println(ex3_2())
    println("---------------------------------------------------------------")
    print("ex3_3 : ")
    println(ex3_3())
    println("---------------------------------------------------------------")
    print("ex3_4 : ")
    println(ex3_4())
    println("---------------------------------------------------------------")
    print("ex3_5 : ")
    println(ex3_5())
    println("---------------------------------------------------------------")
    print("append : ")
    println(append())
    println("---------------------------------------------------------------")
    print("ex3_6 : ")
    println(ex3_6())
    println("---------------------------------------------------------------")
    print("ex3_9 : ")
    println(ex3_9())

  }
}
