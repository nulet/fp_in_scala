package fpinscala.datastructures

/**
  * Created with IntelliJ IDEA.
  * User: layne
  * Date: 2017. 12. 19.
  * Time: PM 10:51
  * Copyright Coupang. All rights reserved.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail:List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](l: List[A]): A = {
    l match {
      case Nil => sys.error("list is empty")
      case Cons(h, _) => h
    }
  }

  // ㅠ_ㅠ github 참고
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }
  }

  def setHead[A](nh: A, l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("setting head for empty list")
      case Cons(_, t) => Cons(nh, t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Cons(_, t) => drop(t, n - 1)
        case _ => l
      }
  }

  // ㅠ_ㅠ github 참조
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t, f) // 이 패턴 뭐지????
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => l
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]) =
    foldRight(as, 0)((_, z) => 1 + z)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =

}