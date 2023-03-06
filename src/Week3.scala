import scala.annotation.tailrec

object Week3 {
  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3, 4);
    val b = List(1, 2, 3, 4, 5);
    val com = List((1.0, 2.0), (2.0, 3.0), (3.0, 4.0))
    println(maximum(com));
  }
  
  // 1.1 a)
  def factorial_noIf(n: Int): Int = n match {
    case 0 => 1
    case x => x * factorial_noIf(x - 1)
  }
  
  // 1.1 b)
  def factorial_withIf(x: Int): Int = {
    if (x == 0) 1
    else x * factorial_withIf(x - 1)
  }
  
  // 1.1 c)
  def factorial_tailRecursive(x: Int): Int = {
    @tailrec
    def aux(result: Int, n: Int): Int = n match {
      case 0 => result
      case curN => aux(result * curN, curN - 1)
    }
    
    aux(1, x);
  }
  
  // 1.2 a)
  def remDup_standardRecursive[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case lstHead :: lstTail => lstHead :: remDup_standardRecursive(lstTail.dropWhile(_ == lstHead))
  }
  
  // 1.2 b)
  def remDup_tailRecursive[A](lst: List[A]): List[A] = {
    @tailrec
    def aux(resultingLst: List[A], l: List[A]): List[A] = l match {
      case Nil => resultingLst
      case lstHead :: lstTail => aux(resultingLst :+ lstHead, lstTail.dropWhile(_ == lstHead))
    }
    
    aux(List[A](), lst);
  }
  
  // 2
  def lazyListRange(lo: Int, hi: Int): LazyList[Int] = {
    println(lo)
    if (lo >= hi) LazyList.empty
    else lo #:: lazyListRange(lo + 1, hi)
  }

  def listRange(lo: Int, hi: Int): List[Int] = {
    println(lo)
    if (lo >= hi) List()
    else lo :: listRange(lo + 1, hi)
  }
  
  // listRange(1, 100).take(3).toList prints all numbers from 1 to 100 since
  // listRange(1, 100) will run to completion before returning the list with 3 elements
  
  // lazyListRange(1, 100).take(3).toList will only print 3 numbers since only
  // 3 numbers will be accessed and, therefore, computed
  
  // 3 a)
  def listOfSumsPerIndex[A: Numeric](x: List[A], y: List[A]): List[A] = {
    // Allows generic Numerics to be added -> (xHead + yHead)
    import Numeric.Implicits._
    (x, y) match {
      case (xHead :: xTail, yHead :: yTail) => (xHead + yHead) :: listOfSumsPerIndex(xTail, yTail)
      case _ => Nil
    }
  }
  
  // 3 b)
  def zipWith[A, B, C](func: (A, B) => C)(x: List[A], y: List[B]): List[C] = (x, y) match {
    case (xHead :: xTail, yHead :: yTail) => func(xHead, yHead) :: zipWith(func)(xTail, yTail)
    case _ => Nil
  }
  
  // 3 c)
  def isSorted[A](lst: List[A], ordered: (A, A) => Boolean): Boolean = lst match {
    // Assumes that empty lists are ordered..
    case head :: tail => {
      if (ordered(head, tail.head)) 
        isSorted(tail, ordered)
      else false
    }
    case _ => true 
  }
  
  // 3 d)
  def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = {

    def swap(l: List[Int]): List[Int] = l match {
      case x :: y :: xs if f(y, x) => y :: swap(x :: xs)
      case x :: xs => x :: swap(xs)
      case Nil => Nil
    }

    val sortedData = swap(data)
    if (sortedData == data) data
    else bubbleSort(sortedData, f)
  }
  
  // Mapping, Filtering and Folding Functions on Lists
  
  // a)
  def doubles(lst: List[Int]): List[Int] = lst.map(x => 2 * x)
  
  // b)
  def oneOrTwoLetters(lst: List[String]): List[String] = lst.filter(x => x.length <= 2)

  // c)
  def sumList(lst: List[Int]): Int = lst.foldLeft(0)(_ + _)
  
  def multList(lst: List[Int]): Int = lst.foldLeft(1)(_ * _)
  
  // 4 a)
  def paresord(lst: List[(Int, Int)]): List[(Int, Int)] = 
    lst.filter(tuple => tuple._1 < tuple._2)
//    lst.filter({ case (x, y) => x < y}) -> Same thing but using pattern matching on the tuple
  
  // 4 b)
  def myconcat(lst: List[String]): String =
    lst.foldLeft("")((result, str) => result + str)
    
  // 4 c)
  def maximum(lst: List[(Double, Double)]): List[Double] =
    lst.map({ case (x, y) => if (x > y) x else y })
    
  // 6
  def indicative(ind: String, telefs: List[String]): List[String] = {
    def filterNumbers(l: List[String], acc: List[String]): List[String] = l match {
      case Nil => acc
      case x :: xs =>
        if (x.substring(0, ind.length) == ind) filterNumbers(xs, x :: acc)
        else filterNumbers(xs, acc)
    }

    filterNumbers(telefs, Nil).reverse
  }
  
  // 7
  def abbreviateNames(names: List[String]): List[String] =
    names.map(names => {
      val parts = names.split(" ")
      s"${parts.head.head}. ${parts.last}"
    })
}