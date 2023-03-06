object Week2 {
  def main(args: Array[String]): Unit = {
    val lst = List(
      ("a", "213", "@a"),
      ("b", "123", "@b"),
      ("c", "313", "@c"),
      ("d", "257", "@d")
    );
    println(findInfoByName(lst, "d"))
  }

  def empty[A](lst: List[A]) = lst.length == 0;

  def length(lst: List[Int]): Int = {
    lst match {
      case Nil => 0
      case _ :: tail => 1 + length(tail)
    }
  }

  def distance(lstPoint: List[(Double, Double)]): List[Double] = {
    lstPoint match {
      case Nil => Nil;
      case (x, y) :: tail => (Math.sqrt(x * x + y * y)) ::
        distance(tail);
    }
  }

  def transf(lst: List[Int]):  List[Int] = lst match {
    case a :: b :: rest => b :: a :: transf(rest)
    case _ => lst
  }

  def sumOfList(lst: List[Int]): Int = {
    if (lst.isEmpty) 0
    else lst.head + sumOfList(lst.tail)
  }

  def appendElement(lst: List[Int], element: Int): List[Int] = {
    //    lst.appended(element)
    lst :+ element;

    // Same thing
  }

  def concatenateLists(lst: List[Int], lst2: List[Int]): List[Int] = {
    //    lst ++ lst2
    lst ::: lst2;

    // Same thing
  }

  def sumElNonRecursive(lst: List[(Int, Int)]) = lst match {
    case a :: b :: c :: d :: e :: rest => c._1 + c._2 + e._1 + e._2;
  }

  def sumEl(lst: List[(Int, Int)]): Int = {
    def helper(l: List[(Int, Int)], index: Int, acc: Int): Int = l match {
      case Nil => acc
      case (x, y) :: tail =>
        if (index == 2 || index == 4)
          helper(tail, index + 1, acc + x + y)
        else
          helper(tail, index + 1, acc)
    }

    helper(lst, 0, 0)
  }

  def sumAndLength(lst: List[Int]): (Int, Int) = {
    def helpers(lst: List[Int], index: Int, sum: Int): (Int, Int) = {
      if (lst.isEmpty) (index, sum)
      else helpers(lst.tail, index + 1, sum + lst.head)
    }

    helpers(lst, 0, 0);
  }

  def average(lst: List[Int]): Double = {
    val listInfo = sumAndLength(lst);
    (listInfo._2 / listInfo._1).doubleValue
  }

  def exerciseG(lst: List[Double], value: Double): (List[Double], List[Double]) = {
    def helper(lst: List[Double], value: Double, below: List[Double], above: List[Double]): (List[Double], List[Double]) = {
      if (lst.isEmpty) (below, above)
      else {
        val currElement = lst.head;

        if (currElement < value)
          helper(lst.tail, value, below.appended(currElement), above);
        else
          helper(lst.tail, value, below, above.appended(currElement));
      }
    }

    helper(lst, value, List(), List());
  }

  def returnValuesAboveAverage(lst: List[Double]): List[Double] = {
    // This average is not really working properly for doubles but whatever..
    val average = Week2.average(lst.map(x => x.intValue))

    def helper(lst: List[Double], value: Double, result: List[Double]): List[Double] = {
      if (lst.isEmpty) result
      else if (lst.head >= value)
        helper(lst.tail, value, result.appended(lst.head))
      else
        helper(lst.tail, value, result)
    }

    helper(lst, average, List());
  }

  // name, phone number and email
  type Entry = (String, String, String)
  type LTelef = List[Entry]

  def emails(lst: LTelef): List[String] = lst match {
    case Nil => Nil
    case (_, _, email) :: tail => email :: (emails(tail))
  }

  def fixedNetworkTelephones(lst: LTelef): List[String] = {
    def helpers(lst: LTelef, emailLst: List[String]): List[String] = {
      if (lst.isEmpty) emailLst
      else {
        val currentEntry = lst.head;
        if (currentEntry._2.startsWith("2"))
          helpers(lst.tail, emailLst.appended(currentEntry._3));
        else
          helpers(lst.tail, emailLst);
      }
    }

    helpers(lst, List());
  }

  def findInfoByName(lst: LTelef, searchName: String): (String, String) = lst match {
    case (name, phone, email) :: tail => {
      if (name == searchName)
        (phone, email)
      else
        findInfoByName(lst.tail, searchName)
    }
    case Nil => ("Not found", "Not found")
  }
}