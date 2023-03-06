object Week1 {
  def main(args: Array[String]): Unit = {
    println(Fun.ex(2.0))
  }
  
  object Fun {
    def func1(x: Double, y: Int) = x + (70*y)
    def ex(a: Double) = 50 * a
  }

  object ex2 {
    def maior(x: Int, y: Int): Int = if (x > y) x else y
    def menor(x: Int, y: Int): Int = if (x < y) x else y

    def a(x: (Int, Int), y: (Int, Int)): (Int, Int) = {
      (x._1 + x._2, y._1 * y._2);
    }

    def b(x: Int, y: Int, z: Int): (Int, Int) = {
      (maior(maior(x, y), z), menor(maior(x, y), z))
    }

    def c(x: (Int, Int, Int)): (Int, Int, Int) = {
      (maior(maior(x._1, x._2), x._3), menor(maior(x._1, x._2), x._3), (menor(menor(x._1, x._2), x._3)))
    }

    def d(x: Int, y: Int, z: Int): Boolean = {
      (x + y > z && y + z > x && z + x > y)
    }

    def e(fullName: String): String = {
      val split = fullName.split(" ")
      (s"${split(0)} ${split.last}")
    }
  }

  object ex3 {
    def a(x: Int, y: Int): Int = {
      var result = x;

      if (y == 0)
        result = 1;
      else {
        for (i <- 1 to y-1)
          result *= x;
      }

      (result.intValue)
    }

    def b(x: List[Int]): (Int, Int) = {
      val sortedList = x.sorted
      (sortedList(0), sortedList.last)
    }

    def c(x: List[Int]): (List[Int], Int) = {
      (x, x.length)
    }

    def d(x: List[Double]): Double = {
      var sum:Double = 0;
      for (i <- 0 to x.length-1) {
        sum += x(i)
      }
      (sum / x.length)
    }
  }
}
