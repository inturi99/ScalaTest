package assignment

object Functions {
  def fact(x: Int): Int = {
    var  result=1
    for(i <-  1 to x)
      result = result * i
    result
  }
  //Max value in array
  def findMax(a: Array[Int]): Int = {
    var max = a(0)
    for ( i <- 1 until a.length)
      if (a(i) > max) max = a(i)
    max
  }
  //Prime Numbers
  def primeNumbers(m: Int, n: Int) = {
    var list = List.empty[Int]
    for (i <- m to n) {
      var  c = 0;
      for(l <- 1 to i) {
        if(i % l == 0)
          c = c + 1
      }
      if (c == 2)
        list  = i :: list
    }
    list reverse

  }
  //Binary Search
  def binarySearch(a: Array[Int], p: Int): Int = {
    var start = 0
    var end = a.length
    var mid = (start + end)/2
    while(a(mid) != p && start < end){
      if(p < a(mid)) end = mid
      else start = mid+1
      mid = (start+end)/2
    }
    if(a(mid)==p) mid else  -1
  }
  //Insertion Sort
  def insertionSort(arr: Array[Int]) {
    for(i <- 1 until arr.length){
      val temp = arr(i)
      var j=i-1
      while(j > -1 && arr(j) > temp){
        arr(j+1) = arr(j)
        j -= 1
      }
      arr(j+1) = temp
    }
  }
   //map
  def map(v: Vector[Int], f: Int => Int) = {
    var result = Vector[Int]()
    for(n <- v){
      result = result :+ f(n)
    }
    result
  }
  // filter
  def filter(v:Vector[Int], f: Int => Boolean) = {
    var result = Vector[Int] ()
    for(n <- v ){
      if(f(n))
        result =result :+ n
    }
    result
  }
  def isEven(i: Int) = i % 2 == 0
}
