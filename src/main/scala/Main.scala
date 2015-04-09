package assignment
object Main extends App{
  // Factorial
  def fact(x: Int): Int = {
    var  r=1
    for(i <-  r to x) r=r*i
    r
  }
  val s=fact(6)
  println(s"Factorial : $s")
  //Max value in array
  def findMax(a:Array[Int]) :Int = {
    var max =a(0)
    for ( i <- 1 until a.length)
      if (a(i) > max) max = a(i)
    max
  }
  var mv = findMax(Array(1,2,3,4,5,6,7))
  println(s"Maximum $mv")
  //Prime Numbers
  def primeNumbers(m : Int, n : Int) = {
    var list=List.empty[Int]
    for (i <- m to n) {
      var  c=0;
      for(l <- 1 to i) {
        if(i%l==0)
          c = c + 1
      }
      if (c == 2)
        list = i :: list
      // print(s"$i ")

    }
    list
  }
  val a= primeNumbers(1,10)
  println(s"$a")
  //Binary Search
  def binarySearch(a:Array[Int],p:Int):Int = {
    var start = 0
    var end = a.length
    var mid = (start+end)/2
    while(a(mid)!=p && start < end){
      if(p < a(mid)) end = mid
      else start = mid+1
      mid = (start+end)/2
    }
    if(a(mid)==p) mid else  -1
  }
  val num1=Array(1,30,70,90,33,77)
  val nums=num1.sorted
  println(binarySearch(nums,33))
  println(binarySearch(nums,90))
  //Insertion Sort
  def insertionSort(arr:Array[Int]){
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
  val num=Array(10,30,70,30,90,33,77)
  println(num.mkString(","))
  insertionSort(num)
  println(num.mkString(","))
}
