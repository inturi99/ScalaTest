package assignment
object SortingAlgorithm  extends App{
  //Selection Sort
  def selectionSort(list : Array[Int]): Array[Int] = {
    for (i <- 0 until (list.length -1)){
      var min = i
       for(j <- (i+1) until list.length){
        if (list(j) < list(min))
          min = j
      }
      if (i != min) {
        val swap = list(i);
        list(i) = list(min);
        list(min) = swap;
      }
    }
    return list
  }
}
