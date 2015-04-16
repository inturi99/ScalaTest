import collection.mutable.Stack
import org.scalatest._
import assignment.Functions._

class Test extends FlatSpec with Matchers {
  "factorial of 6" should "be 720" in {
    val factorial = fact(6)
    factorial should be (720)
  }
  "Max value in Array [1,2,2,6,9]" should "be 9" in {
    val maxvalue=findMax(Array(1,2,3,6,9))
    maxvalue should be (9)
  }
  "Prime Numbers of 1 to 10" should "be (2,3,5,7)" in {
    val p=primeNumbers(1, 10)
    p  should be (List(2,3,5,7))
  }
  "BinarySearch in (1,30,90,70)" should "value 90 position is at  3" in {
    val numbers=Array(1,30,90,70)
    val nums=numbers.sorted
    val position= binarySearch(nums,90)
    position  should be(3)
  }
  "Insertion Sort of (10,30,20,5,70,90)" should "be (5,10,20,30,70,90)" in {
    val nn=Array(10,30,20,5,70,90)
    insertionSort(nn)
    val aa=nn
    aa should  be (Array(5,10,20,30,70,90))
  }
  "Map in Vector[1,2,3,4,5,6]" should "be Vector[2,3,4,5,6,7]" in {
    val ve=Vector(1,2,3,4,5,6)
    val ven=map(ve, _ + 1)
    ven should be (Vector(2,3,4,5,6,7))
  }

  "Map in Vector[1,2,3,4,5,6]" should " not be Vector[2,3,4,5,6,7,8]" in {
    val ve=Vector(1,2,3,4,5,6)
    val ven=map(ve, _ + 1)
    ven should not be (Vector(2,3,4,5,6,7,8))
  }


  "Filter in Vector[1,2,3,4,5,6,7,8,9] " should "be Vector[2,4,6,8]" in {
    val vf=Vector(1,2,3,4,5,6,7,8,9)
    val venf=filter(vf,isEven(_))
    venf should be (Vector(2,4,6,8))
  }
"Filter in Vector[1,2,3,4,5,6,7,8,9] " should "not be Vector[2,4,6,8,9]" in {
    val vf=Vector(1,2,3,4,5,6,7,8,9)
    val venf=filter(vf,isEven(_))
    venf should not be (Vector(2,4,6,8,9))
  }

}
