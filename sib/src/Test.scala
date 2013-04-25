
class A[T] (_arr: Array[T]) { var arr = _arr }
class C {
  def foo[T, B <: A[T]](b: Array[B]){
    var ele1 = b(0)
    var ele2 = b(1)
    for(i <- 0 until ele1.arr.length) {
      val temp = ele1.arr(i)
      ele1.arr(i) = ele2.arr(i)
      ele2.arr(i) = temp
    }
  }
}

object Test extends App {
  var p = Array.ofDim[A[Int]](2)
  var g1 = new A[Int](Array(1, 2, 3, 4, 5))
  var g2 = new A[Int](Array(6, 7, 8, 9, 10))
  p(0) = g1
  p(1) = g2
 
  (new C).foo[Int, A[Int]](p)
  p.foreach{ x => println(x.arr.deep.mkString(",")) } 
}
