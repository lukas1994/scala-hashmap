
object Main extends App {
  println("-- HashMap Main --")
  
  val map = new HashMap[Int](100)
  println("Created HashMap with 100 buckets...")

  for (i <- 0 until 50) {
    map.set(i.toString, i)
  }
  println("Set 50 values...")

  println(s"Load factor: ${map.load}")

  for (i <- 0 until 25) {
    map.delete(i.toString)
  }
  println("Deleted 25 values...")

  println(s"Load factor: ${map.load}")
}
