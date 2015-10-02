
object Main extends App {
  def run(map: HashMap[Int]) = {
    for (i <- 0 until 50) {
      map.set(i.toString, i)
    }
    println("Set 50 values...")

    println(s"Load factor: ${map.load}")

    for (i <- 0 until 25) {
      map.delete(i.toString)
    }
    println("Delete 25 values...")

    println(s"Load factor: ${map.load}")
  }

  println("-- BucketHashMap Main --")
  println("Create BucketHashMap with maxSize=100...")
  run(new BucketHashMap[Int](100))

  println("-- OpenAddressingHashMap Main --")
  println("Create OpenAddressingHashMap with maxSize=100...")
  run(new OpenAddressingHashMap[Int](100))

}
