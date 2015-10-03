import org.scalatest.FunSuite

trait HashMapBehaviors { this: FunSuite =>
  def emptyHashMap(creatEmptyHashMap: => HashMap[Int]) {
    test("set/get") {
      val map = creatEmptyHashMap
      map.set("a", 1)
      assert(map.get("a") === Some(1))
      assert(map.get("b") === None)
      map.set("a", 2)
      assert(map.get("a") === Some(2))
    }

    test("delete") {
      val map = creatEmptyHashMap
      map.set("a", 1)
      assert(map.get("a") === Some(1))
      assert(map.delete("a") === Some(1))
      assert(map.get("a") === None)
      assert(map.delete("a") === None)
    }

    test("load factor") {
      val EPS = 1e-6
      val map = creatEmptyHashMap
      for (i <- 0 until 5) {
        map.set(i.toString, i)
      }
      assert(Math.abs(map.load - 0.5) < EPS)
      for (i <- 0 until 4) {
        map.delete(i.toString)
      }
      assert(Math.abs(map.load - 0.1) < EPS)
      map.delete("4")
      assert(Math.abs(map.load - 0.0) < EPS)
    }
  }
}
