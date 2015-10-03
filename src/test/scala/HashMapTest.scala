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

class BucketHashMapTestSuite extends FunSuite with HashMapBehaviors {
  def emptyHashMap = new BucketHashMap[Int](10)

  testsFor(emptyHashMap(emptyHashMap))

  test("collisions") {
    val map = new BucketHashMap[Int](1)
    map.set("a", 1)
    map.set("b", 2)
    assert(map.get("a") === Some(1))
    assert(map.get("b") === Some(2))
    map.set("a", 2)
    map.set("b", 1)
    assert(map.get("a") === Some(2))
    assert(map.get("b") === Some(1))
  }

  test("large dataset") {
    val map = new BucketHashMap[Int](10)
    for (i <- 0 until 1000) {
      map.set(i.toString, i)
    }
    for (i <- 0 until 1000) {
      assert(map.get(i.toString) === Some(i))
      assert(map.delete(i.toString) === Some(i))
    }
  }
}
class OpenAddressingHashMapTestSuite extends FunSuite with HashMapBehaviors {
  def emptyHashMap = new OpenAddressingHashMap[Int](10)

  testsFor(emptyHashMap(emptyHashMap))

  test("max size") {
    val map = new OpenAddressingHashMap[Int](1)
    map.set("a", 1)
    intercept[RuntimeException] {
      map.set("b", 2)
    }
  }

  test("map optimization") {
    val map = new OpenAddressingHashMap[Int](10)
    for (i <- 0 until 8) {
      map.set(i.toString, i)
    }
    for (i <- 0 until 4) {
      map.delete(i.toString)
    }
    val optimizedMap = map.getOptimizedMap()
    for (i <- 4 until 8) {
      assert(map.get(i.toString) === Some(i))
    }
  }
}
