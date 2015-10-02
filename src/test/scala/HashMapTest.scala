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

  def sizeOneHashMap(createSizeOneHashMap: => HashMap[Int]) {
    test("collisions") {
      val map = createSizeOneHashMap
      map.set("a", 1)
      map.set("b", 2)
      assert(map.get("a") === Some(1))
      assert(map.get("b") === Some(2))
      map.set("a", 2)
      map.set("b", 1)
      assert(map.get("a") === Some(2))
      assert(map.get("b") === Some(1))
    }
  }

  def sizeOneMaxSizeHashMap(createSizeOneMaxSizeHashMap: => HashMap[Int]) {
    test("max size") {
      val map = createSizeOneMaxSizeHashMap
      map.set("a", 1)
      intercept[RuntimeException] {
        map.set("b", 2)
      }
    }
  }
}

class BucketHashMapTestSuite extends FunSuite with HashMapBehaviors {
  def emptyHashMap = new BucketHashMap[Int](10)
  def sizeOneHashMap = new BucketHashMap[Int](1)

  testsFor(emptyHashMap(emptyHashMap))
  testsFor(sizeOneHashMap(sizeOneHashMap))
}
class OpenAddressingHashMapTestSuite extends FunSuite with HashMapBehaviors {
  def emptyHashMap = new OpenAddressingHashMap[Int](10)
  def sizeOneHashMap = new OpenAddressingHashMap[Int](1)

  testsFor(emptyHashMap(emptyHashMap))
  testsFor(sizeOneMaxSizeHashMap(sizeOneHashMap))
}
