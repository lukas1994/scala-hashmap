import org.scalatest.FunSuite

class HashMapTest extends FunSuite {
  test("set/get") {
    val map = new HashMap[Int](10)
    map.set("a", 1)
    assert(map.get("a") == Some(1))
    assert(map.get("b") == None)
    map.set("a", 2)
    assert(map.get("a") == Some(2))
  }

  test("collisions") {
    val map = new HashMap[Int](1)
    map.set("a", 1)
    map.set("b", 2)
    assert(map.get("a") == Some(1))
    assert(map.get("b") == Some(2))
    map.set("a", 2)
    map.set("b", 1)
    assert(map.get("a") == Some(2))
    assert(map.get("b") == Some(1))
  }

  test("delete") {
    val map = new HashMap[Int](10)
    map.set("a", 1)
    assert(map.get("a") == Some(1))
    assert(map.delete("a") == Some(1))
    assert(map.get("a") == None)
    assert(map.delete("a") == None)
  }

  test("large dataset") {
    val map = new HashMap[Int](10)
    for (i <- 0 until 1000) {
      map.set(i.toString, i)
    }
    for (i <- 0 until 1000) {
      assert(map.get(i.toString) == Some(i))
    }
  }
}
