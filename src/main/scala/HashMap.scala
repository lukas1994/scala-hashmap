/** Basic HashMap implementation in Scala.
  *
  * This implementation stores a linked list of objects for each hash value to
  * dael with hash collisions. The `maxSize` parameter determines the number
  * of "buckets" (linked lists).
  * To avoid dealing with special cases when deleting nodes we keep a dummy
  * nodes in the beginning of each linked list.
  * The private helper function `find()` always returns the node *before* the
  * actual node.
  *
  * Instead of returning null if `get()` is called and no value can be found we
  * make use of Scala's powerful type system and use the `Option` type.
  *
  * Currently, the load factor is unbounded. In practice, the number of buckets
  * is not fixed and depending on the load factor the HashMap gets resized
  * accordingly.
  *
  * Moreover, there are better ways to deal with hash collisions. For instance,
  * we could keep the list sorted or use a different hash function and a new
  * HashMap for each bucket.
  */
class HashMap[V](maxSize: Int) {
  private var size = 0
  private var data = new Array[HashMap.Node[V]](maxSize)

  // Create dummy nodes.
  for (i <- 0 until maxSize) {
    data(i) = HashMap.Node("", null.asInstanceOf[V], null)
  }

  /** Needs to be called on the dummy node. Returns the node *before* the
      actual node we want to find. Never returns `null`. */
  private def find(key: String, head: HashMap.Node[V]): HashMap.Node[V] = {
    var n = head
    while (n.next != null && n.next.key != key) {
      n = n.next
    }
    n
  }

  def set(key: String, value: V) = {
    val bucket = key.hashCode % maxSize
    val n = find(key, data(bucket))
    if (n.next != null) {
      n.next.value = value
    }
    else {
      data(bucket).key = key
      data(bucket).value = value
      data(bucket) = HashMap.Node("", null.asInstanceOf[V], data(bucket))
      size += 1
    }
  }

  def get(key: String): Option[V] = {
    val bucket = key.hashCode % maxSize
    val n = find(key, data(bucket))
    if (n.next != null) return Some(n.next.value) else return None
  }

  def delete(key: String): Option[V] = {
    val bucket = key.hashCode % maxSize
    val n = find(key, data(bucket))
    if (n.next != null) {
      val value = n.next.value
      n.next = n.next.next
      size -= 1
      Some(value)
    }
    else {
      None
    }
  }

  def load(): Float = size * 1.0f / maxSize
}

// Companion object
object HashMap {
    /** Nodes for the linked lists used for hash collisions. */
    private case class Node[V](var key: String, var value: V, var next: Node[V])
}
