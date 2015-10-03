/** Basic HashMap implementation in Scala using open addressing.
  *
  * Open addressing is a way to handle hash collisions. If the actual hash
  * position is unavailable the algorithm tries to find the next one that's
  * available by linear probing. So all objects are stored directly in the
  * array.
  * This works reasonably well for a low load factor.
  *
  * A major problem witn open-addressing hash maps is deletion. If an object
  * gets deleted we mark its position as `Deleted` and treat this fields as
  * occupied for `get()` operations and free for `set()` operations.
  * Thus, `find()` can get very slow after many deletions. That's why I
  * implemented `getOptimizedMap()` that "cleans up" the map.
  * At the moment the user of this implementation needs to call this method.
  * Ideally, the HashMap implementation itself handles clean-up runs
  * automatically.
  *
  * Another problem with this method is that the HashMap can be full. My
  * implementation throws a RuntimeException in this case. A better
  * implementation would handle resizing after the load factor gets larger than
  * some threshold.
  *
  * The main advantage of this method is the memory usage. There's no need to
  * allocate new memory for nodes of the linked list like in the
  * `BucketHashMap` implementation.
  *
  * Complexity (the average time complexities of O(1) can only be achieved if
  * resizing is implemented properly):
  *              average   worst
  *  ---------- --------- -------
  *  space      O(n)      O(n)
  *  get()      O(1)      O(n)
  *  set()      O(1)      O(n)
  *  delete()   O(1)      O(n)
  */
class OpenAddressingHashMap[V](maxSize: Int) extends HashMap[V] {
  /* Import classes from companion object. */
  import OpenAddressingHashMap._

  require(maxSize > 0)

  private var size = 0
  private var data = new Array[Entry[V]](maxSize)

  // Initialize `data` array to `Empty`.
  for (i <- 0 until maxSize) {
    data(i) = Empty
  }

  /** Returns index of key in data array, the next free index or `-1` if it
      doesn't exist and the array is full. */
  private def find(key: String): Int = {
    val startIndex = key.hashCode % maxSize
    var firstFree = -1
    for (delta <- 0 until maxSize) {
      val index = (startIndex + delta) % maxSize
      data(index) match {
        case Empty => if (firstFree == -1) return index
                      else return firstFree
        case Deleted => if (firstFree == -1) firstFree = index
        case Used(k, _) => if (k == key) return index
      }
    }
    -1
  }

  def set(key: String, value: V) = {
    val index = find(key)
    if (index == -1) throw new RuntimeException("HashMap full")
    data(index) match {
      case Empty | Deleted => size += 1
      case _ =>
    }
    data(index) = Used(key, value)
  }

  def get(key: String): Option[V] = {
    val index = find(key)
    if (index == -1) {
      None
    } else data(index) match {
      case Empty | Deleted => None
      case Used(_, value) => Some(value)
    }
  }

  def delete(key: String): Option[V] = {
    val index = find(key)
    var ret = None: Option[V]
    if (index != -1) data(index) match {
      case Used(_, value) => {
        ret = Some(value)
        data(index) = Deleted
        size -= 1
      }
      case _ =>
    }
    ret
  }

  def getOptimizedMap(): OpenAddressingHashMap[V] = {
    val newMap = new OpenAddressingHashMap[V](maxSize)
    for (i <- 0 until maxSize) data(i) match {
      case Used(key, value) => newMap.set(key, value)
      case _ =>
    }
    newMap
  }

  def load(): Float = size * 1.0f / maxSize
}

// Companion object.
object OpenAddressingHashMap {
  private abstract class Entry[+V]
  private case object Empty extends Entry[Nothing]
  private case object Deleted extends Entry[Nothing]
  private case class Used[V](key: String, value: V) extends Entry[V]
}
