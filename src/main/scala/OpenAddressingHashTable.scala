/** Basic HashMap implementation in Scala.
  */
class OpenAddressingHashMap[V](maxSize: Int) extends HashMap[V] {
  import OpenAddressingHashMap._

  require(maxSize > 0)

  private var size = 0
  private var data = new Array[Entry[V]](maxSize)

  // Set all entries to `Empty`.
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
        case Deleted if firstFree == -1 => firstFree = index
        case Used(k, _) if k == key => return index
        case _ =>
      }
    }
    -1
  }

  def set(key: String, value: V) = {
    val index = find(key)
    if (index == -1) throw new RuntimeException("HashTable full")
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

  def load(): Float = size * 1.0f / maxSize
}

// Companion object.
object OpenAddressingHashMap {
  private abstract class Entry[+V]
  private case object Empty extends Entry[Nothing]
  private case object Deleted extends Entry[Nothing]
  private case class Used[V](key: String, value: V) extends Entry[V]
}
