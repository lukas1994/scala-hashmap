/** HashMap trait. */
trait HashMap[V] {
  def set(key: String, value: V)

  def get(key: String): Option[V]

  def delete(key: String): Option[V]

  def load(): Float
}
