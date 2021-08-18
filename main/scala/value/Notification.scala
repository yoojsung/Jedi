package value

class Notification(private val value: String) extends Value {
  override def toString: String = this.value

  override def hashCode(): Int = this.value.hashCode()
}

object Notification {
  def apply(m: String): Notification = new Notification(m)
  val OK: Notification = Notification("ok")
  val DONE: Notification = Notification("done")
  val UNSPECIFIED: Notification = Notification("")
}
