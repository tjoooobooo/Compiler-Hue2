package CodeGenerator

trait Address[A <: Address[A]] {
  def valueAsAddress: A
  def thisAsValue: Option[A]
}
