package CodeGenerator

trait TemporaryPool[A, B <: TemporaryPool[A, B]] {
  def nextTemporary: (A, B)
}
