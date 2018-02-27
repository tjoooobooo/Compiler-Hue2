package CodeGenerator

trait GlobalNamePool[A] {
  def nextName: A
}
