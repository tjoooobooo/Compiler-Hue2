package CodeGenerator

trait IntermediateCode[A <: IntermediateCode[A, B], B] {
  def +(tail: A): A

  def finish: B
}