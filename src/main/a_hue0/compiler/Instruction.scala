package thm.mni.cb1718.hue_0.compiler

abstract class Instruction(val opCode: Byte, val arg: Int)
case object Halt extends Instruction(0, 0)
case class Pushc(v: Int) extends Instruction(1, v)
case object Add extends Instruction(2, 0)
case object Sub extends Instruction(3, 0)
case object Mult extends Instruction(4, 0)
case object Div extends Instruction(5, 0)
case object Mod extends Instruction(6, 0)
case object Rdint extends Instruction(7, 0)
case object Wrint extends Instruction(8, 0)
case object Noop extends Instruction(-1, 0)
