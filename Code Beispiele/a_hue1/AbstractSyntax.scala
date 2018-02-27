package puck.assembler

object AbstractSyntax {
  type Object = List[AssemblerLine]

  trait AssemblerLine

  case class Label(name: String) extends AssemblerLine

  trait Directive extends AssemblerLine

  case class ExportDirective(label: String) extends Directive
  case class ImportDirective(fromObject: String, label: String, internalName: String) extends Directive
  case class ObjectDirective(name: String) extends Directive
  case class InitializationDirective(label: String) extends Directive
  case class ExecutableDirective(label: String) extends Directive
  case class ByteDirective(data : Option[Long]) extends Directive
  case class HalfwordDirective(data: Option[Long]) extends Directive
  case class WordDirective(data: Option[Long]) extends Directive

  trait Instruction extends AssemblerLine

  case class Add(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Sub(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Mulu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Divu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Modu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Muli(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Divi(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Modi(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class And(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Or(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Xor(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Sl(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Sr(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Eq(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Ne(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Lti(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Lei(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Gti(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Gei(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Ltu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Leu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Gtu(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Geu(dstR: Int, op1R: Int, op2R: Int) extends Instruction

  case class Brt(srcR: Int, label: String) extends Instruction
  case class Brf(srcR: Int, label: String) extends Instruction
  case class Jmp(label: String) extends Instruction
  case class Jmpr(dstR: Int) extends Instruction
  case class Call(retR: Int, label: String) extends Instruction
  case class Callr(retR: Int, dstR: Int) extends Instruction
  case object Halt extends Instruction
  case class Ldb(srcR: Int, addressR: Int, offset: Long) extends Instruction
  case class Ldhw(srcR: Int, addressR: Int, offset: Long) extends Instruction
  case class Ldw(srcR: Int, addressR: Int, offset: Long) extends Instruction
  case class Stb(dstR: Int, addressR: Int, offset: Long) extends Instruction
  case class Sthw(dstR: Int, addressR: Int, offset: Long) extends Instruction
  case class Stw(dstR: Int, addressR: Int, offset: Long) extends Instruction
  case class Inc(dstR: Int) extends Instruction
  case class Inu(dstR: Int) extends Instruction
  case class Ini(dstR: Int) extends Instruction
  case class Inf(dstR: Int) extends Instruction
  case class Outc(srcR: Int) extends Instruction
  case class Outu(srcR: Int) extends Instruction
  case class Outi(srcR: Int) extends Instruction
  case class Outf(srcR: Int) extends Instruction
  case class Setb(dstR: Int, immediate: Long) extends Instruction
  case class Sethw(dstR: Int, immediate: Long) extends Instruction
  case class Setw(dstR: Int, value: Either[Long, String]) extends Instruction
  case class Cp(dstR : Int, srcR : Int) extends Instruction

  case class Addf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Subf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Mulf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Divf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Ltf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Lef(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Gtf(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Gef(dstR: Int, op1R: Int, op2R: Int) extends Instruction
  case class Addc(dstR: Int, op1R: Int, op2: Long) extends Instruction
  case class Subc(dstR: Int, op1R: Int, op2: Long) extends Instruction
  case object NoOp extends Instruction
}
