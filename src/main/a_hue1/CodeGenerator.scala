package puck.assembler

import puck.assembler.AbstractSyntax._
import puck.assembler.ContextAnalysis.{ContextAnalysisResult, ImportedLabel, InternalLabel, SymbolDefinition}
import puck.common.{ObjectFiles, PuckBinaryUtilities}

object CodeGenerator {

  private def generateCodeSegments(lines: List[AssemblerLine], symbolDefinitions: Map[String, SymbolDefinition]): (List[Byte], List[Long], List[ObjectFiles.Import]) = {
    var internalJumps: List[Long] = List()
    var externalJumps = Map[ImportedLabel, List[Long]]()

    var codeSegment: List[Byte] = List()

    def addJump(to: String, where: Long): Long = {
      val symbol = symbolDefinitions(to)
      symbol.label match {
        case external: ImportedLabel =>
          externalJumps += (external -> (where :: externalJumps.getOrElse(external, List())))
          0
        case internal: InternalLabel =>
          internalJumps ::= where
          internal.internalAddress
      }
    }

    def generateCodePoint(instruction: AssemblerLine): List[Byte] = {
      instruction match {
        case ins: Add => List[Byte](0x00.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Sub => List[Byte](0x01.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Mulu => List[Byte](0x02.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Divu => List[Byte](0x03.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Modu => List[Byte](0x04.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Muli => List[Byte](0x05.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Divi => List[Byte](0x06.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Modi => List[Byte](0x07.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)

        case ins: And => List[Byte](0x10.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Or => List[Byte](0x11.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Xor => List[Byte](0x12.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Sl => List[Byte](0x13.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Sr => List[Byte](0x14.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)

        case ins: Eq => List[Byte](0x20.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Ne => List[Byte](0x21.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Lti => List[Byte](0x22.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Lei => List[Byte](0x23.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Gti => List[Byte](0x24.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Gei => List[Byte](0x25.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Ltu => List[Byte](0x26.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Leu => List[Byte](0x27.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Gtu => List[Byte](0x28.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Geu => List[Byte](0x29.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)

        case ins: Brt =>
          List[Byte](0x30.toByte, ins.srcR.toByte) ::: PuckBinaryUtilities.generateWord(addJump(ins.label, codeSegment.length + 2))
        case ins: Brf =>
          List[Byte](0x31.toByte, ins.srcR.toByte) ::: PuckBinaryUtilities.generateWord(addJump(ins.label, codeSegment.length + 2))
        case ins: Jmp =>
          List[Byte](0x32.toByte) ::: PuckBinaryUtilities.generateWord(addJump(ins.label, codeSegment.length + 1))
        case ins: Jmpr => List[Byte](0x33.toByte, ins.dstR.toByte)
        case ins: Call =>
          List[Byte](0x34, ins.retR.toByte) ::: PuckBinaryUtilities.generateWord(addJump(ins.label, codeSegment.length + 2))
        case ins: Callr => List[Byte](0x35.toByte, ins.retR.toByte, ins.dstR.toByte)
        case Halt => List[Byte](0x36.toByte)

        case ins: Ldb => List[Byte](0x40.toByte, ins.srcR.toByte, ins.addressR.toByte, ins.offset.toByte)
        case ins: Ldhw => List[Byte](0x41.toByte, ins.srcR.toByte, ins.addressR.toByte, ins.offset.toByte)
        case ins: Ldw => List[Byte](0x42.toByte, ins.srcR.toByte, ins.addressR.toByte, ins.offset.toByte)
        case ins: Stb => List[Byte](0x43.toByte, ins.dstR.toByte, ins.addressR.toByte, ins.offset.toByte)
        case ins: Sthw => List[Byte](0x44.toByte, ins.dstR.toByte, ins.addressR.toByte, ins.offset.toByte)
        case ins: Stw => List[Byte](0x45.toByte, ins.dstR.toByte, ins.addressR.toByte, ins.offset.toByte)

        case ins: Inc => List[Byte](0x50.toByte, ins.dstR.toByte)
        case ins: Outc => List[Byte](0x51.toByte, ins.srcR.toByte)
        case ins: Inu => List[Byte](0x52.toByte, ins.dstR.toByte)
        case ins: Outu => List[Byte](0x53.toByte, ins.srcR.toByte)
        case ins: Ini => List[Byte](0x54.toByte, ins.dstR.toByte)
        case ins: Outi => List[Byte](0x55.toByte, ins.srcR.toByte)
        case ins: Inf => List[Byte](0x56.toByte, ins.dstR.toByte)
        case ins: Outf => List[Byte](0x57.toByte, ins.srcR.toByte)

        case ins: Setb => List[Byte](0x60.toByte, ins.dstR.toByte, ins.immediate.toByte)
        case ins: Sethw => List[Byte](0x61.toByte, ins.dstR.toByte) ::: PuckBinaryUtilities.generateHalfWord(ins.immediate)
        case ins: Setw => List[Byte](0x62.toByte, ins.dstR.toByte) ::: PuckBinaryUtilities.generateWord(if (ins.value.isLeft) ins.value.left.get else addJump(ins.value.right.get, codeSegment.length + 2))
        case ins: Cp => List[Byte](0x63.toByte, ins.dstR.toByte, ins.srcR.toByte)

        case ins: Addf => List[Byte](0x70.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Subf => List[Byte](0x71.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Mulf => List[Byte](0x72.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Divf => List[Byte](0x73.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)

        case ins: Ltf => List[Byte](0x80.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Lef => List[Byte](0x81.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Gtf => List[Byte](0x82.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)
        case ins: Gef => List[Byte](0x83.toByte, ins.dstR.toByte, ins.op1R.toByte, ins.op2R.toByte)

        case ins: Addc => List[Byte](0x90.toByte, ins.dstR.toByte, ins.op1R.toByte) ::: PuckBinaryUtilities.generateWord(ins.op2)
        case ins: Subc => List[Byte](0x91.toByte, ins.dstR.toByte, ins.op1R.toByte) ::: PuckBinaryUtilities.generateWord(ins.op2)

        case dir: ByteDirective => List[Byte](dir.data.getOrElse(0L).toByte)
        case dir: HalfwordDirective => PuckBinaryUtilities.generateHalfWord(dir.data.getOrElse(0))
        case dir: WordDirective => PuckBinaryUtilities.generateWord(dir.data.getOrElse(0))

        case _ => List()
      }
    }

    lines.foreach(instruction => {
      codeSegment = codeSegment ::: generateCodePoint(instruction)
    })

    (codeSegment, internalJumps, externalJumps.toList.map(e => ObjectFiles.Import(e._1.from, e._1.label, e._2)))
  }

  def apply(instructions: List[AssemblerLine], contextAnalysis: ContextAnalysisResult): ObjectFiles.ObjectFile = {
    val (codeSegment: List[Byte], internalJumps: List[Long], imports: List[ObjectFiles.Import]) = generateCodeSegments(instructions, contextAnalysis.symbolDefinitions)
    ObjectFiles.ObjectFile(contextAnalysis.objectName, contextAnalysis.exportedSymbols, imports, contextAnalysis.executableSymbols, contextAnalysis.initializationSymbols, internalJumps, codeSegment.toArray)
  }
}
