package puck.assembler

import puck.assembler.AbstractSyntax._
import puck.common.LinedMessageLogger

import scala.util.Try

object ParserVersuch {
  //-----------------Tokens--------------------------------------------------------
  private val newLinePatS = """(\n)"""
  private val numberPatS = """(0|(?:[1-9][0-9]*))"""
  private val registerPatS = """ (?:\$([0-9][0-9]?))"""
  private val stringPatS = """([\w\d]+)"""
  private val hexaOrNumberS = """ ([-]?0x[A-F\d]+|0|(?:[1:9][0-9]*))"""
  private val commentPatS = """;.*"""
  private val exceptionPatS = """(.*[^\s])"""
  private val hexaPatS = """([-]?0x[A-F\d]+)"""
  private val labelPatS = """(?:(\w+)[ ]*:)"""
  private val optionalNumS = """([\d+]*)?"""
  private val twoOrThreeStringsS = """ (\w+) (\w+)[ ]?([(\w+)]*)?"""
  //--------------------Instruktionen-------------------------------------------------------------
  private val iTwoRegHexS = """(LDB|LDHW|LDW|STB|STHW|STW|ADDC|SUBC) """
  private val iThreeRegS = """(ADD|SUB|MULU|DIVU|MODU|MULI|DIVI|MODI|AND|OR|XOR|SL|SR|EQ|NE|LTI|LEI|GTI|GEI|LTU|LEU|GTU|GEU|ADDF|SUBF|MULF|DIVF|LTF|LEF|GTF|GEF) """
  private val iTwoRegS = """(CALLR|CP) """
  private val iRegStringS = """(BRT|BRF|CALL) """
  private val iRegS = """(INC|INU|INI|INF|OUTC|OUTU|OUTI|OUTF|JMPR) """
  private val iJumpS = """(JMP) """
  private val iRegHexS = """(SETB|SETHW) """
  private val iSetwS = """(SETW) """
  private val iHaltS = """(HALT)"""
  //--------------------Directives------------------------------------------------------------------
  private val dStringS = """.(export|object|initialization|executable)"""
  private val dOptionS = """.?(byte|halfword|word)[. ]?"""
  private val dImportS = """.(import)"""
  //---------------Regex Pattern-----------------------------


  private val tokenPatS = List(commentPatS,iTwoRegHexS, iThreeRegS,iTwoRegS, iRegStringS, iRegS, iJumpS, iRegHexS, iSetwS, iHaltS,
    dStringS, dOptionS, dImportS, stringPatS, newLinePatS).reduceRight(_ + "|" + _)
  private val tokenPat = tokenPatS.r

  def apply(code: String): List[(Int, AssemblerLine)] = {
    var result: List[(Int, AssemblerLine)] = List()
    var code2 = code.replaceAllLiterally("STACK","$31").replaceAllLiterally("RETURN","$30").replaceAllLiterally("NULL","$0").replaceAll(commentPatS,"")
    var tokens = tokenPat.findAllIn(code)
    tokens.next()
    for (r <- tokens) {
      printf("(" + r + ") ")
    }
    tokens = tokenPat.findAllIn(code2)
    var counter: Int = 1
    /*
    while (tokens.hasNext) {
      var token = nextToken
      if(token != NoOp) result = result :+ (counter, token)
    }
    def nextToken: AssemblerLine = {
      tokens.next() match {
        case DirectivesString(d, s) =>
          d match {
            case "object" => ObjectDirective(s)
            case "executable" => ExecutableDirective(s)
            case "export" => ExportDirective(s)
            case "initialization" => InitializationDirective(s)
          }
        case DirectivesOption(d, o) =>
          var opt : Option[Long] = if(Try(o.toLong).isSuccess)Option(o.toLong) else None
          d match {
            case "byte" => ByteDirective(opt)
            case "word" => WordDirective(opt)
            case "halfword" => HalfwordDirective(opt)
          }
        case DirectiveImport(i,s1,s2,s3) =>
          ImportDirective(s1,s2,s3)
        case LabelPat(s) => Label(s)
        case InstructionA(instr, r1, r2, r3) =>
          instr match {
            case "ADD" => Add(r1.toInt, r2.toInt, r3.toInt)
            case "SUB" => Sub(r1.toInt, r2.toInt, r3.toInt)
            case "MULU" => Mulu(r1.toInt, r2.toInt, r3.toInt)
            case "DIVU" => Divu(r1.toInt, r2.toInt, r3.toInt)
            case "MODU" => Modu(r1.toInt, r2.toInt, r3.toInt)
            case "MULI" => Muli(r1.toInt, r2.toInt, r3.toInt)
            case "DIVI" => Divi(r1.toInt, r2.toInt, r3.toInt)
            case "MODI" => Modi(r1.toInt, r2.toInt, r3.toInt)
            case "AND" => And(r1.toInt, r2.toInt, r3.toInt)
            case "OR" => Or(r1.toInt, r2.toInt, r3.toInt)
            case "XOR" => Xor(r1.toInt, r2.toInt, r3.toInt)
            case "SL" => Sl(r1.toInt, r2.toInt, r3.toInt)
            case "SR" => Sr(r1.toInt, r2.toInt, r3.toInt)
            case "EQ" => Eq(r1.toInt, r2.toInt, r3.toInt)
            case "NE" => Ne(r1.toInt, r2.toInt, r3.toInt)
            case "LTI" => Lti(r1.toInt, r2.toInt, r3.toInt)
            case "LEI" => Lei(r1.toInt, r2.toInt, r3.toInt)
            case "GTI" => Gti(r1.toInt, r2.toInt, r3.toInt)
            case "GEI" => Gei(r1.toInt, r2.toInt, r3.toInt)
            case "LTU" => Ltu(r1.toInt, r2.toInt, r3.toInt)
            case "LEU" => Leu(r1.toInt, r2.toInt, r3.toInt)
            case "GTU" => Gtu(r1.toInt, r2.toInt, r3.toInt)
            case "GEU" => Geu(r1.toInt, r2.toInt, r3.toInt)
            case "ADDF" => Addf(r1.toInt, r2.toInt, r3.toInt)
            case "SUBF" => Subf(r1.toInt, r2.toInt, r3.toInt)
            case "MULF" => Mulf(r1.toInt, r2.toInt, r3.toInt)
            case "DIVF" => Divf(r1.toInt, r2.toInt, r3.toInt)
            case "LTF" => Ltf(r1.toInt, r2.toInt, r3.toInt)
            case "LEF" => Lef(r1.toInt, r2.toInt, r3.toInt)
            case "GTF" => Gtf(r1.toInt, r2.toInt, r3.toInt)
            case "GEF" => Gef(r1.toInt, r2.toInt, r3.toInt)
          }

        case InstructionB(instr, r, s) =>
          instr match {
            case "BRT" => Brt(r.toInt, s)
            case "BRF" => Brf(r.toInt, s)
            case "CALL" => Call(r.toInt, s)
          }
        case InstructionC(instr, s) => Jmp(s)
        case InstructionE(instr, r1, r2) =>
          instr match {
            case "CALLR" => Callr (r1.toInt, r2.toInt)
            case "CP" => Cp(r1.toInt, r2.toInt)
          }
        case InstructionF(instr) => Halt
        case InstructionG(instr, r1, r2, ls) =>     //(LDB|LDHW|LDW|STB|STHW|STW|ADDC|SUBC)
          instr match {
            case "LDB" => Ldb(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "LDHW" => Ldhw(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "LDW" => Ldw(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "STB" => Stb(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "STHW" => Sthw(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "STW" => Stw(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "ADDC" => Addc(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
            case "SUBC" => Subc(r1.toInt, r2.toInt, java.lang.Long.decode(ls))
          }
        case InstructionH(instr, r) =>       //(INC|INU|INI|INF|OUTC|OUTU|OUTI|OUTF|JMPR)
          instr match {
            case "INC" => Inc(r.toInt)
            case "INU" => Inu(r.toInt)
            case "INI" => Ini(r.toInt)
            case "INF" => Inf(r.toInt)
            case "OUTC" => Outc(r.toInt)
            case "OUTU" => Outu(r.toInt)
            case "OUTI" => Outi(r.toInt)
            case "OUTF" => Outf(r.toInt)
            case "JMPR" => Jmpr(r.toInt)
          }
        case InstructionI(instr, r, ls) =>
          instr match {
            case "SETB" => Setb(r.toInt, java.lang.Long.decode(ls))
            case "SETHW" => Sethw(r.toInt, java.lang.Long.decode(ls))
          }
        case InstructionSETW(instr,r, ls) =>
          val result: Either[Long,String] = try {
            Left(java.lang.Long.decode(ls))
          } catch {
            case e: Exception =>
              Right(ls)
          }
          Setw(r.toInt, result)
        case ExceptionPat(f) =>
          LinedMessageLogger.logError(counter,f)
          NoOp
        case _ =>
          counter = counter + 1
          NoOp
      }
    }*/
    for (r <- result) printf("(" + r + ")\n")
    result
  }
}
