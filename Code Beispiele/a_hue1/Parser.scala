package puck.assembler

import puck.assembler.AbstractSyntax._
import puck.common.LinedMessageLogger

import scala.util.Try

object Parser {
  //-----------------Tokens--------------------------------------------------------
  private val identPatS = """ (\w+)"""
  private val newLinePatS = """(\n)"""
  private val registerPatS = """ (?:\$([0-9][0-9]?))"""
  private val stringPatS = """ (\w+)"""
  private val hexaOrNumber = """ ([-]?0x[A-F\d]+|0|(?:[1-9][0-9]*))"""
  private val hexaOrString = """ ([-]?0x[A-F\d]+|[\w]+)"""
  private val commentPat = """;.*"""
  private val exceptionPatS = """(.*[^\s])"""
  private val labelPatS = """(?:(\w+)[ ]*:)"""
  private val optionalNumS = """([\d]*)?"""
  private val twoOrThreeStringsS = """ (\w+) (\w+)[ ]?([(\w+)]*)?"""
  //--------------------Instruktionen-------------------------------------------------------------
  private val iThreeReg = """(ADD|SUB|MULU|DIVU|MODU|MULI|DIVI|MODI|AND|OR|XOR|SL|SR|EQ|NE|LTI|LEI|GTI|GEI|LTU|LEU|GTU|GEU|ADDF|SUBF|MULF|DIVF|LTF|LEF|GTF|GEF)"""
  private val iRegString = """(BRT|BRF|CALL)"""
  private val iJump = """(JMP)"""
  private val iTwoReg = """(CALLR|CP)"""
  private val iTwoRegHex = """(LDB|LDHW|LDW|STB|STHW|STW|ADDC|SUBC)"""
  private val iReg = """(INC|INU|INI|INF|OUTC|OUTU|OUTI|OUTF|JMPR)"""
  private val iRegHex = """(SETB|SETHW)"""
  private val iSetw = """(SETW)"""
  //--------------------Directives------------------------------------------------------------------
  private val dString = """.(export|object|initialization|executable)"""
  private val dOption = """.?(byte|halfword|word)[. ]?"""
  private val dImport = """.(import)"""
  //--------------------Instruktionen & Directives mit Werten---------------------------------------
  private val directivesStringS = dString + stringPatS
  private val directivesOptionS = dOption + optionalNumS
  private val directivesImportS = dImport + twoOrThreeStringsS

  private val instructionAS = iThreeReg + registerPatS + registerPatS + registerPatS
  private val instructionBS = iRegString + registerPatS + stringPatS
  private val instructionCS = iJump + stringPatS
  private val instructionES = iTwoReg + registerPatS + registerPatS
  private val instructionGS = iTwoRegHex + registerPatS + registerPatS + hexaOrNumber
  private val instructionHS = iReg + registerPatS
  private val instructionIS = iRegHex + registerPatS + hexaOrNumber
  private val instructionSETWS = iSetw + registerPatS + hexaOrString
  private val instructionHaltS = """(HALT)"""
  //---------------Regex Pattern-----------------------------
  private val LabelPat = labelPatS.r
  private val IdentPat = identPatS.r
  private val NewLinePat = newLinePatS.r
  private val DirectivesString = directivesStringS.r
  private val DirectivesOption = directivesOptionS.r
  private val DirectiveImport = directivesImportS.r
  private val InstructionA = instructionAS.r
  private val InstructionB = instructionBS.r
  private val InstructionC = instructionCS.r
  private val InstructionE = instructionES.r
  private val InstructionF = instructionHaltS.r
  private val InstructionG = instructionGS.r
  private val InstructionH = instructionHS.r
  private val InstructionI = instructionIS.r
  private val InstructionSETW = instructionSETWS.r
  private val ExceptionPat = exceptionPatS.r
  private val registerPat = registerPatS.r
  //-----------------------------------------------------------

  private val tokenPatS = List(commentPat, directivesImportS, directivesOptionS, directivesStringS, instructionAS,
    instructionBS, instructionCS, instructionES, instructionHaltS, instructionGS, instructionHS, instructionIS, instructionSETWS
    ,registerPatS, labelPatS, newLinePatS, exceptionPatS).reduceRight(_ + "|" + _)
  private val tokenPat = tokenPatS.r

  def apply(code: String): List[(Int, AssemblerLine)] = {
    var result: List[(Int, AssemblerLine)] = List()
    var code2 = code.replaceAllLiterally("STACK","$31").replaceAllLiterally("RETURN","$30").replaceAllLiterally("NULL","$0").replaceAll(commentPat,"")

    var tokens = tokenPat.findAllIn(code2)


    /*
    for (r <- tokens) {
      printf("(" + r + ") ")
    }
    tokens = tokenPat.findAllIn(code2)
    */
    var counter: Int = 1
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
    }
    //for (r <- result) printf("(" + r + ")\n")
    result
  }
}
