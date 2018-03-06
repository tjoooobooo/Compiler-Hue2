package compiler

import ZwischenCode.ZwischenAST._
import compiler.AssemblerAST._

object CompilerListe {

  def compile(instructions: List[Instr]) : List[AssemblerLine] = {
    var result: List[AssemblerLine] = List()

    for(instruction <- instructions) {
      getAssemblerLine(instruction)
    }
      def getAssemblerLine(instr: Instr) : Unit = instr match{
        case AssignInstr(dest,operand1,op,operand2) =>
         dest match {
           case Variable(name, loc) =>
             if(op.isEmpty) {
               defVariable(name,loc.asInstanceOf[TempMIntLoc].nr,operand2)
               result = result :+ Stw(loc.asInstanceOf[TempMIntLoc].nr,loc.asInstanceOf[TempMIntLoc].nr+1,0)
             }
             else {
               processOp(loc.asInstanceOf[TempMIntLoc].nr, op,operand1,operand2)
               result = result :+ Setw(getValue(operand1), Right("var_"+name))
               result = result :+ Stw(loc.asInstanceOf[TempMIntLoc].nr, getValue(operand1),0) // TODO offset
             }
           case TempMIntLoc(nr) =>
             if(operand1.isEmpty & op.isEmpty) result = result :+ getSetw(nr,operand2)
             else processOp(nr, op, operand1, operand2)
           case _ => NoOp
         }
      }


    def processOp(nr: Int, op: Option[MOp], operand1: Option[MIntLoc], operand2: Option[MIntLocOrValue]) : Unit = {
      op match{
        case Some(MultOp) => result = result :+ Muli(nr, getValue(operand1),getValue(operand2))
        case Some(DivOp) => result = result :+ Divi(nr, getValue(operand1),getValue(operand2))
        case Some(AddOp) => result = result :+ Add(nr, getValue(operand1),getValue(operand2))
        case Some(SubOp) => result = result :+ Sub(nr, getValue(operand1),getValue(operand2))
      }
    }
    def defVariable(name: String, loc: Int, operand2: Option[MIntLocOrValue]) : Unit = {
      result = result :+ getSetw(loc, operand2)
      result = result :+ Setw(loc+1, Right("var_" + name))
    }
    def getValue(op : Option[MIntLocOrValue]): Int = op match{
      case Some(MIntImmediateValue(d)) => d
      case Some(TempMIntLoc(nr)) => nr
    }
    def getSetw(nr: Int,op : Option[MIntLocOrValue]): AssemblerLine = {
      Setw(nr,
      op match {
        case Some(MIntImmediateValue(d)) => Left(d)
        case _ => Right("nix")
      })
    }
    result
  }

}
