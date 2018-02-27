package puck.assembler

import AbstractSyntax._
import puck.common.{LinedMessageLogger, MessageLogger}

object ContextAnalysis {
  trait Label
  case class ImportedLabel(from: String, label: String, internalName: String) extends Label
  case class InternalLabel(name: String, internalAddress: Int) extends Label

  case class SymbolDefinition(label: Label, definedAtLine: Int)

  case class SpecialAddress(name: String, internalAddress: Int)

  case class ContextAnalysisResult(symbolDefinitions: Map[String, SymbolDefinition],
                                   objectName: String,
                                   exportedSymbols: Map[String, Long],
                                   executableSymbols: Map[String, Long],
                                   initializationSymbols: Map[String, Long])

  private def InstructionSize(instruction: Instruction): Int = instruction match {
    case _: Add => 4
    case _: Sub => 4
    case _: Mulu => 4
    case _: Divu => 4
    case _: Modu => 4
    case _: Muli => 4
    case _: Divi => 4
    case _: Modi => 4

    case _: And => 4
    case _: Or => 4
    case _: Xor => 4
    case _: Sl => 4
    case _: Sr => 4

    case _: Eq => 4
    case _: Ne => 4
    case _: Lti => 4
    case _: Lei => 4
    case _: Gti => 4
    case _: Gei => 4
    case _: Ltu => 4
    case _: Leu => 4
    case _: Gtu => 4
    case _: Geu => 4

    case _: Brt => 6
    case _: Brf => 6
    case _: Jmp => 5
    case _: Jmpr => 2
    case _: Call => 6
    case _: Callr => 3
    case Halt => 1

    case _: Ldb => 4
    case _: Ldhw => 4
    case _: Ldw => 4
    case _: Stb => 4
    case _: Sthw => 4
    case _: Stw => 4

    case _: Inc => 2
    case _: Outc => 2
    case _: Inu => 2
    case _: Outu => 2
    case _: Ini => 2
    case _: Outi => 2
    case _: Inf => 2
    case _: Outf => 2

    case _: Setb => 3
    case _: Sethw => 4
    case _: Setw => 6
    case _: Cp => 3

    case _: Addf => 4
    case _: Subf => 4
    case _: Mulf => 4
    case _: Divf => 4

    case _: Ltf => 4
    case _: Lef => 4
    case _: Gtf => 4
    case _: Gef => 4

    case _: Addc => 7
    case _: Subc => 7

    case _ => 0
  }

  private def DirectiveSize(directive: Directive): Int = directive match {
    case _: ByteDirective => 1
    case _: HalfwordDirective => 2
    case _: WordDirective => 4
    case _ => 0
  }

  def gatherDefinedSymbols(lines: List[(Int, AssemblerLine)]): Map[String, SymbolDefinition] = {
    var offset = 0

    var map = Map[String, SymbolDefinition]()

    lines.foreach(line => line._2 match {
      case Label(label) =>
        val entry = map.get(label)
        if (entry.isDefined)
          LinedMessageLogger.logError(line._1, "Address " + label + " already defined at " + entry.get.definedAtLine)
        else
          map += (label -> SymbolDefinition(InternalLabel(label, offset), line._1))
      case ImportDirective(fromObject, label, internalName) =>
        val entry = map.get(internalName)
        if (entry.isDefined)
          LinedMessageLogger.logError(line._1, "Address " + internalName + " already defined at " + entry.get.definedAtLine)
        else
          map += (internalName -> SymbolDefinition(ImportedLabel(fromObject, label, internalName), line._1))
      case ins: Instruction => offset += InstructionSize(ins)
      case dir: Directive => offset += DirectiveSize(dir)
      case _ =>
    })

    map
  }

  def gatherObjectName(lines: List[(Int, AssemblerLine)]): String = {
    val defaultObjectName = "someobject"

    //Filter for ObjectDirectives and reverse it
    val directives = lines.filter(a => a._2 match {
      case _: ObjectDirective => true
      case _ => false
    }).map(line => (line._1, line._2.asInstanceOf[ObjectDirective])).reverse


    if (directives.isEmpty) {
      MessageLogger.logWarning("No object name defined, choosing \"" + defaultObjectName + "\"")
      return defaultObjectName
    }
    //The last one is now head and will be used. Log warning for each overwritten directive
    directives.tail.foreach(line => LinedMessageLogger.logWarning(line._1, "Object name definition overwritten by line " + directives.head._1))
    directives.head._2.name
  }

  def gatherSpecialAddresses(lines: List[(Int, AssemblerLine)], definedSymbols: Map[String, SymbolDefinition]): (Map[String, Long], Map[String, Long], Map[String, Long]) = {
    var exported: Map[String, Long] = Map()
    var executables: Map[String, Long] = Map()
    var initializations: Map[String, Long] = Map()

    lines.foreach(line => {
      line._2 match {
        case dir: ExportDirective =>
          val symbol = definedSymbols(dir.label)
          symbol.label match {
            case _: ImportedLabel => LinedMessageLogger.logError(line._1, "Export of imported Symbols is not allowed")
            case l: InternalLabel =>
              if (exported.contains(dir.label)) LinedMessageLogger.logWarning(line._1, "Multiple export declarations of symbol " + dir.label)
              else exported += (dir.label -> l.internalAddress)
          }
        case dir: ExecutableDirective =>
          val symbol = definedSymbols(dir.label)
          symbol.label match {
            case _: ImportedLabel => LinedMessageLogger.logError(line._1, "Using imported Symbols as executable addresses is not allowed")
            case l: InternalLabel =>
              if (executables.contains(dir.label)) LinedMessageLogger.logWarning(line._1, "Multiple executable declarations of symbol " + dir.label)
              else executables += (dir.label -> l.internalAddress)
          }
        case dir: InitializationDirective =>
          val symbol = definedSymbols(dir.label)
          symbol.label match {
            case _: ImportedLabel => LinedMessageLogger.logError(line._1, "Using imported Symbols as initialization addresses is not allowed")
            case l: InternalLabel =>
              if (initializations.contains(dir.label)) LinedMessageLogger.logWarning(line._1, "Multiple initialization declarations of symbol " + dir.label)
              else initializations += (dir.label -> l.internalAddress)
          }
        case _ =>
      }
    })

    (exported, executables, initializations)
  }

  def apply(lines: List[(Int, AssemblerLine)]): ContextAnalysisResult = {
    val definedSymbols = gatherDefinedSymbols(lines)
    val name = gatherObjectName(lines)
    val (exports, executables, initializations) = gatherSpecialAddresses(lines, definedSymbols)

    ContextAnalysisResult(definedSymbols, name, exports, executables, initializations)
  }
}
