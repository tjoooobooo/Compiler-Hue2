package puck.assembler

import java.io.FileOutputStream

import AbstractSyntax.AssemblerLine
import puck.common.{MessageLogger, ObjectFiles}

object Assembler {

  case class CompilerOptions(
                              inputPath: String,
                              outputPath: String
                            ) {
    override def toString: String = {
      "Input Path: " + inputPath + '\n' +
        "Output Path: " + outputPath
    }
  }

  def apply(options: CompilerOptions): Boolean = {
    println("Assembler invoked with the following options:")
    println(options)
    println()

    val code = io.Source.fromFile(options.inputPath).mkString

    val abstractSyntax = Parser(code)
    if (MessageLogger.hasErrorHappened) return false

    println("Parsed " + abstractSyntax.length + " Assembler Lines")

    val objectCode = generateObjectFromAbstractSyntaxWithLines(abstractSyntax)
    if (objectCode.isEmpty) return false

    new FileOutputStream(options.outputPath).write(ObjectFiles.generateObjectFile(objectCode.get))

    true
  }

  def generateObjectFromAbstractSyntaxWithLines(abstractSyntax: List[(Int, AssemblerLine)]): Option[ObjectFiles.ObjectFile] = {
    val contextAnalysis = ContextAnalysis(abstractSyntax)
    if (MessageLogger.hasErrorHappened) return None

    val abstractObjectCode = CodeGenerator(abstractSyntax.map(t => t._2), contextAnalysis)
    if (MessageLogger.hasErrorHappened) return None

    Some(abstractObjectCode)
  }

  def generateObjectFromAbstractSyntax(abstractSyntax: List[AssemblerLine]) : Option[ObjectFiles.ObjectFile] = {
    generateObjectFromAbstractSyntaxWithLines(abstractSyntax.zipWithIndex.map(_.swap))
  }
}
