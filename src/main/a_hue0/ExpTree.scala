package thm.mni.cb1718.hue_0

// abstract syntax tree
abstract class ExpTree

case class Number(z: Int) extends ExpTree
case class Operation(op: Char,left: ExpTree,  right: ExpTree) extends ExpTree


