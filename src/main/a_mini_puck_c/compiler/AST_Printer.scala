package mini_puck_c.compiler

import AST._

/**
  * A helper class used to print the AST and thus support debugging.
  *
  * Note: The code is based on based on http://www.scala-blogs.org/2009/04/combinators-for-pretty-printers-part-1.html
  */
object AST_Printer {

  sealed abstract class Doc {
    def ~(that: Doc) = Cons(this, that)
    def ~~(that: Doc) = this ~ space ~ that
    def <~(that: Doc) = this ~ line ~ that
    def ~>(that: Doc) = this ~ nest(2, line ~ that)
  }

  implicit def string(s: String): Doc = Text(s)
  val line  = Line
  val space = Text(" ")
  def nest(n: Int, d: Doc) = Nest(n, d)

  case object Empty                         extends Doc
  case object Line                          extends Doc
  case class  Text  (s: String)             extends Doc
  case class  Cons  (left: Doc, right: Doc) extends Doc
  case class  Nest  (n: Int, d: Doc)        extends Doc

  def layout(d: Doc): String = d match {
    case Empty => ""
    case Line => "\n"
    case Text(s) => s
    case Cons(l, r) => layout(l) + layout(r)
    case Nest(n, Empty) => layout(Empty)
    case Nest(n, Line) => "\n" + (" " * n)
    case Nest(n, Text(s)) => layout(Text(s))
    case Nest(n, Cons(l, r)) => layout(Cons(Nest(n, l), Nest(n, r)))
    case Nest(i, Nest(j, x)) => layout(Nest(i+j, x))
  }

  trait DocAble[T] {
    def toDoc: Doc
  }
  trait MkDocAble[T] {
    def apply(x: T) : DocAble[T]
  }

  implicit object MkDocAbleCmd extends MkDocAble[Cmd] {
    def apply(c: Cmd) : DocAble[Cmd] = new DocAble[Cmd] {
      def toDoc: Doc = pp(c)
    }
  }

  implicit object MkDocAbleParamDef extends MkDocAble[ParamDef] {
    def apply(d: ParamDef) : DocAble[ParamDef] = new DocAble[ParamDef] {
      def toDoc: Doc = pp(d)
    }
  }

  implicit object MkDocAbleDef extends MkDocAble[Definition] {
    def apply(d: Definition) : DocAble[Definition] = new DocAble[Definition] {
      def toDoc: Doc = pp(d)
    }
  }

  implicit object MkDocAbleVarDef extends MkDocAble[VarDef] {
    def apply(d: VarDef) : DocAble[VarDef] = new DocAble[VarDef] {
      def toDoc: Doc = pp(d)
    }
  }

  implicit object MkDocAbleValueExp extends MkDocAble[Exp] {
    def apply(e: Exp) : DocAble[Exp] = new DocAble[Exp] {
      def toDoc: Doc = pp(e)
    }
  }

  implicit object MkDocAbleArgumentExp extends MkDocAble[Arg] {
    def apply(e: Arg) : DocAble[Arg] = new DocAble[Arg] {
      def toDoc: Doc = e.method match {
        case Some(ByValue) => pp(e.exp)
        case Some(ByRef) => "&" ~ pp(e.exp)
        case None => "NONE"
      }
    }
  }


  def pp[T: MkDocAble](lst: List[T])(implicit o: MkDocAble[T]): Doc = lst match {
    case Nil => Text("")
    case head::tail  => Text("") ~> (o(head).toDoc ~ tail.foldLeft(Text("").asInstanceOf[Doc])( (acc,item) => acc <~ o(item).toDoc ))
  }

  def pp(lexp: LocExp): Doc = lexp match {
    case DirectLoc(symb) => symb.name
    case StarConv(le) => "*"~pp(le)
  }

  def pp(rexp: Exp): Doc = rexp match {
    case Number(n) => n.toString()
    case LocAccess(lexp) => pp(lexp)
    case Add(l, r)  => "(" ~ pp(l) ~ "+" ~ pp(r) ~ ")"
    case Sub(l, r) => "(" ~ pp(l) ~ "-" ~ pp(r) ~ ")"
    case Mul(l, r) => "(" ~ pp(l) ~ "*" ~ pp(r) ~ ")"
    case Div(l, r)   => "(" ~ pp(l) ~ "/" ~ pp(r) ~ ")"
  }

  def pp(bexp: BoolExp): Doc = bexp match {
    case Less(l, r) => "(" ~ pp(l) ~ "<" ~ pp(r) ~ ")"
    case Greater(l, r) => "(" ~ pp(l) ~ ">" ~ pp(r) ~ ")"
    case Equal(l, r) => "(" ~ pp(l) ~ "=" ~ pp(r) ~ ")"
    case NotEq(l, r) => "(" ~ pp(l) ~ "#" ~ pp(r) ~ ")"
    case LessEq(l, r) => "(" ~ pp(l) ~ "<=" ~ pp(r) ~ ")"
    case GreaterEq(l, r) => "(" ~ pp(l) ~ ">=" ~ pp(r) ~ ")"
  }

  def pp(cmd: Cmd): Doc = cmd match {
    case Assign(left, right) => pp(left) ~~ ":=" ~~ pp(right) ~";"
    case If(e, thenCmds, elseCmds) => "IF" ~~ pp(e) ~~ "THEN" ~
      (if (elseCmds.length >0) pp(thenCmds) <~"ELSE" ~ pp(elseCmds) else  pp(thenCmds) ) <~
      "FI"
    case While(e, cmds) => "WHILE" ~~ pp(e) ~~ "DO" ~
      pp(cmds) <~
      "OD"
    case Write(e:Exp) => "WRITE" ~~ "(" ~ pp(e) ~ ");"
    case Call(symb, args) => "CALL" ~ space ~ symb.name ~ "(" ~ pp(args) ~ ");"
  }

  def pp(typeExp: TypeExp) = typeExp match {
    case IntTypeExp => "INT"
  }

  def pp(defi: Definition): Doc = defi match {
    case ProcDef(symb, paramdefs, localdefs, cmds) =>
      "PROCEDURE" ~ space ~ symb.name ~ "(" ~ pp(paramdefs) ~ ")" ~
        pp(localdefs) <~
        "BEGIN" ~
          pp(cmds) <~
        "END"
    case VarDef(symb, typeExp, initExp) =>
      "VAR" ~ space ~ (symb.name+ "<<" + symb.rtLocInfo.get.toString + ">>") ~ " : " ~ pp(typeExp) ~ " := " ~ pp(initExp) ~ ";"
    case ValueParamDef(symb, _) =>
      (symb.name+ "<<" + symb.rtLocInfo.get.toString + ">>") ~ ":" ~ symb.staticType.get.toString
    case RefParamDef(symb, _) =>
      (symb.name+ "<<" + symb.rtLocInfo.get.toString + ">>") ~ ":" ~ symb.staticType.get.toString
  }

  def pp(defi: VarDef): Doc = defi match {
    case VarDef(symb, typeExp, initExp) =>
      "VAR" ~ space ~ (symb.name + "<<" + symb.rtLocInfo.get.toString + ">>") ~ " : " ~ pp(typeExp) ~ " := " ~ pp(initExp) ~ ";"
  }



  def pp(obj: Obj): Doc =
    "OBJECT" ~
      pp(obj.defs) <~
      "INIT" ~
        pp(obj.cmds) <~
      "END"


  def astAsString(obj: Obj): String =
    layout(pp(obj))



}
