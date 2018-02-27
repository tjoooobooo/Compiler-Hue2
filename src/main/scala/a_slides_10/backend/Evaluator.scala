package a_slides_10.backend
import a_slides_10.frontend.AST._
class Evaluator {

  private type Value = Int

  private val store = new Store
  private val env = new RuntimeEnv {
    override type Address = store.Address
  }

  private def applyOp(op: (Value, Value) => Value, v1: Value, v2: Value) : Value = op(v1, v2)

  private def eval(e: Exp) : Value = e match {
    case Number(v)=> v
    case LocAccess(x)    =>
      val addr = eval(x)
      store(addr)
    case Add(l, r) => applyOp( (x:Value,y:Value) => x + y, eval(l), eval(r) )
    case Sub(l, r) => applyOp( (x:Value,y:Value) => x - y, eval(l), eval(r) )
    case Mul(l, r) => applyOp( (x:Value,y:Value) => x * y, eval(l), eval(r) )
    case Div(l, r) => applyOp( (x:Value,y:Value) => x / y, eval(l), eval(r) )
  }

  private def eval(b: BoolExp): Boolean = b match {
    case Less(e1, e2)      => eval(e1) < eval(e2)
    case Greater(e1, e2)   => eval(e1) > eval(e2)
    case Equal(e1, e2)     => eval(e1) == eval(e2)
    case NotEq(e1, e2)     => eval(e1) != eval(e2)
    case LessEq(e1, e2)    => eval(e1) <= eval(e2)
    case GreaterEq(e1, e2) => eval(e1) >= eval(e2)
  }

  private def eval(le: RefExp): store.Address = le match {
    case VarRef(varSymb) =>
      env.lookup(varSymb)   //.name

  }

  private def exec(cmd: Cmd) : Unit = cmd match {
    case Assign(le, re)  => {
      val addr = eval(le)
      val value = eval(re)
      store.assign(addr, value)
    }

    case If(condE, thenC, elseC) =>
      if (eval(condE)) {
          thenC.foreach( exec(_) )
      } else {
          elseC.foreach( exec(_) )
      }

    case While(condE, body) => {
      while (eval(condE) ) {
        body.foreach( exec(_) )
      }
    }
    case Write(e) =>
      println("Console> " + eval(e))

  }


  private def exec(definition: Definition) : Unit = definition match {
    case VarDef(varsymb,t, e) => {
      val value = eval(e)
      val addr = store.allocateCell(value)
      env.define(varsymb.name, addr)
      store.assign(addr, value)
    }
    case _ => throw new Exception("Undefined definition")
  }


  def exec(p: Prog): Unit = p match {
    case Prog(defs, cmds) =>
      env.enterScope()
      defs.foreach(exec)
      //cmds.foreach(exec(_))
      env.leaveScope()
  }

}
