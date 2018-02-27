package thm.mni.cb1718.hue_0.compiler

object Machine {

  val stackSize = 10
  val memSize = 100
  val stack: Array[Int] = new Array(stackSize)
  var stackTop: Int = -1
  val program_memory: Array[Instruction] = Array.tabulate(memSize)(i => Noop)

  var IR: Instruction = Noop
  var running = false

  def load(prog: List[Instruction]): Unit = {
    var i = 0
    prog.foreach {
      instr =>
        program_memory(i) = instr
        i = i + 1
    }
  }

  def run: Unit = {
    var PC = 0
    running = true
    while (PC < memSize && running) {
      IR = program_memory(PC)
      PC = PC + 1
      execute(IR)
    }
  }

  def execute(code: Instruction): Unit = code match {
    case Pushc(v: Int) =>
      stackTop = stackTop + 1
      stack(stackTop) = v
    case Add =>
      val v1 = stack(stackTop)
      stackTop = stackTop - 1
      val v2 = stack(stackTop)
      stackTop = stackTop - 1
      stackTop = stackTop + 1
      stack(stackTop) = v2 + v1
    case Sub =>
      val v1 = stack(stackTop)
      stackTop = stackTop - 1
      val v2 = stack(stackTop)
      stackTop = stackTop - 1
      stackTop = stackTop + 1
      stack(stackTop) = v2 - v1
    case Mult =>
      val v1 = stack(stackTop)
      stackTop = stackTop - 1
      val v2 = stack(stackTop)
      stackTop = stackTop - 1
      stackTop = stackTop + 1
      stack(stackTop) = v2 * v1
    case Div =>
      val v1 = stack(stackTop)
      stackTop = stackTop - 1
      val v2 = stack(stackTop)
      stackTop = stackTop - 1
      stackTop = stackTop + 1
      stack(stackTop) = v2 / v1
    case Halt =>
      running = false
    case Rdint =>
      val v = scala.io.StdIn.readInt()
      stackTop = stackTop + 1
      stack(stackTop) = v
    case Wrint =>
      val v = stack(stackTop)
      stackTop = stackTop - 1
      println(v)
    case Noop =>
  }

  def reset: Unit = {
    stackTop = -1
    running = false
    IR = Noop
    (0 until memSize) foreach( i => program_memory(i) = Noop )
  }
}



