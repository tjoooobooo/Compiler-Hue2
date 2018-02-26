package slides_10.backend

class Store {
  import scala.collection.mutable.ArrayBuffer

  type Address = Int
  type Value = Int

  // storage is a heap (open ended list of cells)
  private val heap : ArrayBuffer[Value] = ArrayBuffer()

  // get content of a cell
  def apply(a: Address): Value =
    if (a < heap.length) heap(a)
    else throw new NoSuchElementException(s"segmentation fault on addr $a (heap goes up to ${heap.length-1})")

  // set value at an allocated address
  def assign(a: Address, v: Value) : Unit =
    if (a < heap.length) {
      heap(a) = v
    }

  // allocate new cell
  def allocateCell(initVal: Value): Address = {
    heap += initVal
    heap.length -1
  }
}
