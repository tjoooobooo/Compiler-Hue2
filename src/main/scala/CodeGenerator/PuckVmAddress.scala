package CodeGenerator


sealed trait PuckVmAddress extends Address[PuckVmAddress] {
  override def valueAsAddress: PuckVmAddress = AddressAtAddress(this)
  override def thisAsValue: Option[PuckVmAddress] = None
}
case class RegisterAddress(register: Int) extends PuckVmAddress
case class SymbolicAddress(label: String) extends PuckVmAddress
case class StackframeAddress(offset: Long) extends PuckVmAddress
case class AddressAtAddress(address: PuckVmAddress) extends PuckVmAddress {
  override def thisAsValue: Option[PuckVmAddress] = Some(address)
}
