package CodeGenerator

import CodeGenerator.TemporaryPool
import frontend.ProgSymbols.{ProcSymbol, VarSymbol}


trait Machine {
  type ResultCodeType
  type IntermediateCodeType <: IntermediateCode[IntermediateCodeType, ResultCodeType]
  type AddressType <: Address[AddressType]
  type CodeAddressType <: AddressType
  type TemporaryVariableType <: AddressType
  type TemporaryPoolType <: TemporaryPool[TemporaryVariableType, TemporaryPoolType]
  type GlobalPoolType <: GlobalNamePool[CodeAddressType]

  def defaultTemporaryState: TemporaryPoolType
  def globalAddressPool: GlobalPoolType

  def addressOfSymbol(symbol: VarSymbol): AddressType
  def addressOfSymbol(symbol: ProcSymbol): CodeAddressType

  def add(dst: AddressType, lhs: AddressType, rhs: AddressType, temporaryState: TemporaryPoolType): IntermediateCodeType
  def subtract(dst: AddressType, lhs: AddressType, rhs: AddressType, temporaryState: TemporaryPoolType): IntermediateCodeType
  def multiply(dst: AddressType, lhs: AddressType, rhs: AddressType, temporaryState: TemporaryPoolType): IntermediateCodeType
  def divide(dst: AddressType, lhs: AddressType, rhs: AddressType, temporaryState: TemporaryPoolType): IntermediateCodeType
  def modulo(dst: AddressType, lhs: AddressType, rhs: AddressType, temporaryState: TemporaryPoolType): IntermediateCodeType
}
