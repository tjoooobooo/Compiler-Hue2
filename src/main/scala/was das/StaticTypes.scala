object StaticTypes {
  sealed abstract class StaticType {
    def isCompatibleWith(other: StaticType): Boolean
  }
  object IntStaticType extends StaticType {
    override def isCompatibleWith(other: StaticType): Boolean = other match {
      case IntStaticType => true
      case _ => false
    }
  }
  object StringStaticType extends StaticType {
    override def isCompatibleWith(other: StaticType): Boolean = other match {
      case StringStaticType => true
      case _ => false
    }
  }
}