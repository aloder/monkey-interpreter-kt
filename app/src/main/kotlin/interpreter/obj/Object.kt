package interpreter.obj

public enum class ObjectType(public val objName: String) {
  INTEGER_OBJ("INTEGER"),
  BOOLEAN_OBJ("BOOLEAN"),
  NULL_OBJ("NULL"),
}

public interface MonkeyObject {
  fun inspect(): String
  fun type(): ObjectType
}

public class IntegerObj(public val value: Long) : MonkeyObject {
  override fun inspect(): String {
    return "$value"
  }
  override fun type(): ObjectType {
    return ObjectType.INTEGER_OBJ
  }
}

public class BooleanObj(public val value: Boolean) : MonkeyObject {
  override fun inspect(): String {
    return "$value"
  }
  override fun type(): ObjectType {
    return ObjectType.BOOLEAN_OBJ
  }
}

public class NullObj() : MonkeyObject {
  override fun inspect(): String {
    return "null"
  }
  override fun type(): ObjectType {
    return ObjectType.NULL_OBJ
  }
}
