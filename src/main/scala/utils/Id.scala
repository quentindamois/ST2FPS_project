package utils

/**
 * An Opaque type used for id
 * */
opaque type Id = String
object Id:
  def apply(strId: String): Id = strId
  extension (id: Id) def strId: String = id
  extension (id: Id)
    def toString: String = id