package utils

import utils.CustomTypes.Result


// inspired from the stack overflow thread : https://stackoverflow.com/questions/7606587/applying-an-argument-list-to-curried-function-using-foldleft-in-scala
// especially the answer from the user Miles Sabin



case class ListParam[H, T](head: H, tail: T):
  def append[newT](newHead: newT): ListParam[newT, ListParam[H, T]] = ListParam(newHead, this)
  def toSting[H2, T1]: String = {
    val headString: String = this.head.toString
    val tailString: String = this.tail match {
      case Nil => ""
      case nestListParam:ListParam[H2, T1] => ", " + this.tail.toString
    }
    headString + tailString
  }
  def foldFromHead[H, T, H2, T2](fonctionApplied :H => Any): Any = {
    this.tail match {
      case Nil => {
        this.head match {
          case endHead:H => fonctionApplied(endHead)
        }
      }
      case nestLink:ListParam[H2, T2] => {
        this.head match {
          case endHead:H => {
            fonctionApplied(endHead) match {
              case resultFonc:(Any=>Any) => nestLink.foldFromHead(resultFonc)
            }
          }
        }
      }
    }
  }

object ListParam:
  def apply[F](head: F): ListParam[F, List[Nothing]] = ListParam(head, Nil)
  def fromList[H, T](listValue: List[Any]): ListParam[H, T] = {
    val resultListParam: ListParam[H, T]  = listValue match {
      case nonEmptyList: List[Any]  if nonEmptyList.length > 1 => {
        val headValue = nonEmptyList.takeRight(1).head match {
          case toHType:H => toHType
        }
        val tailValue = fromList(nonEmptyList.dropRight(1)) match {
          case toTType:T => toTType
        }
        ListParam(headValue, tailValue)
      }
      case almostEmptyList: List[Any] if almostEmptyList.length == 1 => {
        val headValue = almostEmptyList.takeRight(1).head match {
          case toHType:H => toHType
        }
        val tailValue = Nil match {
          case toTType:T => toTType
        }
        ListParam(headValue, tailValue)
      }
      case _ => {
        val headValue = Nil match {
          case toHType:H => toHType
        }
        val tailValue = Nil match {
          case toTType:T => toTType
        }
        ListParam(headValue, tailValue)
      }
    }
    resultListParam
  }


@main def tentative(): Unit = {
  val test = List(2, "a")
  val curriedSum: Int => String => String = x => y => x.toString + y
  val second = ListParam.fromList(test.reverse)
  val result = second.foldFromHead(curriedSum)
  println(result)
  println(second)
}