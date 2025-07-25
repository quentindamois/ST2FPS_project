package utils

import utils.CustomTypes.Result


// inspired from the stack overflow thread : https://stackoverflow.com/questions/7606587/applying-an-argument-list-to-curried-function-using-foldleft-in-scala
// especially the answer from the user Miles Sabin


/**
 * Class used to create a List of value to be given to a curried function
 * 
 * @constructor the basic constructor of the class
 * @param head the first element of the ListParam
 * @param tail the rest of the ListParam corresponding to a nested ListParam
 * */
case class ListParam[H, T](head: H, tail: T):
  /**
   * Add an element to the ListParam
   *
   * @param newHead a new value to be added to the list
   * @return an Updated ListParam with the new value at the beginning
   * */
  def append[newT](newHead: newT): ListParam[newT, ListParam[H, T]] = ListParam(newHead, this)
  /**
   * Transform the ListParam to a String
   * 
   * @return the value converted in a String
   * */
  def toSting[H2, T1]: String = {
    val headString: String = this.head.toString
    val tailString: String = this.tail match {
      case Nil => ""
      case nestListParam:ListParam[H2, T1] => ", " + this.tail.toString
    }
    headString + tailString
  }
  /**
   * Recursively give the value of the ListParam to a curried function
   * 
   * @param functionApplied the function applied
   * @return the final result of the function
   * */
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
  /**
   * Constructor to create a list with on element
   * 
   * @param head the first element of the new ListParam
   * @return a ListParam with one value
   * */
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


