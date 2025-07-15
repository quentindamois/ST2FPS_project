package utils

import scala.sys.process.processInternal.IOException

object Attempt:
  opaque type Result = Either[String, IOException]


object ErrorHandling {
  
 
}
