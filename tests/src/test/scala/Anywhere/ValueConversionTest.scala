package Anywhere

import com.github.cuzfrog.scmd.{Mandatory, Parameter, SingleValue}

object ValueConversionTest {
  import com.github.cuzfrog.scmd.ScmdValueConverter._

  val a:Parameter[Int] with SingleValue[Int] = ???
  val b:Parameter[Int] with SingleValue[Int] with Mandatory = ???

  val av = a.value
  val bv = b.value
}
