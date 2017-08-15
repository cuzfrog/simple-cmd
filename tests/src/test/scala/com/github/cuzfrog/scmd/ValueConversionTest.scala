package com.github.cuzfrog.scmd

import com.github.cuzfrog.scmd.ScmdUtils.CanMerge
import org.junit._
import org.scalacheck.{Test, _}

class ValueConversionTest extends ScalacheckIntegration {

  private def genValue[T](implicit arbitrary: Arbitrary[T]): Gen[List[T]] =
    Gen.listOf(arbitrary.arbitrary) suchThat (_.nonEmpty)
  private val intValue: Gen[List[Int]] = genValue[Int]


  @Test
  def intTest(): Unit = {
    val prop = Prop.forAll(intValue) { values =>
      assertArgsForImplicitConversion(values) && assertArgsForConverter(values)
    }
    assert(prop)
  }
  @Test
  def strTest(): Unit = {
    val prop = Prop.forAll(genValue[String]) { values =>
      assertArgsForImplicitConversion(values) && assertArgsForConverter(values)
    }
    assert(prop)
  }

  @Test
  def booleanTest(): Unit = {
    val prop = Prop.forAll(genValue[Boolean]) { values =>
      assertArgsForImplicitConversion(values) && assertArgsForConverter(values)
    }
    assert(prop)
  }

  @Test
  def booleanTestDirectConverion1(): Unit = {
    val prop = Prop.forAll(genValue[Boolean]) { values =>
      val args = new ValuesForConversionTest(values)
      import ScmdValueImplicitConversion._
      import args._
      (ps: Boolean) == values.head && (os: Boolean) == values.head

    }
    assert(prop)
  }

  @Test
  def booleanTestDirectConverion2(): Unit = {
    val prop = Prop.forAll(genValue[Boolean]) { values =>
      val args = new ValuesForConversionTest(values)
      import ScmdValueConverter._
      import args._
      ps.value == values.head && os.value == values.head

    }
    assert(prop)
  }


  private def assertArgsForImplicitConversion[T](values: Seq[T]): Boolean = {
    import ScmdValueImplicitConversion._
    val args = new ValuesForConversionTest(values)
    import args._
    ps.contains(values.head) &&
      (psm: T) == values.head &&
      (psd: T) == values.head &&
      (pv: Seq[T]) == values &&
      (pvm: Seq[T]) == values &&
      (pvd: Seq[T]) == values &&
      os.contains(values.head) &&
      (osm: T) == values.head &&
      (osd: T) == values.head &&
      (ov: Seq[T]) == values &&
      (ovm: Seq[T]) == values &&
      (ovd: Seq[T]) == values
  }

  private def assertArgsForConverter[T](values: Seq[T]): Boolean = {
    import ScmdValueConverter._
    val args = new ValuesForConversionTest(values)
    import args._
    ps.value.contains(values.head) &&
      (psm.value: T) == values.head &&
      (psd.value: T) == values.head &&
      (pv.value: Seq[T]) == values &&
      (pvm.value: Seq[T]) == values &&
      (pvd.value: Seq[T]) == values &&
      os.value.contains(values.head) &&
      (osm.value: T) == values.head &&
      (osd.value: T) == values.head &&
      (ov.value: Seq[T]) == values &&
      (ovm.value: Seq[T]) == values &&
      (ovd.value: Seq[T]) == values
  }
}

private class ValuesForConversionTest[T](values: Seq[T])
  extends LowLevelImplicitForValueConversion[T] {
  require(values.nonEmpty, "Value is empty.")

  import DummyApi._
  import ScmdUtils._

  val ps = merge(ParameterSingleValue[T], values)
  val psm = merge(ParameterSingleValueMandatory[T], values)
  val psd = merge(ParameterSingleValueWithDefault[T], values)
  val pv = merge(ParameterVariableValue[T], values)
  val pvm = merge(ParameterVariableValueMandatory[T], values)
  val pvd = merge(ParameterVariableValueWithDefault[T], values)

  val os = merge(OptionArgSingleValue[T], values)
  val osm = merge(OptionArgSingleValueMandatory[T], values)
  val osd = merge(OptionArgSingleValueWithDefault[T], values)
  val ov = merge(OptionArgVariableValue[T], values)
  val ovm = merge(OptionArgVariableValueMandatory[T], values)
  val ovd = merge(OptionArgVariableValueWithDefault[T], values)

  private val propValues = values.zipWithIndex.map {
    case (v, idx) => ("key" + idx, v)
  }
  val prop = merge(PropertyArgVariableValue[T], propValues)
  val propd = merge(PropertyArgVariableValueWithDefault[T], propValues)

  implicit def mergeEv2[A <: PropertyArg[T] with VariableValue[(String, T)]]
  : CanMerge[A, Seq[(String, T)]] =
    new CanMerge[A, Seq[(String, T)]] {
      override def merge(a: A, stuff: Seq[(String, T)]): A =
        ScmdUtils.merge(a.asInstanceOf[PropertyArg[T]], stuff).asInstanceOf[A]
    }
}

private sealed trait LowLevelImplicitForValueConversion[T] {
  implicit def mergeEv1[A <: ArgValue[T]]: CanMerge[A, Seq[T]] =
    new CanMerge[A, Seq[T]] {
      override def merge(a: A, stuff: Seq[T]): A =
        ScmdUtils.merge(a.asInstanceOf[ValueArgument[T]], stuff).asInstanceOf[A]
    }
}