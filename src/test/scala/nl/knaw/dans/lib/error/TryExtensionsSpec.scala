package nl.knaw.dans.lib.error

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class TryExtensionsSpec extends FlatSpec with Matchers {

  "ifSuccess" should "perform a side effect if the Try is a Success" in {
    val value = 42
    val t: Try[Int] = Success(value)
    val sideEffectingInteger = new AtomicInteger()

    sideEffectingInteger.get() should not be value

    t.ifSuccess(sideEffectingInteger.set)

    sideEffectingInteger.get() shouldBe value
  }

  it should "not perform a side effect if the Try is a Failure" in {
    val t: Try[Int] = Failure(new IllegalArgumentException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.ifSuccess(_ => sideEffectingBoolean.set(true))

    sideEffectingBoolean.get() shouldBe false
  }

  "ifFailure" should "perform a side effect only if the Throwable in the Try is defined in the PartialFunction" in {
    val t: Try[Int] = Failure(new IllegalArgumentException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.ifFailure {
      case e: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe true
  }

  it should "not perform the side effect if the Throwable in the Try is not defined in the PartialFunction" in {
    val t: Try[Int] = Failure(new NoSuchElementException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.ifFailure {
      case e: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe false
  }

  it should "not perform the side effect if the Try is actually a Success" in {
    val t: Try[Int] = Success(42)
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.ifFailure {
      case e: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe false
  }
}
