/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.lib.error

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

class TryExtensionsSpec extends FlatSpec with Matchers {

  "doIfSuccess" should "perform a side effect if the Try is a Success" in {
    val value = 42
    val t: Try[Int] = Success(value)
    val sideEffectingInteger = new AtomicInteger()

    sideEffectingInteger.get() should not be value

    t.doIfSuccess(sideEffectingInteger.set)

    sideEffectingInteger.get() shouldBe value
  }

  it should "not perform a side effect if the Try is a Failure" in {
    val t: Try[Int] = Failure(new IllegalArgumentException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.doIfSuccess(_ => sideEffectingBoolean.set(true))

    sideEffectingBoolean.get() shouldBe false
  }

  "doIfFailure" should "perform a side effect only if the Throwable in the Try is defined in the PartialFunction" in {
    val t: Try[Int] = Failure(new IllegalArgumentException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.doIfFailure {
      case _: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe true
  }

  it should "not perform the side effect if the Throwable in the Try is not defined in the PartialFunction" in {
    val t: Try[Int] = Failure(new NoSuchElementException("foobar"))
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.doIfFailure {
      case _: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe false
  }

  it should "not perform the side effect if the Try is actually a Success" in {
    val t: Try[Int] = Success(42)
    val sideEffectingBoolean = new AtomicBoolean(false)

    t.doIfFailure {
      case _: IllegalArgumentException => sideEffectingBoolean.set(true)
    }

    sideEffectingBoolean.get() shouldBe false
  }

  private case class OnErrorHelperException(defaultValue: Int) extends Exception("onError helper exception")

  "getOrRecover" should "return the actual value when the Try is actually a Success" in {
    val t: Try[Int] = Success(42)

    val result = t.getOrRecover {
      case OnErrorHelperException(default) => default
      case _ => -99
    }

    result shouldBe 42
  }

  it should "be able to distinguish between various exception types using pattern matching on the lambda" in {
    val t: Try[Int] = Failure(OnErrorHelperException(-42))

    val result = t.getOrRecover {
      case OnErrorHelperException(default) => default
      case _ => -99
    }

    result shouldBe -42
  }

  it should "return the correct value when the Try is actually a Failure" in {
    val t: Try[Int] = Failure(new NoSuchElementException())

    val result = t.getOrRecover {
      case OnErrorHelperException(default) => default
      case _ => -99
    }

    result shouldBe -99
  }
}
