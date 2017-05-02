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

import java.io.{ PrintWriter, StringWriter }

import org.scalatest.{ FlatSpec, Matchers }

class CompositeExceptionSpec extends FlatSpec with Matchers {

  "getMessage" should "return the number of exceptions in the CompositeException" in {
    val ex1 = new Exception("msg1")
    val ex2 = new Exception("msg2", ex1)
    val ex3 = new Exception("msg3", ex2)
    val ex4 = new Exception("msg4", ex3)
    val ex5 = new Exception("msg5")

    val ce = new CompositeException(ex4, ex5)
    ce.getMessage shouldBe "2 exceptions occurred."
  }

  it should "flatten nested CompositeExceptions" in {
    val ex1 = new Exception("msg1")
    val ex2 = new Exception("msg2", ex1)
    val ex3 = new Exception("msg3", ex2)
    val ex4 = new Exception("msg4", ex3)
    val ex5 = new Exception("msg5")
    val ex6 = new Exception("msg6")

    val ce1 = new CompositeException(ex4, ex5)
    val ce2 = new CompositeException(ce1, ex6)
    ce2.getMessage shouldBe "3 exceptions occurred."
  }

  "getCause" should "return a chain of causes" in {
    val ex1 = new Exception("msg1")
    val ex2 = new Exception("msg2", ex1)
    val ex3 = new Exception("msg3", ex2)
    val ex4 = new Exception("msg4", ex3)
    val ex5 = new Exception("msg5")

    val ce = new CompositeException(ex4, ex5)

    ce.getCause.getMessage should include("Chain of causes")
    ce.getCause.getCause should have message "msg4"
    ce.getCause.getCause.getCause should have message "msg3"
    ce.getCause.getCause.getCause.getCause should have message "msg2"
    ce.getCause.getCause.getCause.getCause.getCause should have message "msg1"
    ce.getCause.getCause.getCause.getCause.getCause.getCause should have message "msg5"
    ce.getCause.getCause.getCause.getCause.getCause.getCause.getCause shouldBe null
  }

  it should "not show duplicated causes" in {
    val ex = new Exception("msg")

    val ce = new CompositeException(ex, ex)

    ce.getCause.getMessage should include("Chain of causes")
    ce.getCause.getCause should have message "msg"
    ce.getCause.getCause.getCause shouldBe null
  }

  it should "terminate when finding duplicate causes" in {
    val ex1 = new Exception("msg1")
    val ex2 = new Exception("msg2", ex1)
    val ex3 = new Exception("msg3", ex2)
    val ex4 = new Exception("msg4", ex3)
    val ex5 = new Exception("msg5", ex2)

    val ce = new CompositeException(ex4, ex5)

    ce.getCause.getMessage should include("Chain of causes")
    ce.getCause.getCause should have message "msg4"
    ce.getCause.getCause.getCause should have message "msg3"
    ce.getCause.getCause.getCause.getCause should have message "msg2"
    ce.getCause.getCause.getCause.getCause.getCause should have message "msg1"
    ce.getCause.getCause.getCause.getCause.getCause.getCause.getMessage should include("Duplicate found in causal chain")
    ce.getCause.getCause.getCause.getCause.getCause.getCause.getCause shouldBe null
  }

  "printStackTrace" should "" in {
    val ex1 = new Exception("msg1")
    val ex2 = new Exception("msg2", ex1)
    val ex3 = new Exception("msg3", ex2)
    val ex4 = new Exception("msg4", ex3)
    val ex5 = new Exception("msg5")

    val ce = new CompositeException(ex4, ex5)

    val stringWriter = new StringWriter
    ce.printStackTrace(new PrintWriter(stringWriter))
    val trace = stringWriter.toString

    trace should {
      include("CompositeException: 2 exceptions occurred") and
        include("  ComposedException 1 :\n\tjava.lang.Exception: msg4") and
        include("\tCaused by: java.lang.Exception: msg3") and
        include("\tCaused by: java.lang.Exception: msg2") and
        include("\tCaused by: java.lang.Exception: msg1") and
        include("  ComposedException 2 :\n\tjava.lang.Exception: msg5")
    }
  }
}
