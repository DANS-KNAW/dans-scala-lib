package nl.knaw.dans.lib.error

import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.Stream.Empty
import scala.util.{ Failure, Success, Try }

class FailFastStreamSpec extends FlatSpec with Matchers with Inside {

  "failFast" should "return a Success with a stream of results when all elements of the input stream are a Success" in {
    val initialCollection: Inclusive = 1 to 10
    inside(initialCollection.toStream.map(Success(_)).failFast) {
      case Success(stream) => stream shouldBe initialCollection
    }
  }

  it should "return a Failure with the error if there is one failure in the input" in {
    val collection: Stream[Try[Int]] = Try(1) #:: Try(2) #::
      Try(throw new ArrayIndexOutOfBoundsException("foobar")) #:: Try(4) #:: Empty

    inside(collection.failFast) {
      case Failure(e) =>
        e shouldBe an[ArrayIndexOutOfBoundsException]
        e.getMessage shouldBe "foobar"
    }
  }

  it should "return a Failure with the first error if there are multiple failures in the input" in {
    val collection: Stream[Try[Int]] = Try(1) #:: Try(2) #::
      Try(throw new ArrayIndexOutOfBoundsException("foo")) #::
      Try(throw new ArrayIndexOutOfBoundsException("bar")) #:: Try(5) #:: Empty

    inside(collection.failFast) {
      case Failure(e) =>
        e shouldBe an[ArrayIndexOutOfBoundsException]
        e.getMessage shouldBe "foo"
    }
  }

  it should "execute all the Trys in the Stream if there is no Failure" in {
    val visited: Array[Int] = Array.fill(5)(0)
    val collection: Stream[Try[Int]] = Try { visited.update(0, visited(0) + 1); 1 } #::
      Try { visited.update(1, visited(1) + 1); 2 } #::
      Try { visited.update(2, visited(2) + 1); 3 } #::
      Try { visited.update(3, visited(3) + 1); 4 } #::
      Try { visited.update(4, visited(4) + 1); 5 } #::
      Empty

    collection.failFast shouldBe a[Success[_]] // this is tested before

    visited shouldBe Array.fill(5)(1)
  }

  it should "not execute the Trys in the Stream that come after the first Failure" in {
    val visited: Array[Int] = Array.fill(5)(0)
    val collection: Stream[Try[Int]] = Try { visited.update(0, visited(0) + 1); 1 } #::
      Try { visited.update(1, visited(1) + 1); 2 } #::
      Try { visited.update(2, visited(2) + 1); throw new ArrayIndexOutOfBoundsException("foo") } #::
      Try { visited.update(3, visited(3) + 1); throw new ArrayIndexOutOfBoundsException("bar") } #::
      Try { visited.update(4, visited(4) + 1); 5 } #::
      Empty

    collection.failFast shouldBe a[Failure[_]] // this is tested before

    visited shouldBe Array(1, 1, 1, 0, 0)
  }
}
