package nl.knaw.dans.lib.error

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Range.Inclusive
import scala.util.{Failure, Success, Try}

class CollectResultsSpec extends FlatSpec with Matchers {

  "collectResults" should "return a Success with a list of results when all elements of the input list are a Success" in {
    val initialCollection: Inclusive = 1 to 10
    val listOfSuccess: IndexedSeq[Try[Int]] = initialCollection.map(Success(_))
    val result: Try[IndexedSeq[Int]] = listOfSuccess.collectResults

    result shouldBe a[Success[_]]
    result.get shouldBe initialCollection
  }

  it should "return a Success of the same type of collection as the input collection type is (IndexedSeq example)" in {
    val listOfSuccess: IndexedSeq[Try[Int]] = (1 to 10).map(Success(_))

    listOfSuccess.collectResults.get shouldBe a[IndexedSeq[_]]
  }

  it should "return a Success of the same type of collection as the input collection type is (Set example)" in {
    val setOfSuccess: Set[Try[Int]] = Set(1, 2, 3, 4).map(Success(_))

    setOfSuccess.collectResults.get shouldBe a[Set[_]]
  }

  it should "return a Failure with the error message if there is one failure in the input" in {
    val collection: List[Try[Int]] = Success(1) :: Success(2) ::
      Failure(new ArrayIndexOutOfBoundsException("foobar")) :: Success(4) :: Nil
    val result = collection.collectResults

    result shouldBe a[Failure[_]]
    (the [CompositeException] thrownBy result.get).getMessage should include ("ArrayIndexOutOfBoundsException: foobar")
  }

  it should "return a Failure of collected error messages if there is more than one failure in the input" in {
    val collection: List[Try[Int]] = Success(1) :: Failure(new IllegalArgumentException("foo")) ::
      Success(3) :: Failure(new NoSuchElementException("bar")) :: Success(5) :: Nil
    val result = collection.collectResults

    result shouldBe a[Failure[_]]
    val errorMessage = (the [CompositeException] thrownBy result.get).getMessage
    errorMessage should include ("IllegalArgumentException: foo")
    errorMessage should include ("NoSuchElementException: bar")
  }

  it should "return a Failure of collected AND FLATTENED error messages when there are nested CompositeExceptions" in {
    val collection1: List[Try[Int]] = Success(1) :: Failure(new IllegalArgumentException("foo")) ::
      Success(3) :: Failure(new NoSuchElementException("bar")) :: Success(5) :: Nil
    val result1 = collection1.collectResults

    val collection = result1 :: Failure(new ArrayIndexOutOfBoundsException("baz")) :: Success(3) :: Nil
    val result = collection.collectResults

    result shouldBe a[Failure[_]]
    val errorMessage = (the [CompositeException] thrownBy result.get).getMessage
    errorMessage should include ("IllegalArgumentException: foo")
    errorMessage should include ("NoSuchElementException: bar")
    errorMessage should include ("ArrayIndexOutOfBoundsException: baz")
    errorMessage should not include "package.CompositeException:"
  }
}
