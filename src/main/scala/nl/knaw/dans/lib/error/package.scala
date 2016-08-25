package nl.knaw.dans.lib

import org.apache.commons.lang.exception.ExceptionUtils._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

package object error {

  /**
   * An exception that bundles a collection of `Throwable`s.
   *
   * The exception message returns the concatenation of all the `Throwable`s' messages.
   *
   * @param throwables a collection of `Throwable`s
   */
  class CompositeException(throwables: Traversable[Throwable])
    extends RuntimeException(throwables.foldLeft("")(
      (msg, t) => s"$msg\n${getMessage(t)} ${getStackTrace(t)}"
    ))

  implicit class IterableTryExtensions[M[_], T](xs: M[Try[T]])(implicit ev: M[Try[T]] <:< Traversable[Try[T]]) {
    /**
     * Consolidates a list of `Try`s into either:
     *  - one `Success` with a list of `T`s or
     *  - a `Failure` with a [[CompositeException]] containing a list of exceptions.
     *
     *  Example:
     *  {{{
     *    import java.io.{File, FileNotFoundException}
     *    import nl.knaw.dans.lib.error._
     *    import scala.util.{Failure, Success, Try}
     *
     *    def getFileLengths(files: List[File]): List[Try[Long]] =
     *     files.map(f =>
     *        if(f.exists) Success(f.length)
     *        else Failure(new FileNotFoundException()))
     *
     *    // Fill in existing and/or non-existing file
     *    val someFileList = List(new File("x"), new File("y"), new File("z"))
     *
     *    getFileLengths(someFileList).collectResults
     *      .map(_.mkString(", "))
     *      .recover { case t => println(t.getMessage) }
     *  }}}
     *
     * @param canBuildFrom an implicit value of class `CanBuildFrom` which determines
     *    the result class `M[T]` from the input type.
     * @return a consolidated result
     */
    def collectResults(implicit canBuildFrom: CanBuildFrom[Nothing, T, M[T]]): Try[M[T]] = {
      if (xs.exists(_.isFailure))
        Failure(new CompositeException(xs.collect { case Failure(e) => e }))
      else
        Success(xs.map(_.get).to(canBuildFrom))
    }
  }
}
