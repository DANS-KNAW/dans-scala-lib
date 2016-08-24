package nl.knaw.dans.lib

import scala.util.{Failure, Success, Try}
import org.apache.commons.lang.exception.ExceptionUtils._

package object error {

  /**
   * An exception that bundles a list of `Throwables`.
   *
   * The exception message returns the concatenation of all the
   *
   * @param throwables
   */
  class CompositeException(throwables: List[Throwable])
    extends RuntimeException(throwables.foldLeft("")(
      (msg, t) => s"$msg\n${getMessage(t)} ${getStackTrace(t)}"
    ))

  implicit class ListTryExtensions[T](xs: List[Try[T]]) {
    /**
     * Consolidates a list of `Try`s into either:
     *  - one `Success` with a list of `T`s or
     *  - a `Failure` with a [[CompositeException]] containing a list of exceptions.
     *
     *  Example:
     *  {{{
     *    import java.io.{File, FileNotFoundException}
     *    import scala.util.{Failure, Success, Try}
     *    import nl.knaw.dans.lib.error._
     *
     *    def getFileLengths(files: List[File]): List[Try[Long]] =
     *     files.map {case f =>
     *        if(f.exists) Success(f.length)
     *        else Failure(new FileNotFoundException())
     *     }
     *
     *    // Fill in existing and/or non-existing file
     *    val someFileList = List(new File("x"), new File("y"), new File("z"))
     *
     *    getFileLengths(someFileList).collectResults()
     *      .map(_.mkString(", "))
     *      .recover { case t => println(t.getMessage) }
     *  }}}
     *
     * @return a consolidated result
     */
    def collectResults(): Try[List[T]] =
      if (xs.exists(_.isFailure))
        Failure(new CompositeException(xs.collect { case Failure(e) => e }))
      else
        Success(xs.map(_.get))
  }
}
