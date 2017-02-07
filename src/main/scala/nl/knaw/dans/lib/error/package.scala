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
  case class CompositeException(throwables: Traversable[Throwable])
    extends RuntimeException(throwables.foldLeft("")(
      (msg, t) => s"$msg\n${getMessage(t)} ${getStackTrace(t)}"
    ))

  implicit class TraversableTryExtensions[M[_], T](xs: M[Try[T]])(implicit ev: M[Try[T]] <:< Traversable[Try[T]]) {
    /**
     * Consolidates a list of `Try`s into either:
     *  - one `Success` with a list of `T`s or
     *  - a `Failure` with a [[CompositeException]] containing a list of exceptions.
     *
     *  @example
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
     *    getFileLengths(someFileList)
     *      .collectResults
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
        Failure(CompositeException(xs.flatMap {
          case Success(_) => Traversable.empty
          case Failure(CompositeException(ts)) => ts
          case Failure(e) => Traversable(e)
        }))
      else
        Success(xs.map(_.get).to(canBuildFrom))
    }
  }

  implicit class FailFastStream[T](val stream: Stream[Try[T]]) {
    /**
     * Evaluates a `Stream` of `Try`s into either:
     *  - one `Success` with a stream of `T`s or
     *  - a `Failure` with the first exception contained in the stream
     *
     * Note that when the `Stream` encounters a `Failure`, the remaining elements are not evaluated.
     * The `Failure` is returned immediately.
     *
     * @example
     * {{{
     *   import nl.knaw.dans.lib.error._
     *
     *   import scala.util.Try
     *
     *   def f(i: Int) = {
     *     if (i <= 2) i
     *     else throw new Exception(s"$i is larger than 2")
     *   }
     *
     *   val stream: Try[Stream[Int]] = (0 to 5).toStream.map(i => Try(f(i))).failFast
     *   println(stream)
     *   // prints: Failure(java.lang.Exception: 3 is larger than 2)
     * }}}
     *
     * @return a evaluated result
     */
    def failFast: Try[Stream[T]] = {
      stream.find(_.isFailure)
        .map(_.flatMap(s => Failure(new IllegalArgumentException(s"Success should never occur here, but got Success($s)"))))
        .getOrElse(Success(stream.map(_.get)))
    }
  }

  implicit class TryExtensions[T](val t: Try[T]) extends AnyVal {
    /**
     * Applies the given side effecting function if and only if this is a `Success`.
     *
     * Example:
     * {{{
     *   import nl.knaw.dans.lib.error.TryExtensions
     *
     *   import scala.util.{Failure, Success, Try}
     *
     *   def getFileLength(file: File): Try[Long] =
     *     if (file.exists) Success(file.length)
     *     else Failure(new FileNotFoundException())
     *
     *   def performSideEffect(size: Long): Unit = println(s"size = $size")
     *
     *   // Fill in existing or non-existing file
     *   val file = new File("x")
     *
     *   getFileLength(file)
     *     .doIfSuccess(size => performSideEffect(size))
     * }}}
     *
     * @param f the side effecting function to be applied
     * @return the original `Try`
     */
    def doIfSuccess(f: T => Unit): Try[T] = {
      t match {
        case success @ Success(x) => Try {
          f(x)
          return success
        }
        case e => e
      }
    }

    /**
     * Applies the given side effecting `PartialFunction` if and only if this is a `Failure` and
     * the `Throwable` is defined in the `PartialFunction`.
     *
     * Example:
     * {{{
     *   import nl.knaw.dans.lib.error.TryExtensions
     *
     *   import scala.util.{Failure, Success, Try}
     *
     *   def getFileLength(file: File): Try[Long] =
     *     if (file.exists) Success(file.length)
     *     else Failure(new FileNotFoundException())
     *
     *   // Fill in existing or non-existing file
     *   val file = new File("x")
     *
     *   getFileLength(file)
     *     .doIfFailure {
     *       case e: FileNotFoundException => println(e.getMessage)
     *     }
     * }}}
     *
     * @param f the side effecting function to be applied
     * @return the original `Try`
     */
    def doIfFailure(f: PartialFunction[Throwable, Unit]): Try[T] = {
      t match {
        case failure @ Failure(e) if f.isDefinedAt(e) => Try {
          f(e)
          return failure
        }
        case x => x
      }
    }

    /**
     * Terminating operator for `Try` that converts the `Failure` case in a value.
     *
     * Example:
     * {{{
     *   import nl.knaw.dans.lib.error.TryExtensions
     *
     *   import scala.util.{Failure, Success, Try}
     *
     *   def getFileLength(file: File): Try[Long] =
     *     if (file.exists) Success(file.length)
     *     else Failure(new FileNotFoundException())
     *
     *   // Fill in existing or non-existing file
     *   val file = new File("x")
     *
     *   getFileLength(file)
     *     .getOrRecover {
     *       // error codes
     *       case _: FileNotFoundException => -1
     *       case _ => -99
     *     }
     * }}}
     *
     * @param handle converts `Throwable` to a value of type `T`
     * @return either the value inside `Try` (on success) or the result of `handle` (on failure)
     */
    def getOrRecover[S >: T](handle: Throwable => S): S = {
      t match {
        case Success(value) => value
        case Failure(throwable) => handle(throwable)
      }
    }
  }
}
