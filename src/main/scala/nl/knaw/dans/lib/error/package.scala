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

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

package object error {

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
      xs.collect { case Failure(e) => e }.toSeq match {
        case Seq() => Success(xs.map(_.get).to(canBuildFrom))
        case Seq(e) => Failure(e)
        case es => Failure(CompositeException(es))
      }
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
     *   def performSideEffect(size: Long): Unit = println(s"size = $$size")
     *
     *   // Fill in existing or non-existing file
     *   val file = new File("x")
     *
     *   getFileLength(file)
     *     .doIfSuccess(size => performSideEffect(size))
     * }}}
     *
     * @param sideEffect the side effecting function to be applied
     * @return the original `Try`
     */
    def doIfSuccess[A](sideEffect: T => A): Try[T] = {
      t match {
        case Success(value) => Try {
          sideEffect(value)
          value
        }
        case failure => failure
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
     * @param sideEffect the side effecting function to be applied
     * @return the original `Try`
     */
    def doIfFailure[A](sideEffect: PartialFunction[Throwable, A]): Try[T] = {
      t match {
        case Failure(throwable) if sideEffect.isDefinedAt(throwable) => Try {
          sideEffect(throwable)
          throw throwable
        }
        case other => other
      }
    }

    /**
     * Terminating operator for `Try` that converts the `Failure` case in a value.
     *
     * Example:
     * {{{
     *   import nl.knaw.dans.lib.error.TryExtensions
     *   import java.io.{ File, FileNotFoundException }
     *   import scala.util.{Failure, Success, Try}
     *
     *   def getFileName(file: File): Try[String] =
     *     if (file.exists) Success(file.getName)
     *     else Failure(new FileNotFoundException())
     *
     *   // Fill in existing or non-existing file
     *   val file = new File("x")
     *
     *   getFileName(file)
     *     .getOrRecover {
     *       // error codes
     *       case _: FileNotFoundException => "<file not found>"
     *       case _ => "<an internal error occurred>"
     *     }
     * }}}
     *
     * @param recover recovers a `Throwable` and turns it into a value of type `T`
     * @return either the value inside `Try` (on success) or the result of `recover` (on failure)
     */
    def getOrRecover[S >: T](recover: Throwable => S): S = {
      t match {
        case Success(value) => value
        case Failure(throwable) => recover(throwable)
      }
    }

    /**
     * Combines the results of `this` and `other` by either applying the function in `this` to the
     * value in `other` (if both are a `Success`) or returning a `Failure` (in case either one or
     * both of them are a `Failure`).
     *
     * Example:
     * {{{
     *   import nl.knaw.dans.lib.error.TryExtensions
     *
     *   import scala.util.{Failure, Success, Try}
     *
     *   def add(i: Int)(j: Int): Int = i + j
     *
     *   val input1 = Try(1)
     *   val input2 = Try(2)
     *
     *   Try { add _ }.combine(input1).combine(input2)
     * }}}
     *
     * @param other the value to be applied to the function in `this`
     * @param ev evidence that `T` is a function
     * @tparam S the input of the function in `this`
     * @tparam R the output of the function in `this`
     * @return the result of applying the value in `this` to `other`
     */
    def combine[S, R](other: Try[S])(implicit ev: T <:< (S => R)): Try[R] = {
      (t, other) match {
        case (Success(f), Success(s)) => Try { f(s) }
        case (Success(_), Failure(e)) => Failure(e)
        case (Failure(e), Success(_)) => Failure(e)
        case (Failure(e1), Failure(e2)) => Failure(new CompositeException(e1, e2))
      }
    }
  }
}
