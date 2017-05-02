package nl.knaw.dans.lib.error

import java.io.{ PrintStream, PrintWriter }

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * An exception that bundles a collection of `Throwable`s.
 *
 * The exception message returns the concatenation of all the `Throwable`s' messages.
 *
 * @param throwables a collection of `Throwable`s
 */
class CompositeException(private val throwables: Seq[Throwable]) extends RuntimeException {

  @tailrec
  private def flattenExceptions(throwables: Seq[Throwable], result: mutable.ListBuffer[Throwable] = mutable.ListBuffer.empty): Seq[Throwable] = {
    throwables match {
      case Seq() => result
      case Seq(CompositeException(ths), tail@_*) => flattenExceptions(ths ++ tail, result)
      case Seq(th, tail@_*) => flattenExceptions(tail, result += th)
    }
  }

  val exceptions: Seq[Throwable] = flattenExceptions(throwables)

  lazy private val msg = exceptions.size match {
    case 0 => "No exceptions occurred."
    case 1 => "1 exception occurred."
    case n => s"$n exceptions occurred."
  }
  override def getMessage: String = msg

  lazy private val cause: Throwable = {
    val emptyThrowable: Throwable = new RuntimeException("Chain of causes for CompositeException In Order Received =>")

    def listCauses(th: Throwable): List[Throwable] = {
      Option(th.getCause)
        .filterNot(th ==)
        .map(root => {
          Stream.iterate((null: Throwable, root)) {
            case (_, curr) => (curr, curr.getCause)
          }.takeWhile {
            case (prev, curr) => curr != null && curr != prev
          }.map { case (_, curr) => curr }.toList
        })
        .getOrElse(List.empty)
    }

    def getRootCause(th: Throwable): Throwable = {
      listCauses(th) match {
        case Nil => th
        case cs => cs.last
      }
    }

    exceptions.foldLeft((emptyThrowable, Set.empty[Throwable])) {
      case ((chain, seen), th) if seen contains th => (chain, seen)
      case ((chain, seen), th) =>
        // check if any of them have been seen before
        val (seen2, th2) = listCauses(th).foldLeft((seen + th, th)) {
          case ((seenThs, _), c) if seenThs contains c =>
            // already seen this outer Throwable so skip
            (seenThs, new RuntimeException("Duplicate found in causal chain so cropping to prevent loop ..."))
          case ((seenThs, e), c) => (seenThs + c, e)
        }

        // we now have 'th2' as the last in the chain
        try { chain.initCause(th2) }
        catch { case _: Throwable => } // discard error

        (getRootCause(chain), seen2)
    }

    emptyThrowable
  }
  override def getCause: Throwable = cause

  override def printStackTrace(): Unit = {
    printStackTrace(System.err)
  }

  override def printStackTrace(s: PrintStream): Unit = {
    s.synchronized {
      s.println(stackTraceString)
    }
  }

  override def printStackTrace(s: PrintWriter): Unit = {
    s.synchronized {
      s.println(stackTraceString)
    }
  }

  private def stackTraceString: String = {
    val builder = new StringBuilder(128)
    builder.append(this).append('\n')

    for (elem <- getStackTrace) {
      builder.append("\tat ").append(elem).append('\n')
    }

    for ((ex, i) <- exceptions.zipWithIndex) {
      builder.append("  ComposedException ").append(i + 1).append(" :\n")
      appendStackTrace(ex, "\t")
    }

    @tailrec
    def appendStackTrace(th: Throwable, prefix: String): Unit = {
      builder.append(prefix).append(th).append('\n')

      for (elem <- th.getStackTrace) {
        builder.append("\t\tat ").append(elem).append('\n')
      }

      if (th.getCause != null) {
        builder.append("\tCaused by: ")
        appendStackTrace(th.getCause, "")
      }
    }

    builder.toString()
  }
}

object CompositeException {
  def apply(errors: Throwable*): CompositeException = new CompositeException(errors)

  def unapply(arg: CompositeException): Option[Seq[Throwable]] = Option(arg.throwables)
}
