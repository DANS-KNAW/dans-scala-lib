/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.lib.logging

import org.scalatra.{ ActionResult, ScalatraBase }

/**
 * Package for logging servlet requests and responses in a standardized format.
 *
 * ==Rationale==
 * The request are logged with `before` filters. An `after` filter would not see an
 * `org.scalatra.ActionResult`. Its values are not saved in the implicit response provided by
 * `org.scalatra.ScalatraBase` as done by a trait for a `org.scalatra.ScalatraServlet` that extends
 * `org.scalatra.auth.ScentrySupport`.
 * See the last extensive readme version (documentation moved into an incomplete book and guides)
 * https://github.com/scalatra/scalatra/blob/6a614d17c38d19826467adcabf1dc746e3192dfc/README.markdown
 * sections #filters #action.
 *
 * Using `org.scalatra.util.RequestLogging` broke unit test in easy-deposit-api as it added a session
 * header to some responses.
 *
 * ==Usage==
 * To enable servlet logging, add the `ServletLogger` trait to the servlet definition, together with
 * a `RequestLogFormatter`, a `ResponseLogFormatter` and a `com.typesafe.scalalogging.Logger`.
 *
 * In the example below we use `DebugEnhancedLogging` for the latter. The `PlainLogFormatter`
 * and `MaskedLogFormatter` both implement the two LogFormatters. The latter masks privacy sensitive
 * values like user names, passwords and remote addresses.
 * 
 * To also log the body of a response, either add `LogResponseBodyAlways` or `LogResponseBodyOnError`.
 *
 * When you want to mask less, for example to debug tests, add individual
 * parts of the `MaskedLogFormatter` to the `PlainLogFormatter`.
 *
 * {{{
 *    import nl.knaw.dans.lib.logging.DebugEnhancedLogging
 *    import nl.knaw.dans.lib.logging.servlet._
 *    import org.scalatra.{ Ok, ScalatraServlet }
 *
 *    // example that logs plain values of request and response details
 *    class ExampleServlet extends ScalatraServlet
 *      with ServletLogger
 *      with PlainLogFormatter
 *      with LogResponseBodyOnError
 *      with DebugEnhancedLogging {
 *
 *      get("/") {
 *        Ok("All is well").logResponse
 *      }
 *    }
 *
 *    // example that masks privacy sensitive values when logging request and response details
 *    class MaskedServlet extends ScalatraServlet
 *      with ServletLogger
 *      with MaskedLogFormatter
 *      with DebugEnhancedLogging {
 *
 *      get("/") {
 *        Ok("All is well").logResponse
 *      }
 *    }
 *
 *    // example that masks only the remote address (request) and remote user (response) values
 *    import nl.knaw.dans.lib.logging.servlet.masked._
 *    class MaskedServlet extends ScalatraServlet
 *      with ServletLogger
 *      with PlainLogFormatter
 *      with MaskedRemoteAddress
 *      with MaskedRemoteUser
 *      with DebugEnhancedLogging {
 *
 *      get("/") {
 *        Ok("All is well").logResponse
 *      }
 *    }
 * }}}
 *
 * ==Extension==
 *
 * For a variant of a `MaskedLogFormatter` component you can create a trait that extends either
 * `RequestLogExtensionBase` or `ResponseLogExtensionBase`. In this trait, implement the desired
 * method (`formatHeader`, `formatParameter`, `formatResponseHeader` or `formatActionHeader`),
 * using an `abstract override` (which is important for mixing in this formatter with the others).
 * Keep in mind that the formatter should only return a formatted version of the input and not
 * mutate or perform side effects on it.
 *
 * {{{
 *    trait MyCustomRequestHeaderFormatter extends RequestLogExtensionBase {
 *      this: ScalatraBase =>
 *
 *      abstract override protected def formatHeader(header: HeaderMapEntry): HeaderMapEntry = {
 *        super.formatHeader(header) match {
 *          case (name, values) if name.toLowerCase == "my-header" =>
 *            name -> values.map(formatMyHeader)
 *          case otherwise => otherwise
 *        }
 *      }
 *
 *      private def formatMyHeader(value: String): String = {
 *        // return some formatting of 'value'
 *      }
 *    }
 *
 *    class ExampleServlet extends ScalatraServlet
 *      with ServletLogger
 *      with PlainLogFormatter
 *      with MyCustomRequestHeaderFormatter
 *      with DebugEnhancedLogging {
 *
 *      get("/") {
 *        Ok("All is well").logResponse
 *      }
 *    }
 * }}}
 */
package object servlet {

  type HeaderMap = Map[String, Seq[String]]
  type HeaderMapEntry = (String, Seq[String])
  type ActionHeadersMap = Map[String, String]
  type ActionHeaderEntry = (String, String)
  type MultiParamsEntry = (String, Seq[String])

  implicit private[servlet] class MapExtensions[K, V](val stringMap: Map[K, V]) extends AnyVal {

    /** @return a toString like value with less class names */
    private[servlet] def makeString: String = {
      def formatSeq[T](seq: Seq[T]): String = {
        seq.map {
          case t: Seq[_] => formatSeq(t)
          case t => t
        }.mkString("[", ", ", "]")
      }

      stringMap.map {
        case (k, v: Seq[_]) => k -> formatSeq(v)
        case (k, v: Map[_, _]) => k -> v.makeString
        case kv => kv
      }.mkString("[", ", ", "]")
    }
  }

  /**
   * Convenience syntax for logging a response.
   *
   * @param actionResult the `ActionResult to be logged`
   */
  implicit class LogResponseSyntax(val actionResult: ActionResult) extends AnyVal {
    /**
     * Performs the side effect of the logging of the response, contained in the given `ActionResult`.\
     *
     * @example
     * {{{
     *   import nl.knaw.dans.lib.logging.DebugEnhancedLogging
     *   import nl.knaw.dans.lib.logging.servlet._
     *   import org.scalatra.{ Ok, ScalatraServlet }
     *
     *   class ExampleServlet extends ScalatraServlet with ServletLogger with DebugEnhancedLogging {
     *     get("/") {
     *       Ok("All is well").logResponse
     *     }
     *   }
     * }}}
     * @param responseLogger the logger with which to format/output the response
     * @return the original `ActionResult`
     */
    def logResponse(implicit responseLogger: AbstractServletLogger): ActionResult = {
      responseLogger.logResponse(actionResult)
    }
  }

  trait PlainLogFormatter extends RequestLogFormatter with ResponseLogFormatter {
    this: ScalatraBase =>
  }

  type MaskedLogFormatter = masked.MaskedLogFormatter

  type LogResponseBody = body.LogResponseBody
  type LogResponseBodyAlways = body.LogResponseBodyAlways
  type LogResponseBodyOnError = body.LogResponseBodyOnError
}
