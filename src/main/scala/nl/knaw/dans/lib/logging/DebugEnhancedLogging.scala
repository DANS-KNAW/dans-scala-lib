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
package nl.knaw.dans.lib.logging

import com.typesafe.scalalogging.LazyLogging

trait DebugEnhancedLogging extends LazyLogging {
  /**
   * Logs a trace level statement with the function name, parameter names and parameter values.
   *
   * Example:
   * {{{
   *   // Function definition
   *   def someFunction(s: String, i: Int) = {
   *    trace(s, i)
   *    // Your code ...
   *   }
   *
   *   // Function call
   *   someFunction("My input string", 2)
   *
   *   // This will result in a logging line like this (if the TRACE log level is enabled):
   *   [TRACE] someFunction [(s, i)]:  ("My input string",2)
   * }}}
   *
   * @param value list of parameter values
   * @param enclosing implicitly passed in context from the sourcecode library
   * @tparam V type parameter, no idea how it works
   */
  def trace[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Name): Unit = {
    logger.trace(s"${enclosing.value} [${value.source}]: ${value.value}")
  }


  /**
   * Logs a custom debug level statement with the calling function name prefixed
   *
   * Example:
   * {{{
   *   // Function definition
   *   def someFunction(s: String, i: Int) = {
   *    // Your code ...
   *    debug("Doing important stuff here")
   *   }
   *
   *
   *   // This will result in a logging line like this (if the DEBUG log level is enabled):
   *   [DEBUG] someFunction: Doing important stuff here
   * }}}
   * @param msg debug message to be logged
   * @param enclosing implicitly passed in context from the sourcecode library
   */
  def debug(msg: String)(implicit enclosing: sourcecode.Name): Unit = {
    logger.debug(s"${enclosing.value}: $msg")
  }
}
