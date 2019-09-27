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
package nl.knaw.dans.lib

import java.util.UUID

package object string {

  implicit class StringExtensions(val s: String) extends AnyVal {

    /**
     * Return true when all characters in the `String` are classified as ''whitespace'' or if
     * the `String` is `null`, false otherwise.
     *
     * @return whether whether the `String` is `null` or all characters are ''whitespace''
     */
    def isBlank: Boolean = {
      s match {
        case null | "" => true
        case _ => s.forall(Character.isWhitespace)
      }
    }

    /**
     * Wraps the `String` in an `Option` and returns `Option.empty` if all characters are ''whitespace''
     * or if the `String` is `null`.
     *
     * @return `None` if all characters are whitespace or if the `String` is `null`, `Some(s)` otherwise
     */
    def toOption: Option[String] = {
      if (s.isBlank) Option.empty
      else Option(s)
    }

    /**
     * Returns the empty `String` if all characters are ''whitespace'' or if the `String` is `null`,
     * the original `String` otherwise.
     *
     * @return the empty `String` if all characters are ''whitespace'' or if the `String` is `null`,
     *         the input otherwise
     */
    def emptyIfBlank: String = s.toOption.getOrElse("")

    /**
     * Parses the `String` into a `UUID` if it is well-formed, meaning it conforms to the regex
     * `^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$`.
     * 
     * @return a `UUID` if it is well-formed; an error otherwise.
     */
    def toUUID: Either[UUIDError, UUID] = {
      val uuidRegex = "^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$"
      if (s matches uuidRegex)
        Right(UUID.fromString(s))
      else
        Left(UUIDError(s))
    }
  }
  
  case class UUIDError(s: String) extends Exception(s"String '$s' is not a UUID")
}
