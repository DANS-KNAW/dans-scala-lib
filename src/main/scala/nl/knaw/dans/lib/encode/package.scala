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

import com.google.common.net.PercentEscaper
import java.nio.file.Path
import scala.collection.JavaConverters._

package object encode {

  // PercentEscaper that escapes all characters except '_' and English alphanumerical characters
  private val bagStorePercentEscaper = new PercentEscaper("_", false)

  implicit class PathEncoding(val path: Path) extends AnyVal {

    /**
     * Escapes all characters in path segments, except those declared as "safe characters"
     *
     * @example
     * {{{
     *    import nl.knaw.dans.lib.encode._
     *    import java.nio.file.Paths
     *
     *    val path = Paths.get("an/example/path_123/test-file(#3).txt")
     *    val escapedPath = path.escapePath
     *    // escapedPath: "an/example/path_123/test%2Dfile%28%233%29%2Etxt"
     *  }}}
     * @return an escaped path
     */
    def escapePath(implicit escaper: PercentEscaper = bagStorePercentEscaper): String = {
      path.asScala.map(_.toString.escapeString).mkString("/")
    }
  }

  implicit class StringEncoding(val s: String) extends AnyVal {

    /**
     * Escapes all characters in a string, except those declared as "safe characters"
     *
     * @example
     * {{{
     *    import import nl.knaw.dans.lib.encode._
     *
     *    "10.17026/dans-23n-v3pq".escapeString
     *    // result: "10%2E17026%2Fdans%2D23n%2Dv3pq"
     *  }}}
     * @return an escaped string
     */
    def escapeString(implicit percentEscaper: PercentEscaper = bagStorePercentEscaper): String = {
      percentEscaper.escape(s)
    }
  }
}
