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
package nl.knaw.dans.lib.logging.servlet.body

import nl.knaw.dans.lib.logging.servlet.ResponseLogFormatter
import nl.knaw.dans.lib.string._
import org.scalatra.{ ActionResult, ScalatraBase }

import scala.language.postfixOps

private[servlet] trait LogResponseBody extends ResponseLogFormatter {
  this: ScalatraBase =>

  protected def formatResponseBody(actionResult: ActionResult): Option[Any]

  override protected def formatResponseLog(actionResult: ActionResult): String = {
    val formattedBody = formatResponseBody(actionResult)
      .withFilter(Unit !=)
      .withFilter(() !=)
      .map(String.valueOf)
      .map {
        case b if b.isBlank => "; body=[]"
        case b => s"; body=[$b]"
      }
      .getOrElse("")

    super.formatResponseLog(actionResult) + formattedBody
  }
}
