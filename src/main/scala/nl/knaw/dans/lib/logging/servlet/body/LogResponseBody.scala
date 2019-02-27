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

import javax.servlet.http.HttpServletResponse
import nl.knaw.dans.lib.logging.servlet.ResponseLogFormatter
import org.scalatra.{ ActionResult, ScalatraBase }

trait LogResponseBody extends ResponseLogFormatter {
  this: ScalatraBase =>
  
  def shouldLogResponseBody(response: HttpServletResponse): Boolean

  override protected def formatResponseLog(actionResult: ActionResult): String = {
    val body = if (shouldLogResponseBody(response)) s"; body=[${actionResult.body}]"
               else ""
    
    super.formatResponseLog(actionResult) + body
  }
}