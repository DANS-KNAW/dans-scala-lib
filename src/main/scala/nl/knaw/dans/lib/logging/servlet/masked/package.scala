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
package nl.knaw.dans.lib.logging.servlet

import org.scalatra.ScalatraBase

package object masked {

  type MaskedAuthenticationParameters = request.MaskedAuthenticationParameters
  type MaskedAuthorizationHeader = request.MaskedAuthorizationHeader
  type MaskedCookie = request.MaskedCookie
  type MaskedRemoteAddress = request.MaskedRemoteAddress
  type MaskedRequestLogFormatter = request.MaskedRequestLogFormatter
  type MaskedRemoteUser = response.MaskedRemoteUser
  type MaskedResponseLogFormatter = response.MaskedResponseLogFormatter
  type MaskedSetCookie = response.MaskedSetCookie

  private[servlet] trait MaskedLogFormatter extends MaskedRequestLogFormatter with MaskedResponseLogFormatter {
    this: ScalatraBase =>
  }
}
