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
package nl.knaw.dans.lib.logging.servlet.masked

import nl.knaw.dans.lib.logging.servlet.HeaderMapEntry

object Masker {

  def formatCookie(value: String): String = {
    val cookieName = value.replaceAll("=.*", "")
    val cookieValue = value.replaceFirst("[^=]+=", "")
    // replace sequences of chars without dots
    val maskedCookieValue = cookieValue.replaceAll("[^.]+", "****")
    s"$cookieName=$maskedCookieValue"
  }

  /**
   * Formats the value of the request property RemoteAddr.
   * The default implementation masks the network and part of the host within that network.
   * Thus it is no longer identifying a person while we still might have a chance
   * to identify sessions in the log for debugging purposes.
   *
   * https://www.bluecatnetworks.com/blog/ip-addresses-considered-personally-identifiable-information/
   * in case of link rot paste the url at the tail of https://web.archive.org/web/20181030102418/
   *
   * Services without public access might not need to mask.
   */
  def formatRemoteAddress(remoteAddress: String): String = {
    // TODO https://docs.oracle.com/javase/9/docs/api/java/net/Inet6Address.html
    // see also:
    //  * https://stackoverflow.com/questions/3118829/why-request-getremoteaddr-returns-ipv4-or-ipv6-depending-on-context-post-quer
    //  * https://stackoverflow.com/questions/7589526/what-is-the-ip-format-returned-by-servletrequest-getremoteaddr
    remoteAddress.replaceAll("([0-9]+[.]){3}", "**.**.**.")
  }

  def formatCookieHeader(headerName: String)(formatter: String => String): HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase == headerName)(formatter)
  }

  /**
   * Formats the value of headers with a case insensitive name ending with "authorization".
   * This implementation keeps the key like "basic", "digest" and "bearer" but masks the actual
   * credentials.
   */
  def formatAuthorizationHeader: HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase endsWith "authorization")(_.replaceAll(" .+", " *****"))
  }

  def formatRemoteUserHeader: HeaderMapEntry => HeaderMapEntry = {
    formatTuple(_.toLowerCase == "remote_user")(_ => "*****")
  }

  def formatTuple(predicate: String => Boolean)
                         (format: String => String)
                         (tuple: (String, Seq[String])): HeaderMapEntry = {
    tuple match {
      case (name, values) if predicate(name) => name -> values.map(format)
      case otherwise => otherwise
    }
  }
}
