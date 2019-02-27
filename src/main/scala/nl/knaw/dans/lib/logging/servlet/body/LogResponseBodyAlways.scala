package nl.knaw.dans.lib.logging.servlet.body

import javax.servlet.http.HttpServletResponse
import org.scalatra.ScalatraBase

trait LogResponseBodyAlways extends LogResponseBody {
  this: ScalatraBase =>

  override def shouldLogResponseBody(response: HttpServletResponse): Boolean = true
}
