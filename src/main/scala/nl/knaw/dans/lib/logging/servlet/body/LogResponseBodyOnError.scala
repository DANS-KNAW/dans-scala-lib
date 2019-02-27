package nl.knaw.dans.lib.logging.servlet.body

import javax.servlet.http.HttpServletResponse
import org.scalatra.ScalatraBase

trait LogResponseBodyOnError extends LogResponseBody {
  this: ScalatraBase =>

  override def shouldLogResponseBody(response: HttpServletResponse): Boolean = {
    val status = response.status
    400 <= status && status <= 599
  }
}
