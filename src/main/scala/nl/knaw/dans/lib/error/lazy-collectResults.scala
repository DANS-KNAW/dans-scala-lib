//import nl.knaw.dans.lib.error.StreamCollectResults
//
//import scala.util.Try
//
//def f(i: Int) = {
//  println(s"f($i)")
//  if (i <= 2) i
//  else throw new Exception("larger than 2")
//}
//
//val stream: Try[Stream[Int]] = (0 to 5).toStream.map(i => Try(f(i))).failFast
//val result = stream.map(_ => ())
//
//println(result)
