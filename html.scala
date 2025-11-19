package kursomaten

/** use Jsoup to parse html and get stuff out **/

type Html = org.jsoup.nodes.Document

object Html:
  import scala.jdk.CollectionConverters.*
  import org.jsoup.Jsoup 
  import collection.immutable.ArraySeq.{unsafeWrapArray as wrap}

  extension (s: String) 
    def parse: Html = Jsoup.parseBodyFragment(s)
    def linesFromBreaks: Seq[String] = wrap(s.split("\\\\n").map(_.trim).filterNot(_.isEmpty))

  extension (soup: Html)
    def h1: Seq[String] = soup.select("h1").eachText().asScala.toList
    def h2Contents: Seq[(String, Seq[String])] = 
      val result = soup.select("h2").asScala.map: h2 =>
        val key = h2.text().trim
        val sb = StringBuilder()
        var el = h2.nextElementSibling()
        while el != null && el.tagName() != "h2" do
          val txt = el.text().trim
          if txt.nonEmpty then
            if sb.length > 0 then sb.append("\\n")
            sb.append(txt)
          end if 
          el = el.nextElementSibling()
        end while 
        key -> sb.toString().linesFromBreaks
      result.toSeq

end Html 