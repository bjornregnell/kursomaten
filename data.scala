package kursomaten 

case class ProgId(id: String)

case class AcademicYear(fall_spring: String)
object AcademicYear:
  def current: String =
    import java.util.{GregorianCalendar, Calendar}
    val year: Int = Calendar.getInstance.get(Calendar.YEAR)
    val monthZeroBased: Int = Calendar.getInstance.get(Calendar.MONTH)
    val startYear = if monthZeroBased <= 6 then year - 1 else year
    val fall = startYear.toString.takeRight(2)
    val spring = (startYear + 1).toString.takeRight(2)
    s"${fall}_${spring}"

case class Programme(pid: ProgId, sv: String, en: String)

enum Lang { case En, Sv}

case class Course(code: String, year: AcademicYear, prog: ProgId):
  def coursePath = dir / year.fall_spring / prog.id / s"$code-syllabus.json"
  def loadJsonSyllabus() = ujson.read(os.read(coursePath))
  def loadSyllabus(): Map[Lang, Syllabus] = 
    val pairs: Seq[(Lang, Syllabus)] = loadJsonSyllabus() match
      case ujson.Obj(value) => 
        val en = util.Try(Lang.En -> Syllabus(value("en").toString)).toOption
        val sv = util.Try(Lang.Sv -> Syllabus(value("sv").toString)).toOption
        Seq(en, sv).flatten
      case other => Seq() 
    pairs.toMap


case class Syllabus(htmlSoup: String):
  import Html.*
  
  lazy val html: Html = htmlSoup.parse
  
  lazy val titles: Seq[String] = html.h1.map(_.linesFromBreaks).flatten
  
  lazy val h2Contents: Seq[(String, Seq[String])] = html.h2Contents

  lazy val h2Map: Map[String, Seq[String]] = h2Contents.toMap
  
  lazy val headings: Seq[String] = h2Map.keys.toSeq

  lazy val lines: Seq[String] = html.text().linesFromBreaks

  lazy val text: String = lines.mkString("\n")