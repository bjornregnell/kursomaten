package kursomaten 

case class ProgId(id: String)

case class AcademicYear(fall_spring: String)
object AcademicYear:
  def current: String =
    import java.util.{GregorianCalendar, Calendar}
    val year: Int = Calendar.getInstance.get(Calendar.YEAR)
    val monthZeroBased: Int = Calendar.getInstance.get(Calendar.MONTH)
    val startYear = if monthZeroBased < 6 then year - 1 else year
    val fall = startYear.toString.takeRight(2)
    val spring = (startYear + 1).toString.takeRight(2)
    s"${fall}_${spring}"

case class Programme(pid: ProgId, sv: String, en: String)
object Programme:
  def save(ps: Seq[Programme]): Unit = 
    val lines: String = ps.map(p => s"${p.pid.id}\t${p.sv}\t${p.en}").mkString("\n")
    os.write(progFile, lines)
  
  lazy val downloadAll: Seq[Programme] = 
    val data = download.json(url.programmes)
    val result = data match
      case ujson.Arr(buf) => buf.collect:  
          case ujson.Obj(value) => Some:
            Programme(
              pid = ProgId(value("programmeCode").value.toString), 
              sv = value("programme_sv").value.toString,
              en = value("programme_en").value.toString,
            )
      case j => None //throw Exception(s"unexpected json value: $j")
    result.iterator.toSeq.flatten

  lazy val allProgs: Seq[Programme] =
    if os.exists(progFile) then 
      val lines = collection.immutable.ArraySeq.unsafeWrapArray: 
        os.read(progFile).split("\n")
      lines.map: line =>
        val parts = line.split("\t")
        Programme(ProgId(parts(0)), parts(1), parts(2)) 
    else 
      save(downloadAll)
      downloadAll
  

  lazy val allProgIds: Seq[ProgId] = allProgs.map(_.pid).sortBy(_.id) 
end Programme



