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



