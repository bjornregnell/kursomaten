package kursomaten

/* 
Example api-requests:
https://api.lth.lu.se/lot/courses/programmes?kull=false
https://api.lth.lu.se/lot/courses?programmeCode=D&academicYearId=18_19
https://api.lth.lu.se/lot/courses?programmeCode=D
https://api.lth.lu.se/lot/courses/EDAA85/syllabus/25_26
https://kurser.lth.se/lot/about

*/


val dir = 
  val target = os.pwd / "target" /"lot"
  os.makeDir.all(target)
  target

val sleepMillisNotToOverloadServer = 1000

object url:
  val programmes = "https://api.lth.lu.se/lot/courses/programmes?kull=false"

  def courses(programmeCode: String, academicYearId: String) = 
    s"https://api.lth.lu.se/lot/courses?programmeCode=$programmeCode&academicYearId=$academicYearId"

  def syllabus(courseCode: String,  academicYearId: String) = 
    s"https://api.lth.lu.se/lot/courses/$courseCode/syllabus/$academicYearId"
end url 

case class Programme(code: String, sv: String, en: String)
object Programme:
  val progFile = dir / "programmes.csv" 

  def save(ps: Seq[Programme]): Unit = 
    val lines: String = ps.map(p => s"${p.code}\t${p.sv}\t${p.en}").mkString("\n")
    os.write(progFile, lines)
  
  lazy val downloadAll: Seq[Programme] = 
    val data = ujson.read(requests.get(url.programmes).text())
    val result = data match
      case ujson.Arr(buf) => buf.collect:  
          case ujson.Obj(value) => Some:
            Programme(
              code = value("programmeCode").value.toString, 
              sv = value("programme_sv").value.toString,
              en = value("programme_en").value.toString,
            )
      case j => None //throw Exception(s"unexpected json value: $j")
    result.iterator.toSeq.flatten

  lazy val loadAll: Seq[Programme] =
    if os.exists(progFile) then 
      val lines = collection.immutable.ArraySeq.unsafeWrapArray: 
        os.read(progFile).split("\n")
      lines.map: line =>
        val parts = line.split("\t")
        Programme(parts(0), parts(1), parts(2)) 
    else 
      save(downloadAll)
      downloadAll
  
  lazy val allProgIds: Seq[String] = loadAll.map(_.code).sorted 
  
case class Course(code: String, data: Map[String, ujson.Value]) 
object Course:
  def downloadCoursesOfProgram(programmeCode: String, academicYearId: String): Seq[Course] = 
    val data = ujson.read(requests.get(url.courses(programmeCode, academicYearId)).text())
    val result = data match
      case ujson.Arr(buf) => buf.collect:
        case ujson.Obj(value) => Some(Course(value("courseCode").value.toString,value.toMap))
      case j => None //throw Exception(s"unexpected json value: $j")
    result.iterator.toSeq.flatten

  def downloadCoursesOfAllProgrammesOfYear(academicYearId: String): Map[String, Map[String, Course]] = 
    val result = 
      println("Laddar ner alla kurser frö program:")
      for p <- Programme.loadAll yield 
        println(s"  $p")
        val courseMap = downloadCoursesOfProgram(p.code, academicYearId).map(c => c.code -> c).toMap
        println(courseMap.keySet)
        Thread.sleep(sleepMillisNotToOverloadServer)
        p.code -> courseMap
      end for
    result.toMap

  def downloadSyllabus(courseCode: String, academicYearId: String): ujson.Value = 
    ujson.read(requests.get(url.syllabus(courseCode, academicYearId)).text())
