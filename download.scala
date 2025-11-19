package kursomaten

/* 
Example api-requests:
https://api.lth.lu.se/lot/courses/programmes?kull=false
https://api.lth.lu.se/lot/courses?programmeCode=D&academicYearId=18_19
https://api.lth.lu.se/lot/courses?programmeCode=D
https://api.lth.lu.se/lot/courses/EDAA85/syllabus/25_26
https://kurser.lth.se/lot/about

*/

object url:
  val programmes = "https://api.lth.lu.se/lot/courses/programmes?kull=false"

  def courses(academicYearId: String, pid: ProgId) = 
    s"https://api.lth.lu.se/lot/courses?programmeCode=${pid.id}&academicYearId=$academicYearId"

  def syllabus(academicYearId: String, courseCode: String) = 
    s"https://api.lth.lu.se/lot/courses/$courseCode/syllabus/$academicYearId"

end url 

object download:
  def json(fromUrl: String): ujson.Value = ujson.read(requests.get(fromUrl).text())

  def allCoursesOfProgram(academicYearId: String, pid: ProgId): Seq[(String, ujson.Value)] = 
    val data = download.json(url.courses(academicYearId, pid))
    val result = data match
      case ujson.Arr(buf) => buf.collect:
        case o@ujson.Obj(value) => value("courseCode").value.toString -> o
      case ujson.Str(_) => ???
      case ujson.Obj(_) => ???
      case ujson.Num(_) => ???
      case ujson.False  => ???
      case ujson.True   => ???
      case ujson.Null   => ???
    result.iterator.toSeq

  def syllabus(academicYearId: String, courseCode: String): ujson.Value = 
    download.json(url.syllabus(academicYearId, courseCode))

end download