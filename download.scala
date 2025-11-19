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
    val result = download.json(url.courses(academicYearId, pid)) match
      case ujson.Arr(buf) => buf.collect:
        case obj@ujson.Obj(value) => value("courseCode").value.toString -> obj
      case other => Seq()
    result.toSeq 
    
  def syllabus(academicYearId: String, courseCode: String): ujson.Value = 
    download.json(url.syllabus(academicYearId, courseCode))

end download