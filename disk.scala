package kursomaten

object disk:
  def loadAllYears: Seq[String] = 
    os.list(dir).collect{ case p if p.toIO.isDirectory() => p.last }

  def loadAllCodes: Seq[(String, String, String)] = 
    val xss = for year <- loadAllYears yield
      val programs = os.list(dir / year).collect{ case p if p.toIO.isDirectory() => p.last }
      programs.flatMap: prog =>
        os.list(dir / year / prog).collect: 
          case p if !p.toIO.isDirectory() => 
            (p.last.takeWhile(_ != '-'), year, prog)  
    xss.flatten.distinct

  def saveAllCourses(academicYearId: String, pid: ProgId): Unit =
    val wd = dir / academicYearId / pid.id
  
    for (courseCode, value) <- download.allCoursesOfProgram(academicYearId, pid) do
      Thread.sleep(sleepMillisNotToOverloadServer)
      val id: String = value match
        case ujson.Obj(value) => value.apply("id").toString
        case other => 
          warning(s"Uknown json value: $other")
          "id-unknown"
      
      val metaFile = s"$courseCode-metadata-$id.json"    
      val syllFile = s"$courseCode-syllabus.json"
      
      if os.exists(wd / metaFile) 
      then warning(s"File ${wd/metaFile} already exists, not saving.")
      else 
        println(s"Saving ${wd/metaFile}")
        os.write(wd / metaFile, value.render(escapeUnicode = false), createFolders = true)

      if os.exists(wd / syllFile) then
        warning(s"File ${wd/syllFile} already exists, not saving.")
      else 
        println(s"Saving ${wd/syllFile}")
        val syl = download.syllabus(academicYearId, courseCode = courseCode)
        os.write(wd / syllFile, syl.render(escapeUnicode = false), createFolders = true)

    end for

end disk