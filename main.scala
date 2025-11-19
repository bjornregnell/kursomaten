package kursomaten

val usage = s"""
  --help          show this message
  --prog          show all study programmes at LTH
  --crawl         download course data for ${AcademicYear.current}
  --crawl 23 24   download course data for academic years 23_24 and 24_25

  destination dir: $dir
"""

val dir = 
  val target = os.pwd / "target" /"lot"
  os.makeDir.all(target)
  target

val progFile = dir / "programmes.csv"

val sleepMillisNotToOverloadServer = 500

def warning(msg: String): Unit = println(Console.YELLOW_B + msg + Console.RESET)

@main def Main(args: String*) =
  println(s"*** Välkommen till Kursomaten ***\n")
  if args.nonEmpty then println(s"args: $args")
  println(s"innevarande läsår: ${AcademicYear.current}")
  args match
    case Seq(cmd, xs*) if cmd == "--prog" =>
      println("Alla programkoder på LTH:")
      println(Programme.allProgIds.map(_.id).mkString(" "))
    
    case Seq(cmd, xs*) if cmd == "--crawl" =>
      val years = if xs.isEmpty then Seq(AcademicYear.current) else
        xs.map: x => 
          val yOpt = x.toIntOption
          require(yOpt.isDefined && x.length == 2, s"bad year argument: $x")
          val fallYear   = (yOpt.get + 100).toString.drop(1)
          val springYear = (yOpt.get + 101).toString.drop(1)
          s"${fallYear}_${springYear}"

      println(s"Laddar ner alla kurskoder för alla program för läsår: ${years}")

      for 
        y <- years
        pid <- Programme.allProgIds 
      do
        Course.saveAllCourses(y, pid)
      end for
    
    case args => 
      println(s"\nUnknown or missing args ${args.mkString(" ")}\n  type --help for more information")
    
  
  