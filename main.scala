package kursomaten

import kursomaten.Programme.downloadAll

val currentYear = "25_26"

val usage = s"""
  --help          show this message
  --prog          show all study programmes at LTH
  --crawl         download course data for $currentYear
  --crawl 23 24   download course data for years 23_24, 24_25

  destination dir: $dir
"""

@main def Main(args: String*) =
  println("*** Kursomaten ***")

  args match
    case Seq(cmd, xs*) if cmd == "--prog" =>
      println("Alla programkoder på LTH:")
      println(Programme.allProgIds.mkString(", "))
    
    case Seq(cmd, xs*) if cmd == "--crawl" =>
      val years = if xs.isEmpty then Seq(currentYear) else
        xs.map: x => 
          val yOpt = x.toIntOption
          require(yOpt.isDefined, s"bad year argument: $x")
          s"${yOpt.get}_${yOpt.get + 1}"

      println(s"Laddar ner alla kurskoder för alla program för läsår: ${years}")

      ???
      
      for y <- years do
        val m = Course.downloadCoursesOfAllProgrammesOfYear(y)
        println(m)
        Thread.sleep(2000)
      end for
    
    case args => 
      println(s"unknown args $args\n  type --help for more information")
    
  
  