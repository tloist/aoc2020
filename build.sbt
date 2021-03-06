import Dependencies._
val dottyVersion = "3.0.0-M2"

lazy val day01 = dayProject(1, "Report Repair")
lazy val day02 = dayProject(2, "Password Philosophy")
lazy val day03 = dayProject(3, "Toboggan Trajectory")
lazy val day04 = dayProject(4, "Passport Processing")
lazy val day05 = dayProject(5, "Binary Boarding")
lazy val day06 = dayProject(6, "Custom Customs")
lazy val day07 = dayProject(7, "Handy Haversacks")
  .settings(libraryDependencies += guava)
lazy val day08 = dayProject(8, "Handheld Halting")
lazy val day09 = dayProject(9, "Encoding Error")
lazy val day10 = dayProject(10, "Adapter Array")
lazy val day11 = dayProject(11, "Seating System")
lazy val day12 = dayProject(12, "Rain Risk")
  .settings(libraryDependencies += scalacheck % Test)

lazy val common = project
  .in(file("days/common"))
  .settings(
    name := f"Advent-of-Code 2020: Commons",
    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      betterFiles,
      catsParse,
      junitInterface % Test,
      munit % Test
    )
  )

def dayProject(day: Int, title: String = "") = Project.apply(f"day_$day%02d", file(f"days/$day%02d"))
  .settings(
    name := f"AoC Day $day%2d" + (if (title.nonEmpty) s" - $title" else ""),
    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      junitInterface % Test,
      munit % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(common % "compile->compile;test->test")